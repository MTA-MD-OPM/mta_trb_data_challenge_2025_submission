# This file contains the ETL process to build the core analysis data
# for the text intervention analysis
# this requires at least 12 GB RAM to run

# ---- Extract ----
## ---- Messages ----
messages <- dbGetQuery(opm_warehouse_prd, 
  "
SELECT 
  CAST(CH.MESSAGE_TIME AS date) AS MessageDate
  , CAST(CH.MESSAGE_TIME AS time) AS MessageTime
	, O.ONBOARD_LOGON_ID AS OperatorId
	, MESSAGE_DATA
FROM 
    [TMDataMart].[dbo].[COMM_HISTORY] CH
    LEFT JOIN [TMDataMart].[dbo].[COMM_VEHICLE_SNAPSHOT] CVS 
        ON CONVERT(VARCHAR, CH.MESSAGE_TIME, 120) = CONVERT(VARCHAR, CVS.SNAPSHOT_TIME, 120)
    INNER JOIN [TMDataMart].[dbo].[OPERATOR] O 
        ON O.OPERATOR_ID = CVS.DRIVER_ID
WHERE 
    CH.DISPATCHER_ID = 222
    AND CH.MESSAGE_TIME BETWEEN '2024-01-01' AND '2024-04-30'
    AND CH.MESSAGE_DATA LIKE '%ahead%'
  ") %>%
  # Make OperatorId class match the class in adherence
  dplyr::mutate(
    OperatorId = as.character(OperatorId)
    , MessageDateTime = as.POSIXct(
      paste0(MessageDate, " ", substr(MessageTime, 0, 8)))
  )

# Duplicate messages
dup_messages <- messages[duplicated(messages),]
write_parquet(dup_messages, here::here("ds_early_bus_departure_intervention_study", "data", "dup_messages.parquet"))

messages <- messages %>% #Messages are double-counted sometimes in the data
  # OperatorIds (7274 , 90568 ) received two messages at the same time (2024-04-17 15:04:46, 2024-04-22 10:42:33)
  dplyr::distinct(MessageDateTime, MessageDate, OperatorId, MESSAGE_DATA) 

write_parquet(messages, here::here("ds_early_bus_departure_intervention_study", "data", "messages.parquet"))
rm(dup_messages)

## ---- Adherence ----
# MTA's adherence table shows every stop a bus made and whether it made that 
# stop on time or not
adherence <- dbGetQuery(opm_warehouse_prd, paste0("
SELECT
  trip_id AS TripId
  , scheduled_date AS ScheduledDate
  , scheduled_time AS ScheduledTime
  , vehicle_id AS VehicleId
  , driver_payrollNumber AS OperatorId
  ,	lsStopSequence AS StopSequence
  , stop_id AS StopId
  , actual_time AS DepartureTime
  , sched_adherence_secs AS AdherenceSecs	
  , stop_name AS StopName
  , early_flag AS Early
  , IsNode AS TimePointFlag
FROM
[OPM_Datawarehouse].[dbo].[SwiftlyJoinedWithTrapeze]
WHERE actual_date >= '2024-01-01' And actual_date <= '2024-05-01'
")) 

# This gap because of RAM limitations
adherence$DepartureDateTime <- paste0(adherence$ScheduledDate, " ", substr(adherence$DepartureTime, 0, 8))
adherence$DepartureDateTime <- as.POSIXct(adherence$DepartureDateTime)
adherence$ScheduledDateTime <- paste0(adherence$ScheduledDate, " ", substr(adherence$ScheduledTime, 0, 8))
adherence$ScheduledDateTime <- as.POSIXct(adherence$ScheduledDateTime)

#---- Transform----

#### ===== Work Piece =====
# A "Work" is analogous to a "shift" for an operator
# A " work piece " is part of a shift.  
# in the data, a work piece is a sequence of stops within a uniquetrip + OperatorId combination
# A work piece is unique within OperatorId and TripId, but not within days
# since one one may cross the midnight threshold
# Since work piece is not identified explicitly in the data, we need to generate it
adherence <- adherence %>%
  dplyr::group_by(TripId, OperatorId) %>%
  dplyr::arrange(TripId, OperatorId, DepartureDateTime) %>%
  # Each work piece has a unique starting scheduled time and date within OperatorId and TripId
  # Assign the first stop this unique time and date
  dplyr::mutate(
    # First, make sure the first stop is sequenced at 1
    StopSequence = case_when(
      # If the sequence number before is larger, then that was the end of the last sequence
      dplyr::lag(StopSequence, 1L) > StopSequence | 
        # If the last one doesn't exist, this is the very first stop
        is.na(dplyr::lag(StopSequence, 1L)) ~ 1
      , T ~ StopSequence
    )
    # Then capture the date and time the work piece started
   , WorkPieceStartDateTime = ifelse(StopSequence == 1, ScheduledDateTime, NA)
  ) %>%
  # fill in the NA values within each work piece using the start date and time
  tidyr::fill(WorkPieceStartDateTime) 

adherence <- adherence %>%
  dplyr::ungroup() %>%
  dplyr::distinct(TripId, OperatorId, WorkPieceStartDateTime) %>%
  # Generate a numeric ID for each work piece
  dplyr::mutate(
    WorkPieceId= dplyr::row_number()
  ) %>%
  inner_join(adherence) %>%
  dplyr::mutate(
    WorkPieceId= factor(WorkPieceId)
    , TimePointFlag = (TimePointFlag == 1)
    ) 

TotalRuns <- length(unique(adherence$WorkPieceId))

# Test
nrow(distinct(ungroup(adherence), TripId, OperatorId, WorkPieceId)) ==
nrow(distinct(ungroup(adherence), WorkPieceId)) # work piece ID should be unique within Trip and Operator

adherence <- adherence %>%
  dplyr::select(-WorkPieceStartDateTime) # no longer useful

### ---- Early Work Pieces ----
# Now we want to know which _drivers_ ran early on which work pieces
# This will include both the drivers that received a text message, and drivers that 
# did not.
# We have no standard for an early work piece, so I make one

OperatorIds <- adherence %>%
  group_by(OperatorId, TripId, WorkPieceId) %>%
  dplyr::summarise(
    Early = mean(Early)
    , Stops = n()
    , Secs = mean(AdherenceSecs)
  ) %>%
  # Criteria from 'early' work piece:
    # More than 40 stops because shorter than that is... weird
    # Departs more than 40% of stops early
    # On average, departs stops 40 seconds early. 
  dplyr::filter(Early >= .4 & Stops > 40 & Secs <= -40) %>%
  distinct(OperatorId, TripId, WorkPieceId)

adherence <- subset( #subset less memory intensive and faster
  adherence 
  , (OperatorId %in% OperatorIds$OperatorId
      & WorkPieceId%in% OperatorIds$WorkPieceId)
  | OperatorId %in% messages$OperatorId #maybe some text recipients don't meet my criteria. Keep anyway.
)

TotalEarlyRuns <- length(unique(adherence$WorkPieceId))  

#---- Match stops messages -----
# Identify the stops where messages were received by assigning messages to stops
MessageStops <-  messages %>%
  dplyr::mutate(MessageId = row_number()) %>% # Give each message a unique ID
  # Join each message into adherence by Operator and Date
   inner_join(adherence
                  , by = c('MessageDate' = 'ScheduledDate' 
                           , 'OperatorId' = 'OperatorId')
                  , relationship = "many-to-many") %>% #many-to-many is ok for now
    # Identify the stop the message was received at. 
    # Message was received at a stop if the message was received 
    # before reaching the stop
    dplyr::group_by(TripId, OperatorId, WorkPieceId, MessageDate) %>%
    dplyr::mutate(
      MessageArrivalDiffTime = 
        abs(difftime(DepartureDateTime, MessageDateTime, units = "mins"))
      , NextStopName = dplyr::lead(StopName, n = 1L, order_by = DepartureDateTime)
      , NextDepartureTime = dplyr::lead(DepartureDateTime, n = 1L, order_by = DepartureDateTime) 
    ) %>%
     dplyr::select(TripId, OperatorId,  WorkPieceId
                , MessageDate, MessageId, MessageDateTime
                , StopId, StopSequence, StopName 
                , DepartureDateTime,NextStopName, NextDepartureTime
                , MessageArrivalDiffTime, AdherenceSecs, Early) %>% # most of these fields are for diagnostic purposes
  dplyr::arrange(TripId, OperatorId, DepartureDateTime) %>%
    dplyr::filter( # Attribute the message to the stop that precedes the message
      difftime(MessageDateTime, DepartureDateTime) >= 0 & # They are at or have left the last stop
        difftime(MessageDateTime, NextDepartureTime) < 0 # But have not departed the next stop
    ) # Messages at the very end of a work piece will be excluded

# ---- Final data ----
# identify the specific stop operators received the message at
text_data <- MessageStops %>%
  ungroup() %>%
  dplyr::select(
    # These fields identify a specific stop
    TripId, OperatorId, WorkPieceId,  DepartureDateTime
    # These fields indicate a message was received
    , MessageId, MessageDateTime) %>%
  right_join(adherence, relationship = "one-to-one") %>%
  # Need to filter AGAIN because some work pieces are included because Operator got a text on a different work piece
  dplyr::filter(
    WorkPieceId%in% OperatorIds$WorkPieceId
    | WorkPieceId%in% MessageStops$WorkPieceId
  )

# Test
text_data %>% dplyr::filter(!is.na(MessageId)) %>%
  distinct(OperatorId) %>% 
  summarise(n = n()) == length(unique(MessageStops$OperatorId))

nrow(filter(text_data, WorkPieceId== 1060)) == 0
nrow(distinct(filter(text_data, !(WorkPieceId%in% MessageStops$WorkPieceId)), WorkPieceId)) 

## ==== Contextual vars ====
### ==== Total messages received ====
text_data <- text_data %>%
# Group operators by the number of messages they received overall in the data 
  group_by(OperatorId) %>%
  dplyr::filter(!is.na(MessageId)) %>%
  dplyr::summarise(TotalMessages = n()) %>%
  right_join(text_data, relationship = "one-to-many") %>%
  dplyr::mutate(
    TotalMessages = ifelse(is.na(TotalMessages), 0, TotalMessages)
  )

# Test
text_data %>% dplyr::group_by(TotalMessages) %>%
  distinct(OperatorId, TotalMessages) %>% 
  summarise(n = n())

### ==== Continuous Stop Sequence ====
text_data <- text_data %>%
# Re-sequence StopSequence so that it is continuous and complete
  dplyr::group_by(TripId, OperatorId, WorkPieceId) %>%
  dplyr::arrange(TripId, OperatorId, WorkPieceId, DepartureDateTime) %>%
  dplyr::mutate(
    StopSequence = row_number()
  ) 

###==== Treatment & Control Groups ====
text_data <- text_data %>%
# Create a dummy for the treatment and control groups
    ungroup() %>%
  # Since this is not an RCT, I will not use the term 'control'
  dplyr::mutate(
    TreatmentGroup = 
      factor(
        case_when(
          (WorkPieceId%in% MessageStops$WorkPieceId) ~ "Treatment Group"
        ,  !(WorkPieceId%in% MessageStops$WorkPieceId) ~ "Comparison Group"
        , T ~ "Unknown"
        )
        , levels = c("Comparison Group", "Treatment Group", "Unknown")
      )
  )

# Test
text_data %>% dplyr::group_by(TreatmentGroup) %>%
  distinct(OperatorId, TreatmentGroup) %>% 
  summarise(n = n())
text_data %>% dplyr::group_by(TreatmentGroup) %>%
  distinct(WorkPieceId, TreatmentGroup) %>% 
  summarise(n = n())

### ===== RelativeStopOrder ====
# For the treatment group, generate a stop sequence relative to the message stop  
text_data <- text_data %>%
    dplyr::filter(
      # Identify which stop in the sequence received the message
      !is.na(MessageId) & TreatmentGroup == "Treatment Group"
    ) %>%
    # Capture the StopSequence for the stop that received the message
    dplyr::rename(RelativeStopSequence = StopSequence) %>%
    # Join back in so you have a column to difference against
    dplyr::select(WorkPieceId, RelativeStopSequence) %>%
    right_join(text_data, relationship = "one-to-many") %>%
    dplyr::mutate(
      RelativeStopSequence = StopSequence - RelativeStopSequence # Positive means stop is after message
    ) 

# Test
text_data %>% dplyr::group_by(TreatmentGroup) %>%
  distinct(OperatorId, TreatmentGroup) %>% 
  summarise(n = n())

# For the control group, generate a stop sequence relative to the middle stop
text_data <- text_data %>%
  dplyr::filter(TreatmentGroup ==  "Comparison Group") %>%
  dplyr::group_by(OperatorId, TripId, WorkPieceId) %>%
  dplyr::summarise(
    LastStop = max(StopSequence, na.rm = T)
  ) %>%
  dplyr::mutate(
    MiddleStop = floor(LastStop/2)
  ) %>%
  dplyr::select(OperatorId, TripId, WorkPieceId, MiddleStop) %>%
  right_join(text_data, relationship = "one-to-many") %>%
  dplyr::mutate(
    RelativeStopSequence = case_when(
      (TreatmentGroup ==  "Comparison Group") ~ StopSequence - MiddleStop
      , T ~ RelativeStopSequence
    )
  )

### ==== Pre- & Post- dummy ====
# Include a dummy for treatment period & treatment group
text_data <- text_data %>%
  dplyr::mutate(
    TreatmentPeriod = 
      factor(
        ifelse(RelativeStopSequence <= 0 & TreatmentGroup == "Treatment Group", "Pre-text", "Post-text")
        , levels = c("Pre-text", "Post-text")
      )
  ) 

# Test
text_data %>% dplyr::group_by(TreatmentGroup, TreatmentPeriod) %>%
  distinct(OperatorId, TreatmentGroup) %>% 
  summarise(n = n())


### ==== Mask OperatorId ====
# We don't need to use actual OperatorIds. We can mask these.
mask <- data.frame(
  OperatorId = unique(text_data$OperatorId)
) %>%
  dplyr::mutate(
    OperatorIdHash = anonymizer::anonymize(OperatorId, .algo = "sha256", .seed = text_data_seed)
  )

text_data <- text_data %>%
  inner_join(mask) %>%
  group_by(TripId, WorkPieceId, OperatorIdHash) %>%
  dplyr::select(-OperatorId) 

# ---- Load ----

# Save
write_parquet(text_data
              , here::here(
                "ds_early_bus_departure_intervention_study"
                , "data"
                , "text_data.parquet")
              )
write_parquet(MessageStops
              , here::here(
                "ds_early_bus_departure_intervention_study"
                , "data"
                , "MessageStops.parquet")
)
write_parquet(messages
              , here::here(
                "ds_early_bus_departure_intervention_study"
                , "data"
                , "messages.parquet")
)


## ==== Dictionaries ====
text_data_Fields <- data.frame(
    Field = names(text_data)
  , Table = rep("text_data")
  , Description = c(
      "TripId identifies the trip"
    , "Work piece is part of a work, which intersects with trip"
    , "This identifies the middle stop for each workpiece"
    , "Stop sequence calculated relative to the stop where the operator received the text"
    , "Total messages received by this operator over study period"
    , "Data and time of stop departure"
    , "Sequential message ID to identify messages"
    , "Date and time message was sent"
    , "Date the bus was scheduled to depart the stop"
    , "Time the bus was scheduled to depart the stop on ScheduleDate"
    , "Unique ID for the bus vehicle"
    , "Sequence of stop on trip"
    , "Unique ID for bus stop"
    , "Clock time bus departed on stop on ScheduleDate"
    , "Difference between bus departure and scheduled departure in seconds"
    , "Name of the bus stop"
    , "Flag for early departure from a bus stop. Early defined by MTA."
    , "Flag for whether the stop is a time point or not"
    , "Date and time the bus was supposed to depart the stop"
    , "Group work pieces into treated or comparison work pieces"
    , "Group stops into before and after stops"
    , "OperatorIdHash uniqly and anonymously identifies individual bus operators"
  )
)


write.csv(
  text_data_Fields
  , file = here::here("ds_early_bus_departure_intervention_study", "data", "text_data_Fields.csv")
  , row.names = FALSE)

text_data_Table <- data.frame(
  Table = "Non. Saved in repository"
  , Description = "Early bus work pieces"
  , RowDescription = "Each row is a bus stop at a time on a day."
)
write.csv(
  text_data_Table
  , file = here::here("ds_early_bus_departure_intervention_study", "data", "text_data_Table.csv")
  , row.names = FALSE)

## ====Close ====
# Remove data frames
rm(adherence, messages, MessageStops, OperatorIds, test, text_data, text_data_Fields
   , text_data_Table, mask)


