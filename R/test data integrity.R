#---- Load ----
text_data <- read_parquet(here::here(
                "ds_early_bus_departure_intervention_study"
                , "data"
                , "text_data.parquet")
)
MessageStops <- read_parquet(here::here(
                "ds_early_bus_departure_intervention_study"
                , "data"
                , "MessageStops.parquet")
)
messages <- read_parquet(here::here(
                "ds_early_bus_departure_intervention_study"
                , "data"
                , "messages.parquet")
)


#---- Problems -----
# What messages didn't find a matching stop?
NoMatchMessages <- MessageStops %>%
  dplyr::mutate(Matched = T) %>%
  right_join(messages, relationship = 'many-to-many') %>%
  dplyr::mutate(Matched = !is.na(Matched)) %>%
  dplyr::filter(!Matched) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    OperatorId, MessageDateTime, MESSAGE_DATA
  )

# Save the text_data locally for easy use
write_parquet(NoMatchMessages
              , here::here(
                "ds_early_bus_departure_intervention_study"
                , "data"
                , "NoMatchMessages.parquet")
)


# Anomalies
# OperatorId == 55558 AND ScheduledDate == '2024-01-22': Message 2024-01-22 09:27:07
# Their prior stop was at 09:03:00 and their next stop was 11:47:09

# OperatorId == 61846 and SceduledDate == 03-11 MessageDateTime 2024-03-11 11:38:49
# Their prior stop was at 2024-03-11 09:58:24 and their next stop was 2024-03-11 14:09:02

# OperatorId == 8966 and SceduledDate == 04-09 MessageDateTime 2024-04-09 13:39:43
# OperatorId has no records in adherence for that day
# OperatorId == 8966 and SceduledDate == 04-09 MessageDateTime 2024-04-23
# OperatorId has no records in adherence for that day

# ---- Summary Table ----
summary_stats <- data.frame(
  Statistic = as.character()
  , Value = as.numeric()
)
# total messages
summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Total text messages sent"
    , Value = nrow(messages)
  )
)
# operators texted
summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Operators targeted for texts"
    , Value = length(unique(messages$OperatorId))
  )
)
# matched texts
summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Texts successfully matched to specific stop/trip/operator"
    , Value = length(unique(MessageStops$MessageId))
  )
)
# Treatment count
# matched texts
summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Operators in the treatment group"
    , Value = 
      length(
        unique(
          subset(text_data, TreatmentGroup == "Treatment Group")$OperatorId
        )
      )
  )
)
# control group
summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Operators in the comparison group"
    , Value = 
      length(
        unique(
          subset(text_data, TreatmentGroup == "Comparison Group")$OperatorId
        )
      )
  )
)
# Treatment count
# matched texts
summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Work pieces in the treatment group"
    , Value = 
      length(
        unique(
          subset(text_data, TreatmentGroup == "Treatment Group")$WorkPieceId
        )
      )
  )
)
# control group
summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Work pieces in the comparison group"
    , Value = 
      length(
        unique(
          subset(text_data, TreatmentGroup == "Comparison Group")$WorkPieceId
        )
      )
  )
)



write_parquet(summary_stats
              , here::here(
                "ds_early_bus_departure_intervention_study"
                , "data"
                , "summary_stats.parquet")
)
rm(summary_stats)