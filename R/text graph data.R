# 
# # Dev testing
# text_data <-  read_parquet(
#   here::here(
#   "ds_early_bus_departure_intervention_study"
#   , "data"
#   , "text_data.parquet")
#   )

#
# discipline_data <-  read_parquet(
#   here::here(
#   "ds_early_bus_departure_intervention_study"
#   , "data"
#   , "discipline_data.parquet")
#   )

#

# ---- Graph Data ----
gd1 <- text_data %>%
  dplyr::filter(OperatorIdHash == "c568c7000cdc3fe9e2e94f7c7054c860cf4cdeaecb768b108fcbb02236be6417"
                & TreatmentGroup == "Treatment Group")

gd2 <- subset(text_data,
               TreatmentGroup == "Comparison Group"
               &  WorkPieceId == "18589" 
              &  OperatorIdHash== "efc11f34f3397fcee564ce2f747de1d69176a6c28e2e474e490fd99f50cc0aea" 
  ) 
# Dev testing
# g1.1(gd1)
# g1.2(gd1)

# See text 'regression analysis.R' for gd3 construction 

gd4 <- text_data %>%
  dplyr::group_by(RelativeStopSequence, TreatmentGroup) %>%
  dplyr::summarise(
    Stops = n()
    , `Mean early` = mean(Early, na.rm = T)
    , `Median adherence` = median(AdherenceSecs)
  ) %>%
  tidyr::pivot_longer(`Mean early` :`Median adherence`
                      , names_to = "Stat", values_to = "Value"
  ) 

if(FALSE){

gd5 <- text_data %>%
  dplyr::ungroup() %>%
  distinct(MessageDateTime) %>%
  dplyr::mutate(
    Week = lubridate::week(MessageDateTime)
    , Date = as.Date(MessageDateTime)
  ) %>%
  dplyr::group_by(Week) %>%
  dplyr::summarise(
    Interventions = n()
    , Date = max(Date)
  ) %>%
  dplyr::mutate(
    `Intervention Type` = "Live Texts"
  )

a <- discipline %>%
  dplyr::filter(Discipline == "TRUE") %>%
  dplyr::select(Date) %>%
  dplyr::mutate(
    Week = lubridate::week(Date)
  ) %>%
  dplyr::group_by(Week) %>%
  dplyr::summarise(
    Interventions = n()
    , Date = max(Date)
  ) %>%
  dplyr::mutate(
    `Intervention Type` = "Discipline"
  )

gd5 <- rbind(gd5, a); rm(a)
}

# Helta's ask
# Depart the next timepoint with an average (or whatever term/calculation) adherence time
# of ___
# Depart the next Timepoint early __% of the time
# Achieve an early % of ___ over the course of the rest of the trip/day
text_data %>%
  dplyr::filter(
    TreatmentPeriod == "Post-text" 
    & TimePointFlag
    & TreatmentGroup == "Treatment Group"
  ) %>%
  dplyr::group_by(WorkPieceId) %>%
  dplyr::slice_head(n=1) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(
    `Early Rate` = mean(Early)
    , `Adherence Seconds` = mean(AdherenceSecs)
  )

text_data %>%
  dplyr::filter(
    TreatmentPeriod == "Post-text" 
    & TimePointFlag
    & TreatmentGroup == "Treatment Group"
  ) %>%
  dplyr::group_by(WorkPieceId) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(
    `Early Rate` = mean(Early)
    , `Adherence Seconds` = mean(AdherenceSecs)
  )


