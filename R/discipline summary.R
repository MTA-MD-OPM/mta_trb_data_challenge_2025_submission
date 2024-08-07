
# ---- Summary Data ----

discipline_data <- read_parquet(
  here::here("ds_early_bus_departure_intervention_study"
             , "data"
             , "discipline_data.parquet")
)

summary_stats <- data.frame(
  Statistic = as.character()
  , Value = as.numeric()
)

# N disciplines
value <- discipline_data %>%
  ungroup() %>%
  summarise(
    Value = sum(as.logical(Discipline))
  )

summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Total number of disciplines issued."
    , Value = value[1,1]
  )
)

# N operators
a <- discipline_data %>%
  dplyr::filter(DisciplineInd) %>%
  distinct(OperatorIdHash)

summary_stats <- rbind(
  summary_stats
  , data.frame( 
    Statistic = "Total number of Operators Receiving a discipline."
    , Value = nrow(a)
  )
)  

# Operators by disciplines
a <- 
  discipline_data %>%
  dplyr::filter(DisciplineInd) %>%
  dplyr::group_by(TotalDiscplines) %>%
  dplyr::distinct(OperatorIdHash, TotalDiscplines) %>%
  summarise(
    Value = n()
  ) %>%
  dplyr::mutate(
    Statistic = paste0("Operators that received ", TotalDiscplines, " disciplines.")
  ) %>%
  dplyr::select(-TotalDiscplines) %>%
  dplyr::select(Statistic, Value)


summary_stats <- rbind(summary_stats, a)

write_parquet(summary_stats
              , here::here("ds_early_bus_departure_intervention_study"
                           , "data"
                           , "discipline_summary_stats.parquet")
)

rm(summary_stats, a, value)