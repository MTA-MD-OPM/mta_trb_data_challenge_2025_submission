# This file contains the ETL process to build the core analysis data
# for the text intervention analysis
# this requires at least 12 GB RAM to run

# ----- Extract ----
discipline <- dbGetQuery(opm_warehouse_prd, "
WITH
disciplines AS (
	SELECT DISTINCT  driverID AS DriverId, CAST(incidentDate AS DATE) AS IncidentDate, 'TRUE' AS Discipline
	FROM [OPM_Datawarehouse].[dbo].[Trackit_WriteUps_CSV]
	WHERE evidenceOfViolation Like '%Ahead%')
, adherence AS (
SELECT CAST(actual_date AS DATE) AS Date, driver_payrollNumber AS OperatorId 
	, AVG(CAST(early_flag AS NUMERIC)) AS [Early Departure Rate], AVG(sched_adherence_secs) AS [Average Adherence Seconds]
	FROM [OPM_Datawarehouse].[dbo].[SwiftlyJoinedWithTrapeze]
	WHERE scheduled_date BETWEEN '2023-01-01' AND '2024-04-30' 
	AND IsNode = 1 /*Include only time-point stops */
	-- Limit adherence to only the drivers who received a discipline
  	AND driver_payrollNumber IN (
  		SELECT DISTINCT  driverID FROM [OPM_Datawarehouse].[dbo].[Trackit_WriteUps_CSV]
  		WHERE evidenceOfViolation Like '%Ahead%')
  	GROUP BY actual_date, driver_payrollNumber
    )
SELECT adherence.*
, CASE WHEN Discipline = 'TRUE' THEN Discipline ELSE 'FALSE' END AS Discipline 
, IncidentDate
FROM adherence LEFT JOIN disciplines
	ON adherence.OperatorId = disciplines.DriverId 
	AND adherence.Date = disciplines.IncidentDate
ORDER BY adherence.OperatorId, adherence.Date
")

# ----- Transform -----
## ---- Days ----
# Here I just want to identify what day they received a discipline, and then
# Calculate a day column relative to discipline days

# Need to create columns for each discipline's date
a <- discipline %>%
  distinct(OperatorId, IncidentDate) %>%
  dplyr::group_by(OperatorId) %>%
  dplyr::arrange(OperatorId, IncidentDate) %>%
  # Order disciplines
  dplyr::mutate(DisciplineNumber = row_number()) %>%
  tidyr::pivot_wider(id_cols = OperatorId, names_from = DisciplineNumber, values_from = IncidentDate, names_prefix = "DisciplineNum")

# Categorize employees based on the number of disciplines overall they received
b <- discipline %>%
  dplyr::group_by(OperatorId) %>%
  dplyr::summarise(
    TotalDiscplines = sum(as.logical(Discipline))
  ) %>%
  dplyr::filter(TotalDiscplines > 0)
  
# join together
# Calculate date relative to discipline date
discipline_data <- discipline %>%
  dplyr::select( -IncidentDate) %>%
  inner_join(a, relationship = "many-to-one") %>%
  inner_join(b, relationship = "many-to-one") %>%
  dplyr::mutate(
    Date = as.Date(Date)
    , dplyr::across(DisciplineNum1:DisciplineNum4, ~  as.Date(.x))
    , DateRelative1 = round(as.numeric(Date - DisciplineNum1))
    , DateRelative2 = round(as.numeric(Date - DisciplineNum2))
    , DateRelative3 = round(as.numeric(Date - DisciplineNum3))
    , DateRelative4 = round(as.numeric(Date - DisciplineNum4))
  ) 

## ==== pre- post dummy ====
discipline_data <- discipline_data %>%
  dplyr::mutate(
    Period1 = factor(
      ifelse(DateRelative1 < 0, "Pre-discipline", "Post-discipline")
      , levels = c("Pre-discipline", "Post-discipline")
    )
    , DisciplineInd = as.logical(Discipline) # Encode 
  )

## ==== Mask OperatorId ====
# We don't need to use actual OperatorIds. We can mask these.
mask <- data.frame(
  OperatorId = unique(discipline_data$OperatorId)
) %>%
  dplyr::mutate(
    OperatorIdHash = anonymizer::anonymize(OperatorId, .algo = "sha256", .seed = text_data_seed)
  )

discipline_data <- discipline_data %>%
  inner_join(mask) %>%
  group_by(OperatorIdHash) %>%
  dplyr::select(-OperatorId) 

# ---- Load ----
write_parquet(discipline_data
              , here::here("ds_early_bus_departure_intervention_study"
                           , "data"
                           , "discipline_data.parquet")
              )

rm(a, b, discipline, mask)

## ---- Dictionary ----
discipline_data_Fields <- data.frame(
  Field = names(discipline_data)
  , Table = rep("discipline_data")
  , Description = c(
    "Date"
    , "Average daily early departure rate"
    , "Average daily adherence seconds"
    , "Date of first discipline for running ahead of schedule"
    , "Date of second discipline for running ahead of schedule"
    , "Date of third discipline for running ahead of schedule"
    , "Date of fourth discipline for running ahead of schedule"
    , "Date of fifth discipline for running ahead of schedule"
    , "Total disciplines received for running ahead of schedule for operator"
    , "Day relative to date of first discipline for running ahead of schedule"
    , "Day relative to date of second discipline for running ahead of schedule"
    , "Day relative to date of third discipline for running ahead of schedule"
    , "Day relative to date of fourth discipline for running ahead of schedule"
    , "Day relative to date of fifth discipline for running ahead of schedule"
    , "Day category based on first discipline"
    , "Indicator for whether operator received a discipline on this day"
    , "OperatorIdHash uniqly and anonymously identifies individual bus operators"
  )
)


write.csv(
  text_data_Fields
  , file = here::here("ds_early_bus_departure_intervention_study", "data", "discipline_data_Fields.csv")
  , row.names = FALSE)

discipline_data_Table <- data.frame(
  Table = "Non. Saved in repository"
  , Description = "Early bus work pieces"
  , RowDescription = "Each row is a bus stop at a time on a day."
)
write.csv(
  discipline_data_Table
  , file = here::here("ds_early_bus_departure_intervention_study", "data", "discipline_data_Table.csv")
  , row.names = FALSE)

## ====Close ====
# Remove data frames
rm(discipline_data)



