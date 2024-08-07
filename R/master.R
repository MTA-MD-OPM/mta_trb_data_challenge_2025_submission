# This file outlines the steps to produce the early intervention analysis
# There are two analyses:
# - Text intervention
# - Discipline intervention

library(here)
# ---- Setup ----
source(here::here("ds_early_bus_departure_intervention_study", "R", "libraries.R"))

# ---- Ingest ----
# Ingest file inputs data from the Warehouse and outputs a .parquet file
# Takes a long time to run
# Needs at least 12 GB RAM
# Also, you need MTA credentials to run the ingest. If you don't have those 
# credentials, the rest of the scripts will still run

# Do you have database access?
test <- try(source(here::here("creds.R")))
if(!class(test) == "try-error"){ # If so, run ingest
  source(here::here("creds.R"))
  source(here::here("ds_early_bus_departure_intervention_study", "R", "text ingest.R"))
  source(here::here("ds_early_bus_departure_intervention_study", "R", "text data integrity.R"))
} # if no database access, continue to next steps

# ---- Text intervention ----
## ==== Analysis ====
# Skip ingest if you've run it once and haven't changed it
# Produce graph data ("gd") and graphing functions
source(here::here("ds_early_bus_departure_intervention_study", "R","text graph data.R"))
# Graph functions
source(here::here("ds_early_bus_departure_intervention_study", "R","text graph functions.R"))
# Regression analysis
source(here::here("ds_early_bus_departure_intervention_study", "R","text regression analysis.R"))

# Render the markdown memo. Markdown file does not call ingest.
rmarkdown::render(here::here("ds_early_bus_departure_intervention_study", "text intervention analysis.Rmd"))

# ---- Discipline intervention ----
test <- try(source(here::here("creds.R")))
if(!class(test) == "try-error"){ # If so, run ingest
source(here::here("ds_early_bus_departure_intervention_study", "R","discipline ingest.R"))
}

## ==== Analysis ====
source(here::here("ds_early_bus_departure_intervention_study", "R","discipline analysis.R"))
# Render the markdown memo
rmarkdown::render(here::here("ds_early_bus_departure_intervention_study", "text intervention analysis.Rmd"))


# ---- Close ----
# Close SQL connections
dbDisconnect(opm_warehouse_prd); rm(opm_warehouse_prd)
dbDisconnect(opm_warehouse_dev); rm(opm_warehouse_dev)
rm(apollo_ds)
