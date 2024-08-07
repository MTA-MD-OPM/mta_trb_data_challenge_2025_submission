# 
# text_data <-  read_parquet(
#   here::here(
#   "ds_early_bus_departure_intervention_study"
#   , "data"
#   , "text_data.parquet")
#   )

# ---- Graph functions ----
g1.1reg <- function(gd1){
  
  this_caption <- paste0("Trip ", gd1$TripId, " on ", gd1$ScheduledDate)
  check.integer <- function(N){
    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
  }
  
  
  g <- gd1 %>%
    dplyr::mutate(StopLabel = ifelse(
      check.integer(RelativeStopSequence/20), StopName, NA
      )
    ) %>%
    ggplot(aes(x = RelativeStopSequence, y = AdherenceSecs, label = StopLabel)) +
    geom_point(aes(color = TimePointFlag), size = 4) + 
    # geom_text(check_overlap = TRUE) +
    scale_color_manual(values = c("darkblue", MD_colors[2])) +
    geom_line() +
    geom_vline(xintercept = 0
               , linewidth = 1
               , linetype = "dashed"
               , color = "black") +
    theme_minimal() +
    # scale_colour_manual(values = MD_colors) +
    theme(legend.position = "bottom") +
    annotate("rect"
             , xmin = min(gd1$RelativeStopSequence)
             , xmax = max(gd1$RelativeStopSequence)
             , ymin = min(gd1$AdherenceSecs)
             , ymax = -120,
             alpha = .2) +
    xlab("") + ylab("") +
    labs(caption = this_caption)
  return(g)
}

g.reg.1.1 <- function(coeff_data, label){
  ggplot(data=coeff_data, aes(x=sort_order, color = `Statistically Significant`)) +
    theme_classic() +
    geom_linerange(aes(ymin=Low, ymax=High)) +
    geom_point(aes(x = sort_order, y = Estimate)) +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
    labs(subtitle = label) +
    xlab("Run") + ylab("") +
    theme(legend.position = "none"
          , axis.ticks.x = element_blank()
          , axis.text.x = element_blank()) 
  
}

# ---- Regressions ----
# Model the average texting effect
if(F){
model1.1 <- lm(
  formula = AdherenceSecs ~ RelativeStopSequence + TreatmentPeriod + TreatmentPeriod*RelativeStopSequence
  , data = subset(text_data, TreatmentGroup == "Treatment Group")
)
model1.2 <- lm(
  formula = Early ~ RelativeStopSequence + TreatmentPeriod + TreatmentPeriod*RelativeStopSequence
  , data = subset(text_data, TreatmentGroup == "Treatment Group")
)
model2.1 <- lm(
  formula = AdherenceSecs ~ RelativeStopSequence +
    TreatmentPeriod*WorkPieceId
  + TreatmentPeriod*RelativeStopSequence*WorkPieceId
  , data = subset(text_data, TimePointFlag == T & TreatmentGroup == "Treatment Group")
)
model2.2 <- lm(
  formula = Early ~ RelativeStopSequence +
    TreatmentPeriod*WorkPieceId 
  + TreatmentPeriod*RelativeStopSequence*WorkPieceId
  , data = subset(text_data, TimePointFlag == T & TreatmentGroup == "Treatment Group")
)
saveRDS(model1.1, file = here::here("ds_early_bus_departure_intervention_study"
                                    , "data"
                                    , "model1.1.Rda"
                                    )
)
saveRDS(model1.2, file = here::here("ds_early_bus_departure_intervention_study"
                                 , "data"
                                 , "model1.2.Rda"
)
)
saveRDS(model2.1, file = here::here("ds_early_bus_departure_intervention_study"
                                 , "data"
                                 , "model2.1.Rda"
)
)
saveRDS(model2.2, file = here::here("ds_early_bus_departure_intervention_study"
                                 , "data"
                                 , "model2.2.Rda"
)
)
}else{
  model1.1 <- readRDS(here::here("data"
                  , "model1.1.Rda"
  ))
  model1.2 <- readRDS(here::here("data"
                  , "model1.2.Rda"
  ))
  model2.1 <- readRDS(here::here("data"
                  , "model2.1.Rda"
  ))
  model2.2 <- readRDS(here::here("data"
                  , "model2.2.Rda"
  ))
}

# Main outcome graph data
coeffs1.1 <- data.frame(summary(model1.1)$coefficients)
coeffs1.2 <- data.frame(summary(model1.2)$coefficients)

gd3.1 <-  data.frame(
  TreatmentPeriod = factor(c("Pre-text", "Post-text"), levels = c("Pre-text", "Post-text"))
  , `Mean adherence seconds`  = c(coeffs1.1[1,1], coeffs1.1[3,1] + coeffs1.1[1,1])
  ) %>%
  dplyr::rename(`Mean adherence seconds` = `Mean.adherence.seconds`) %>%
  dplyr::mutate(
    Labs = round(`Mean adherence seconds`)
  )

gd3.2 <-  data.frame(
  TreatmentPeriod = factor(c("Pre-text", "Post-text"), levels = c("Pre-text", "Post-text"))
  , `Mean early arrival`  = c(coeffs1.2[1,1], coeffs1.2[3,1] + coeffs1.2[1,1])
) %>%
  dplyr::rename(`Mean early arrival` = `Mean.early.arrival`) %>%
  dplyr::mutate(
    Labs = paste0(substr(as.character(`Mean early arrival` * 100),0,5), "%")
  )


inline_data <- c(round(gd3.1[2,2]) - round(gd3.1[1,2]))
inline_data <- c(inline_data, c(round((gd3.2[1,2] - gd3.2[2,2])*100)))

# --- Effect heterogenaity ----
coeffs2.1 <- data.frame(summary(model2.1)$coefficients)
coeffs2.1 <- coeffs2.1 %>%
  dplyr::mutate(
    Effect = rownames(coeffs2.1)
    , High = Std..Error * 1.98 + Estimate
    , Low = -Std..Error * 1.98 + Estimate
    , WorkPieceId = tidyr::extract_numeric(Effect)*-1
    , `Statistically Significant` = ifelse(Pr...t.. <= .05,T,F)) %>%
  dplyr::filter(
    grepl("TreatmentPeriodPost-text:WorkPieceId", Effect)
    & !grepl("RelativeStopSequence", Effect)) %>%
  dplyr::arrange(Estimate) %>%
  dplyr::mutate(
    sort_order = row_number()
    , `Outcome Group` = ifelse(Estimate > 0 , "Complier", "Noncomplier"))

coeffs2.2 <- data.frame(summary(model2.2)$coefficients)
coeffs2.2 <- coeffs2.2 %>%
  dplyr::mutate(
    Effect = rownames(coeffs2.2)
    , High = Std..Error * 1.98 + Estimate
    , Low = -Std..Error * 1.98 + Estimate
    , WorkPieceId = tidyr::extract_numeric(Effect)*-1
    , `Statistically Significant` = ifelse(Pr...t.. <= .05,T,F)) %>%
  dplyr::filter(
    grepl("TreatmentPeriodPost-text:WorkPieceId", Effect)
    & !grepl("RelativeStopSequence", Effect)) %>%
  dplyr::arrange(Estimate) %>%
  dplyr::mutate(
    sort_order = row_number()
    , `Outcome Group` = ifelse(Estimate < 0 , "Complier", "Noncomplier"))

# ---- Compliers etc. ----
# 
# non_compliers_examples <- c(1, 4, 5)
# weird_examples <- c(253, 256, 201, 251)
# compliers_examples <-c(250, 285, 286)
# 
# subset(text_data,
#        WorkPieceId == coeffs2.1$WorkPieceId[249]) %>%
#   g1.1reg()





