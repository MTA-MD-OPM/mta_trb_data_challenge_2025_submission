# discipline_data <- read_parquet(
#   here::here(
#     "ds_early_bus_departure_intervention_study"
#     , "data", "discipline_data.parquet"))
# ---- Graph functions ----
g1.1 <- function(gd1){
  g <- gd1 %>%
    ggplot(aes(x = DateRelative1, y = `Mean early departure` )) +
    geom_line(linewidth = 1, color = MD_colors[1]) + 
    geom_line(data = subset(gd1, Period1 == "Pre-discipline")
              , aes(x = DateRelative1, y = `Mean prediction`), linewidth = 1, color = MD_colors[2]) + 
    geom_line(data = subset(gd1, Period1 == "Post-discipline")
              , aes(x = DateRelative1, y = `Mean prediction`), linewidth = 1, color = MD_colors[2]) +
    theme_classic() +
    geom_vline(xintercept = 0, linewidth = 1, linetype = "dotted") +
    labs(
      subtitle = "Graph 1.1 - Early Rate"
    ) +
    xlab("Day relative to discipline day") +
    scale_y_continuous(labels = scales::percent_format())
  return(g)
}
g1.2 <- function(gd1) {  
  g <- gd1 %>%
    ggplot(aes(x = DateRelative1, y = `Mean adherence seconds` )) +
    geom_line(linewidth = 1, color = MD_colors[1]) + 
    geom_line(data = subset(gd1, Period1 == "Pre-discipline")
              , aes(x = DateRelative1, y = `Mean prediction2`), linewidth = 1, color = MD_colors[2]) +
    geom_line(data = subset(gd1, Period1 == "Post-discipline")
              , aes(x = DateRelative1, y = `Mean prediction2`), linewidth = 1, color = MD_colors[2]) + 
    theme_classic() +
    geom_vline(xintercept = 0, linewidth = 1, linetype = "dotted") +
    labs(
      subtitle = "Graph 1.2 - Adherence"
    ) +
    xlab("Day relative to discipline day") +
    scale_y_continuous(labels = scales::comma_format())
  return(g)
}
g.reg.1.1 <- function(coeff_data, label){
  ggplot(data=coeff_data, aes(x=sort_order, color = `Statistically Significant`)) +
    theme_classic() +
    geom_linerange(aes(ymin=Low, ymax=High)) +
    geom_point(aes(x = sort_order, y = Estimate)) +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
    labs(subtitle = label) +
    xlab("Operator") + ylab("") +
    theme(legend.position = "none"
          , axis.ticks.x = element_blank()
          , axis.text.x = element_blank()) 
  
}
g1.1reg <- function(gd1){
  g <- gd1 %>%
    ggplot(aes(x = DateRelative1, y = `Early Departure Rate` )) +
    geom_line() +
    geom_smooth(data = subset(gd1, Period1 == "Pre-discipline"), method = "gam", se = F
                , linewidth = 2 ) + 
    geom_smooth(data = subset(gd1, Period1 == "Post-discipline"), method = "gam", se = F
                , linewidth = 2 ) +
    geom_vline(xintercept = 0
               , linewidth = 1
               , linetype = "dotted"
               , color = "darkblue") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "none") +
    xlab("") + ylab("") 
  return(g)
}

# ---- Regressions ----
if(FALSE){
discipline_model1.1 <- lm(data = discipline_data
             ,  formula = `Early Departure Rate` ~  
               poly(DateRelative1, degree = 3) +  Period1)
discipline_model1.2 <- lm(data = discipline_data
             ,  formula = `Average Adherence Seconds` ~  
               poly(DateRelative1, degree = 3) +  Period1)
discipline_model2.1 <- lm(data = discipline_data
               ,  formula = `Early Departure Rate` ~  
                 OperatorIdHash 
               + DateRelative1*OperatorIdHash 
               + Period1*OperatorIdHash 
               + Period1*OperatorIdHash*DateRelative1)
discipline_model2.2 <- lm(data = discipline_data
               ,  formula =  `Average Adherence Seconds` ~  
                 OperatorIdHash 
               + DateRelative1*OperatorIdHash 
               + Period1*OperatorIdHash 
               + Period1*OperatorIdHash*DateRelative1)

saveRDS(model1.1, file = here::here(
  "ds_early_bus_departure_intervention_study"
  , "data" ,"discipline_model1.1.Rda")
)
saveRDS(model1.2, file = here::here(
  "ds_early_bus_departure_intervention_study"
  , "data" ,"discipline_model1.2.Rda")
)
saveRDS(model2.1, file = here::here(
  "ds_early_bus_departure_intervention_study"
  , "data" ,"discipline_model2.1.Rda")
)
saveRDS(model2.2, file = here::here(
  "ds_early_bus_departure_intervention_study"
  , "data" ,"discipline_model2.2.Rda")
)
}else{
  discipline_model1.1 <- readRDS("data/discipline_model1.1.Rda")
  discipline_model1.2 <- readRDS("data/discipline_model1.2.Rda")
  discipline_model2.1 <- readRDS("data/discipline_model2.1.Rda")
  discipline_model2.2 <- readRDS("data/discipline_model2.2.Rda")
}

# --- Coeffs ----
coeffs1.1 <- data.frame(summary(discipline_model1.1)$coefficients)
coeffs1.2 <- data.frame(summary(discipline_model1.2)$coefficients)

coeffs2.1 <- data.frame(summary(discipline_model2.1)$coefficients)
coeffs2.1 <- coeffs2.1 %>%
  dplyr::mutate(
    Effect = rownames(coeffs2.1)
    , High = Std..Error * 1.98 + Estimate
    , Low = -Std..Error * 1.98 + Estimate
    , OperatorIdHash =  gsub(":DateRelative1", "", gsub(":Period1Post-discipline", "", gsub("OperatorIdHash", "", rownames(coeffs2.1))))
    , `Statistically Significant` = ifelse(Pr...t.. <= .05,T,F)) %>%
  dplyr::filter(
    grepl(":Period1Post-discipline", Effect)
    ) %>%
  dplyr::arrange(Estimate) %>%
  dplyr::mutate(
    sort_order = row_number()
    , `Outcome Group` = ifelse(Estimate < 0 , "Complier", "Noncomplier")
    )

coeffs2.2 <- data.frame(summary(discipline_model2.2)$coefficients)
coeffs2.2 <- coeffs2.2 %>%
  dplyr::mutate(
    Effect = rownames(coeffs2.2)
    , High = Std..Error * 1.98 + Estimate
    , Low = -Std..Error * 1.98 + Estimate
    , OperatorIdHash = gsub(":DateRelative1", "", gsub(":Period1Post-discipline", "", gsub("OperatorIdHash", "", rownames(coeffs2.2))))
    , `Statistically Significant` = ifelse(Pr...t.. <= .05,T,F)) %>%
  dplyr::filter(
    grepl(":Period1Post-discipline", Effect)
  ) %>%
  dplyr::arrange(Estimate) %>%
  dplyr::mutate(
    sort_order = row_number()
    , `Outcome Group` = ifelse(Estimate > 0 , "Complier", "Noncomplier")
  )

# ---- gd1 ------
gd1 <- discipline_data %>%
  ungroup() %>%
  dplyr::mutate(
    Predicted = predict(discipline_model1.1)
    , Predicted2 = predict(discipline_model1.2)
  ) %>%
  dplyr::group_by(DateRelative1, Period1) %>%
  dplyr::summarise(
    n = n()
    , `Mean early departure` = mean(`Early Departure Rate`)
    , `Mean adherence seconds` = mean(`Average Adherence Seconds`)
    , `Mean prediction` = mean(Predicted)
    , `Mean prediction2` = mean(Predicted2)
  ) %>%
  dplyr::filter(between(as.numeric(DateRelative1), -365, 365)) 

# ---- Avg Duration ----
a <- discipline_data %>%
  ungroup () %>%
  dplyr::mutate(
    Predicted = predict(discipline_model1.1)
    , Predicted2 = predict(discipline_model1.2)
  ) %>%
  dplyr::filter(DateRelative1 == 1) %>%
  dplyr::select(Predicted, Predicted2) %>%
  slice_head(n = 1) 

gd1 %>%
  dplyr::filter(
    `Mean early departure` >= .15 & DateRelative1 > 10
  ) %>%
  dplyr::summarise(
    `Relapse Day` = min(DateRelative1)
  ) %>%
  slice_head(n = 1)


gd1 %>%
  dplyr::filter(
    `Mean early departure` >= .20 & DateRelative1 > 10
  ) %>%
  dplyr::summarise(
    `Relapse Day` = min(DateRelative1)
  ) %>%
  slice_head(n = 1)


# ---- Compliers etc ----

# coeffs2.1 and coeffs2.2 have reversed interpretations
# Compliers 1,4, 5
# Non compliers 160, 165, 184

g1 <- subset(discipline_data,
             OperatorIdHash == coeffs2.2$OperatorIdHash[1]) %>%
  g1.1reg()


g2 <- subset(discipline_data,
             OperatorIdHash == coeffs2.1$OperatorIdHash[165]) %>%
  g1.1reg()
g3 <- subset(discipline_data,
             OperatorIdHash == coeffs2.1$OperatorIdHash[189]) %>%
  g1.1reg()

