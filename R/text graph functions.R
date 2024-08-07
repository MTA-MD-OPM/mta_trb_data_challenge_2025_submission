# ---- Graph Functions ----
g1.1 <- function(gd1){
  g <- gd1 %>%
  ggplot(aes(x = ScheduledDateTime, y = AdherenceSecs)) +
  geom_point(aes(color = TimePointFlag), size = 4) +
  scale_color_manual(values = c("darkblue", MD_colors[2])) +
  geom_line() +
  geom_vline(xintercept = subset(gd1, !is.na(MessageDateTime))$MessageDateTime
             , linewidth = 2
             , linetype = "dashed"
             , color = "darkblue") +
  theme_minimal() +
  # scale_colour_manual(values = MD_colors) +
  theme(legend.position = "none") +
  annotate("rect"
           , xmin = min(gd1$ScheduledDateTime)
           , xmax = max(gd1$ScheduledDateTime)
           , ymin = min(gd1$AdherenceSecs)
           , ymax = -120,
           alpha = .2) +
  labs(subtitle = "Time on X-axis") +
  xlab("Time") + ylab("Adherence Seconds")
  return(g)
}

g1.2 <- function(gd1){
  g <- gd1 %>%
  ggplot(aes(x = RelativeStopSequence, y = AdherenceSecs)) +
  geom_point(aes(color = TimePointFlag), size = 4) + 
  scale_color_manual(values = c("darkblue", MD_colors[2])) +
  geom_line() +
  geom_vline(xintercept = 0
             , linewidth = 2
             , linetype = "dashed"
             , color = "darkblue") +
  theme_minimal() +
 #scale_colour_manual(values = MD_colors) +
  theme(legend.position = "none") +
  annotate("rect"
           , xmin = min(gd1$RelativeStopSequence)
           , xmax = max(gd1$RelativeStopSequence)
           , ymin = min(gd1$AdherenceSecs)
           , ymax = -120,
           alpha = .2) +
  xlim(min(gd1$RelativeStopSequence),max(gd1$RelativeStopSequence)) +
  labs(subtitle = "Relative Stop Sequence on X-axis") +
  xlab("Relative Stop") + ylab("Adherence Seconds")
  return(g)
}

g2.1 <- function(gd2){
  g<- gd2 %>%
  ggplot(aes(x = ScheduledDateTime, y = AdherenceSecs, color = as.factor(RunId))) +
  geom_point() + geom_line() +
  geom_vline(xintercept = subset(gd2, !is.na(MessageDateTime))$MessageDateTime[1]
             , linewidth = 2
             , linetype = "dashed"
             , color = "red") +
  geom_vline(xintercept = subset(gd2, !is.na(MessageDateTime))$MessageDateTime[2]
             , linewidth = 2
             , linetype = "dashed"
             , color = "blue") +
  theme_minimal() +
  annotate("rect"
           , xmin = min(gd2$ScheduledDateTime)
           , xmax = max(gd2$ScheduledDateTime)
           , ymin = min(gd2$AdherenceSecs)
           , ymax = -120,
           alpha = .2) +
  theme(legend.position = "none") +
  labs(subtitle = "Operator B Receives One Text Message During Run") +
  xlab("Time") + ylab("Departure Time Difference (Adherence in Seconds)")
  return(g)
}

g2.2 <- function(gd2){
  g <- gd2 %>%
  ggplot(aes(x = RelativeStopSequence, y = AdherenceSecs, color = RunId)) +
  geom_point() + geom_line() +
  geom_vline(xintercept = 0
             , linewidth = 2
             , linetype = "dashed"
             , color = "red") +
  theme_minimal() +
  annotate("rect"
           , xmin = min(gd2$RelativeStopSequence)
           , xmax = max(gd2$RelativeStopSequence)
           , ymin = min(gd2$AdherenceSecs)
           , ymax = -120,
           alpha = .2) +
  labs(subtitle = "Operator B Receives One Text Message During Run") +
  xlab("Time") + ylab("Departure Time Difference (Adherence in Seconds)")
  return(g)
}

g3.2 <- function(data){
  g <- data %>%
  ggplot(aes(x = TreatmentPeriod, y = `Mean early arrival`, label = Labs)) + 
  geom_bar(stat = "identity", fill = '#FFD520') +
  geom_point(size = 5, color = '#E03A3E') + 
  geom_label_repel(aes(label = Labs),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic() +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(ylim=c(.2,.7)) +
  labs(subtitle = "Early arrival rate")
  return(g)
}

g3.1 <- function(data){
  g<- data %>%
  ggplot(aes(x = TreatmentPeriod, y = `Mean adherence seconds`, label = Labs)) + 
  geom_bar(stat = "identity", fill = '#E03A3E') +
  geom_point(size = 5, color = '#FFD520') + 
  geom_label_repel(aes(label = Labs),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic() +
  coord_cartesian(ylim=c(-150,-80)) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(subtitle = "Average adherence seconds")
  return(g)
}

g4.1 <- function(gd4){
  g <- gd4 %>%
  dplyr::filter(Stat == 'Median adherence') %>%
  ggplot(aes(x = RelativeStopSequence, y = Value, color = TreatmentGroup)) +
  geom_line(size = 2) + 
  scale_color_manual(values = MD_colors) +
  geom_vline(xintercept = 0
             , linewidth = 2
             , linetype = "dashed"
             , color = "blue") +
  annotate("rect"
           , xmin = -50
           , xmax = 50
           , ymin = min(gd4$Value)
           , ymax = -120,
           alpha = .2) +
  coord_cartesian(xlim=c(-40,40)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(subtitle = "Median adherence Seconds") +
  xlab("Stop") + ylab("Adherence time in seconds") +
  scale_y_continuous(labels = scales::comma_format())
  return(g)
}

g4.2 <- function(gd4){
  g <- gd4 %>%
  dplyr::filter(Stat == 'Mean early') %>%
  ggplot(aes(x = RelativeStopSequence, y = Value, color = TreatmentGroup)) +
  geom_line(size = 2) + 
  scale_color_manual(values = MD_colors) +
  geom_vline(xintercept = 0
             , linewidth = 2
             , linetype = "dashed"
             , color = "blue") +
  coord_cartesian(xlim=c(-50,50)) +
  theme_classic() +
  labs(subtitle = "Early departure") +
  xlab("Stop") + ylab("Early departure rate") +
  scale_y_continuous(labels = scales::percent_format())
  return(g)
}

g5.1 <- function(gd5){
  g <- gd5 %>%
    ggplot(aes(x = Date, y = Interventions
               , color = `Intervention Type`, fill = `Intervention Type`)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_color_manual(values = MD_colors) +
    scale_fill_manual(values = MD_colors) +
    theme(legend.position = "bottom") +
    xlab("Week")
  
}

