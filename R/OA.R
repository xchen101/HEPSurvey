library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")

OA1 <- coded_data %>%
  group_by(`p2q2 [OA1]`) %>% #<- change  
  summarize(count = n()) 
OA2 <- coded_data %>%
  group_by(`p2q2 [OA2]`) %>% #<- change  
  summarize(count = n()) 


OA1$QN <- "Find and read papers"
OA2$QN <- "Submit papers"
OA <- bind_rows(OA1, OA2)
OA$bin <- rowSums(OA[, c ("p2q2 [OA1]", "p2q2 [OA2]")], na.rm = T) 
OA$bin <- recode(OA$bin, "1" = "Strongly disagree", "2" = "Somewhat disagree", "3" = "Neutral", "4" = "Somewhat agree", "5" = "Strongly agree")


ggplot(OA[order(OA$bin, decreasing = T),], aes(fill = factor(bin, levels = c("Strongly disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Strongly agree")), y = count, x = QN)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
 # geom_text(aes(label = count, size = 2, position = stack(vjust = 0.5))) +
  scale_fill_manual(values = c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0") )+
  theme(legend.title=element_blank())+
  labs(x = "OA Changed how I...", y = "Frequency")
