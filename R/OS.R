library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")

OS1 <- coded_data %>%
  group_by(`p2q1 [OS1]`) %>% #<- change  
  summarize(count = n()) 
OS2 <- coded_data %>%
  group_by(`p2q1 [OS2]`) %>% #<- change  
  summarize(count = n()) 
OS3 <- coded_data %>%
  group_by(`p2q1 [OS3]`) %>% #<- change  
  summarize(count = n()) 


OS1$QN <- "Discipline"
OS2$QN <- "Researcher"
OS3$QN <- "General public"
OS <- bind_rows(OS1, OS2, OS3)
OS$bin <- rowSums(OS[, c ("p2q1 [OS1]", "p2q1 [OS2]", "p2q1 [OS3]")], na.rm = T) 
OS$bin <- recode(OS$bin, "1" = "Strongly disagree", "2" = "Somewhat disagree", "3" = "Neutral", "4" = "Somewhat agree", "5" = "Strongly agree")


ggplot(OS[order(OS$bin, decreasing = T),], aes(fill = factor(bin, levels = c("Strongly disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Strongly agree")), y = count, x = QN)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  scale_fill_manual(values = c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0") )+
  theme(legend.title=element_blank())+
  labs(x = "Beneficiaries of Open Science", y = "Frequency")
  
  