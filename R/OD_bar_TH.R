library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")

OD1 <- coded_data %>%
  filter(D4 == "TH") %>%
  group_by(`p3q2E [OD_E1]`) %>% #<- change  
  summarize(count = n()) 
OD2 <- coded_data %>%
  filter(D4 == "TH") %>%
  group_by(`p3q2E [OD_E2]`) %>% #<- change  
  summarize(count = n()) 
OD3 <- coded_data %>%
  filter(D4 == "TH") %>%
  group_by(`p3q2E [OD_E3]`) %>% #<- change  
  summarize(count = n()) 
OD4 <- coded_data %>%
  filter(D4 == "TH") %>%
  group_by(`p3q2E [OD_E4]`) %>% #<- change  
  summarize(count = n()) 


OD1$QN <- "Data used in published\nstudies should be \nmade open"
OD2$QN <- "Access to data should\nbe provided to anyone\nwho's interested"
OD3$QN <- "Open and easy access to\ndata from past studies\nis helpful to me"
OD4$QN <- "Whether data from my\nstudy will be open does\nnot affect how I work"

OD <- bind_rows(OD1, OD2, OD3, OD4)
OD$bin <- rowSums(OD[, c ("p3q2E [OD_E1]", "p3q2E [OD_E2]", "p3q2E [OD_E3]", "p3q2E [OD_E4]")], na.rm = T) 
OD$bin <- recode(OD$bin, "1" = "Strongly disagree", "2" = "Somewhat disagree", "3" = "Neutral", "4" = "Somewhat agree", "5" = "Strongly agree")


ggplot(OD[order(OD$bin, decreasing = T),], aes(fill = factor(bin, levels = c("Strongly disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Strongly agree")), y = count, x = QN)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  scale_fill_manual(values = c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0") )+
  theme(legend.title=element_blank())+
  labs(x = " ", y = "Frequency")








OD5 <- coded_data %>%
  filter(D4 == "TH") %>%
  group_by(`p3q2E [OD_E3]`) %>% #<- change  
  summarize(count = n()) 
OD6 <- coded_data %>%
  filter(D4 == "TH") %>%
  group_by(`p3q2T [OD_E5]`) %>% #<- change  
  summarize(count = n()) 

OD5$QN <- "Open experimental data\nis helpful to me"
OD6$QN <- "Open theory data is\nhelpful to me"

OD <- bind_rows(OD5, OD6)
OD$bin <- rowSums(OD[, c ("p3q2E [OD_E3]", "p3q2T [OD_E5]")], na.rm = T) 
OD$bin <- recode(OD$bin, "1" = "Strongly disagree", "2" = "Somewhat disagree", "3" = "Neutral", "4" = "Somewhat agree", "5" = "Strongly agree")


ggplot(OD[order(OD$bin, decreasing = T),], aes(fill = factor(bin, levels = c("Strongly disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Strongly agree")), y = count, x = QN)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  scale_fill_manual(values = c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0") )+
  theme(legend.title=element_blank())+
  labs(x = " ", y = "Frequency")
