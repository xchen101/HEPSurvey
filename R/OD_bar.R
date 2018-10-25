# Reference
# http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/ 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")

OD1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p3q2E [OD_E1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(OD1, aes(x = `p3q2E [OD_E1]`, y = freq, fill = D3)) + # <- change
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Data used in published \nstudies should be made open") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

OD2 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p3q2E [OD_E2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(OD2, aes(x = `p3q2E [OD_E2]`, y = freq, fill = D3)) + # <- change
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Access to data should be\nprovided to anyone who's interested") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

OD3 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p3q2E [OD_E3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(OD3, aes(x = `p3q2E [OD_E3]`, y = freq, fill = D3)) + # <- change
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Access to data from past\nstudies is helpful to me") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

OD4 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p3q2E [OD_E4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p4 <- ggplot(OD4, aes(x = `p3q2E [OD_E4]`, y = freq, fill = D3)) + # <- change
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Whether data will be open \ndoes not affect how I work") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

grid.arrange(
  p1,
  p2,
  p3,
  p4,
  nrow = 2
)
