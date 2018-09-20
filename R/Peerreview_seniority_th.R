library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")

# PRTT1 by seniority and gender
PRTT1 <- coded_data %>% #<- change
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(D3, `p7q1T [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(PRTT1, aes(x = `p7q1T [1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Data") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

# PRTT2 by seniority and gender
PRTT2 <- coded_data %>% #<- change
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(D3, `p7q1T [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(PRTT2, aes(x = `p7q1T [2]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Code") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

# PRTT3 by seniority and gender
PRTT3 <- coded_data %>% #<- change
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(D3, `p7q1T [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(PRTT3, aes(x = `p7q1T [3]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Documented methods") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

grid.arrange(
  p1,
  p2,
  p3,
  nrow = 1
)
