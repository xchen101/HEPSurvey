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

p1 <- ggplot(OD1, aes(x = `p3q2E [OD_E1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Data used in published studies \nshould be made open.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD2 by seniority and gender
OD2 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p3q2E [OD_E2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(OD2, aes(x = `p3q2E [OD_E2]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Access to data should be \nprovided to anyone who's interested.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD3 by seniority and gender
OD3 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p3q2E [OD_E3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(OD3, aes(x = `p3q2E [OD_E3]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Access to research data from \npast studies is helpful to me.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD4 by seniority and gender
OD4 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p3q2E [OD_E4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p4 <- ggplot(OD4, aes(x = `p3q2E [OD_E4]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether data will be open \ndoes not affect how I work.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

grid.arrange(
  p1,
  p2,
  p3,
  p4,
  nrow = 2
)

# OD1 by field and gender
OD1 <- coded_data %>%
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(D3, `p3q2E [OD_E1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p5 <- ggplot(OD1, aes(x = `p3q2E [OD_E1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Data used in published studies \nshould be made open.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD2 by field and gender
OD2 <- coded_data %>%
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(D3, `p3q2E [OD_E2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p6 <- ggplot(OD2, aes(x = `p3q2E [OD_E2]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Access to data should be \nprovided to anyone who's interested.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD3 by seniority and gender
OD3 <- coded_data %>%
  filter(!is.na(Sex), D4 =="TH") %>%
  group_by(D3, `p3q2E [OD_E3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p7 <- ggplot(OD3, aes(x = `p3q2E [OD_E3]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Access to research data from \npast studies is helpful to me.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD4 by field and gender
OD4 <- coded_data %>%
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(D3, `p3q2E [OD_E4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p8 <- ggplot(OD4, aes(x = `p3q2E [OD_E4]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether data will be open \ndoes not affect how I work.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")


# OD5 TH by seniority and gender
OD5 <- coded_data %>%
#  filter(!is.na(Sex), !is.na(`p3q2T [OD_E5]`)) %>%
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(D3, `p3q2T [OD_E5]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p9 <- ggplot(OD5, aes(x = `p3q2T [OD_E5]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Access to experiment data \nfrom past studies is helpful to me. ") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

p9
grid.arrange(
  p5,
  p6,
  p7,
  p8,
  nrow = 2
)
