library(tidyverse)
library(dplyr)
library(ggplot2)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

# OD1 by seniority and gender
OD1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p3q2E [OD_E1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD1, aes(x = `p3q2E [OD_E1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Data used in published studies \nshould be made open.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD2 by seniority and gender
OD2 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p3q2E [OD_E2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD2, aes(x = `p3q2E [OD_E2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Access to research data should be provided \nto anyone who's interested to see them.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD3 by seniority and gender
OD3 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p3q2E [OD_E3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD3, aes(x = `p3q2E [OD_E3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Open and easy access to research data \nfrom past studies is helpful to me.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD4 by seniority and gender
OD4 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p3q2E [OD_E4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD4, aes(x = `p3q2E [OD_E4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Whether data from my study will be open \ndoes not affect how I work.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")



# OD1 by field and gender
OD1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p3q2E [OD_E1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD1, aes(x = `p3q2E [OD_E1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Data used in published studies \nshould be made open.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD2 by field and gender
OD2 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p3q2E [OD_E2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD2, aes(x = `p3q2E [OD_E2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Access to research data should be provided \nto anyone who's interested to see them.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD3 by seniority and gender
OD3 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p3q2E [OD_E3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD3, aes(x = `p3q2E [OD_E3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Open and easy access to research data \nfrom past studies is helpful to me.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OD4 by field and gender
OD4 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p3q2E [OD_E4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD4, aes(x = `p3q2E [OD_E4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Whether data from my study will be open \ndoes not affect how I work.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")


# OD5 TH by seniority and gender
OD5 <- coded_data %>%
  filter(!is.na(Sex), !is.na(`p3q2T [OD_E5]`)) %>%
  group_by(Sex, D3, `p3q2T [OD_E5]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OD5, aes(x = `p3q2T [OD_E5]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Open and easy access to experiment data \nfrom past studies is helpful to me. (only TH)") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")
