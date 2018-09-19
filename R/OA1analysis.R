library(tidyverse)
library(dplyr)
library(ggplot2)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

# OA1 by seniority and gender
OA1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p2q2 [OA1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OA1, aes(x = `p2q2 [OA1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("OA has changed the way I find and read papers.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OA2 by seniority and gender
OA2 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p2q2 [OA2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OA2, aes(x = `p2q2 [OA2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("OA has changed the way I submit \npapers for publication.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OA1 by field and gender
OA1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p2q2 [OA1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OA1, aes(x = `p2q2 [OA1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("OA has changed the way I find and read papers.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OA2 by field and gender
OA2 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p2q2 [OA2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OA2, aes(x = `p2q2 [OA2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) + # <-change
  ggtitle("OA has changed the way I submit \npapers for publication.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")
