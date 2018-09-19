library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

# F1 by seniority and gender
F1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p5q2 [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F1, aes(x = `p5q2 [1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
#  facet_wrap(~ Sex) +
  ggtitle("Factor 1 \nHow much additional work it takes to share.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")


# F1 by seniority and gender
F1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F1, aes(x = `p5q2 [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 1 \nHow much additional work it takes to share.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F1 by field and gender
F1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F1, aes(x = `p5q2 [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 1 \nHow much additional work it takes to share.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F2 by seniority and gender
F2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F2, aes(x = `p5q2 [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 2\nWhether I have the rights to share.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F2 by field and gender
F2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F2, aes(x = `p5q2 [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 2\nWhether I have the rights to share.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F3 by seniority and gender
F3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F3, aes(x = `p5q2 [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 3\nHow competitive the research area is.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F3 by field and gender
F3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F3, aes(x = `p5q2 [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 3\nHow competitive the research area is.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F4 by seniority and gender
F4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F4, aes(x = `p5q2 [4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 4\nWhether the data or code is of good quality.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F4 by field and gender
F4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F4, aes(x = `p5q2 [4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 4\nWhether the data or code is of good quality.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F5 by seniority and gender
F5 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [5]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F5, aes(x = `p5q2 [5]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 5\nWhether the data or code is essential \nfor reproducing the study.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F5 by field and gender
F5 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [5]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F5, aes(x = `p5q2 [5]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 5\nWhether the data or code is essential \nfor reproducing the study.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F6 by seniority and gender
F6 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [6]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F6, aes(x = `p5q2 [6]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 6\nWhether I consider it to be useful to \nother researchers.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F6 by field and gender
F6 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [6]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F6, aes(x = `p5q2 [6]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 6\nWhether I consider it to be useful to \nother researchers.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F7 by seniority and gender
F7 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [7]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F7, aes(x = `p5q2 [7]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 7\nWhether I am asked to share by \nother researchers.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F7 by field and gender
F7 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [7]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F7, aes(x = `p5q2 [7]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 7\nWhether I am asked to share by \nother researchers.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F8 by seniority and gender
F8 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [8]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F8, aes(x = `p5q2 [8]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 8\nWhether the data or code will be \nresponsibly used.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F8 by field and gender
F8 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [8]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F8, aes(x = `p5q2 [8]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 8\nWhether the data or code will be \nresponsibly used.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F8 by seniority and gender
F8 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [8]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F8, aes(x = `p5q2 [8]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 8\nWhether the data or code will be \nresponsibly used.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F8 by field and gender
F8 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [8]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F8, aes(x = `p5q2 [8]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 8\nWhether the data or code will be \nresponsibly used.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F8 by seniority and gender
F8 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [8]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F8, aes(x = `p5q2 [8]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 8\nWhether the data or code will be \nresponsibly used.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F8 by field and gender
F8 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [8]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F8, aes(x = `p5q2 [8]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 8\nWhether the data or code will be \nresponsibly used.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F9 by seniority and gender
F9 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p5q2 [9]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F9, aes(x = `p5q2 [9]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Factor 9\nWhether I am obligated to share by mandates.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# F9 by field and gender
F9 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p5q2 [9]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(F9, aes(x = `p5q2 [9]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Factor 9\nWhether I am obligated to share by mandates.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")
