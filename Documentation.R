library(tidyverse)
library(dplyr)
library(ggplot2)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

# Doc1 by seniority and gender
Doc1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q1 [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Doc1, aes(x = `p6q1 [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Prefer asking people than read documentation") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# Doc1 by field and gender
Doc1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p6q1 [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Doc1, aes(x = `p6q1 [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Prefer asking people than read documentation") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# Doc2 by seniority and gender
Doc2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q1 [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Doc2, aes(x = `p6q1 [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Documentation affect the reproducibility of \nthe results") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# Doc2 by field and gender
Doc2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p6q1 [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Doc2, aes(x = `p6q1 [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Documentation affect the reproducibility of \nthe results") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# Doc3 by seniority and gender
Doc3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q1 [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Doc3, aes(x = `p6q1 [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Writing documentation is not considered \nproductive use of time") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# Doc3 by field and gender
Doc3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p6q1 [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Doc3, aes(x = `p6q1 [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Writing documentation is not considered \nproductive use of time") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# Doc4 by seniority and gender
Doc4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q1 [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Doc4, aes(x = `p6q1 [4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Documentation is not useful for outsiders.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# Doc4 by field and gender
Doc4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p6q1 [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Doc4, aes(x = `p6q1 [4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Documentation is not useful for outsiders.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")
