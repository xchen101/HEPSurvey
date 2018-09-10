library(tidyverse)
library(dplyr)
library(ggplot2)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

# PRE1 by seniority and gender
PRE1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q2E [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRE1, aes(x = `p6q2E [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Analysis code should be peer reviewed") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# PRE2 by seniority and gender
PRE2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q2E [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRE2, aes(x = `p6q2E [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Quality of the code shouldn't affect the \noutcome of the review") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# PRE3 by seniority and gender
PRE3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q2E [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRE3, aes(x = `p6q2E [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Documentation should be peer reviewed") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# PRE4 by seniority and gender
PRE4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q2E [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRE4, aes(x = `p6q2E [4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Data and code should be readily documented \nwhen the paper is submitted") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

########################################################
# Not included in the report!!!!!!!!!!!!!Need fixing!!!!!!!!!!!!!!#
# PRT1 by seniority and gender
PRT1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q2T [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRT1, aes(x = `p6q2T [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Analysis code should be peer reviewed") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# PRT2 by seniority and gender
PRT2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q2T [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRT2, aes(x = `p6q2T [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Quality of the code shouldn't affect the \noutcome of the review") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# PRT3 by seniority and gender
PRT3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q2T [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRT3, aes(x = `p6q2T [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Documentation should be peer reviewed") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# PRT4 by seniority and gender
PRT4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p6q2T [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRT4, aes(x = `p6q2T [4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Data and code should be readily documented \nwhen the paper is submitted") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

#########################################
#
#
#
#########################################

# PRTE1 by seniority and gender
PRTE1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p7q1E [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRTE1, aes(x = `p7q1E [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Data") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

# PRTE2 by seniority and gender
PRTE2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p7q1E [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRTE2, aes(x = `p7q1E [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Code") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

# PRTE3 by seniority and gender
PRTE3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p7q1E [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRTE3, aes(x = `p7q1E [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Related Twiki pages") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

# PRTE4 by seniority and gender
PRTE4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p7q1E [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRTE4, aes(x = `p7q1E [4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Presentation slides") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

# PRTE5 by seniority and gender
PRTE5 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p7q1E [5]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRTE5, aes(x = `p7q1E [5]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Scripted workflow") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

#########################################
#
#
#
#########################################

# PRTT1 by seniority and gender
PRTT1 <- coded_data %>% #<- change
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(Sex, D3, `p7q1T [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRTT1, aes(x = `p7q1T [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Data") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

# PRTT2 by seniority and gender
PRTT2 <- coded_data %>% #<- change
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(Sex, D3, `p7q1T [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRTT2, aes(x = `p7q1T [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Code") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

# PRTT3 by seniority and gender
PRTT3 <- coded_data %>% #<- change
  filter(!is.na(Sex), D4 == "TH") %>%
  group_by(Sex, D3, `p7q1T [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(PRTT3, aes(x = `p7q1T [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Documented methods") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")
