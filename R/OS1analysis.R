library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

coded_data %>%
  group_by(D1, D4) %>%
  summarize(count = n())

coded_data %>%
  filter(!is.na(D1)) %>%
  group_by(`p2q1 [OS1]`, D1) %>%
  summarize(count = n())

OS1_field <- coded_data %>%
  filter(!is.na(D1)) %>%
  group_by(`p2q1 [OS1]`, D4) %>%
  summarize(count = n())
#is it possible to change the layout of the tibble printed here? probably not. 

ggplot(data = OS1_field)
ggplot(OS1_field, aes(x = `p2q1 [OS1]`, y = count, color = D4)) + 
  geom_line(aes(color = D4, group = D4)) +
  geom_point() 

#=========
OS1_field_sex <- coded_data %>%
  filter(!is.na(D1)) %>%
  group_by(Sex, D4, `p2q1 [OS1]`) %>% #<- change the third variable 
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(data = OS1_field_sex)
ggplot(OS1_field_sex, aes(x = `p2q1 [OS1]`, y = freq, color = Sex)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Open Science benefits HEP as a discipline") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

#=========
OS1 <- coded_data %>%
  group_by(Sex, D3, `p2q1 [OS1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OS1, aes(x = `p2q1 [OS1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ Sex) +
  ggtitle("Open Science benefits HEP as a discipline") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

#==============
# OS1 by seniority and gender
OS1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p2q1 [OS1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))


ggplot(OS1, aes(x = `p2q1 [OS1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Open Science benefits HEP as a discipline") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OS1 by field and gender
OS1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p2q1 [OS1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OS1, aes(x = `p2q1 [OS1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Open Science benefits HEP as a discipline") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")


# OS2 by seniority and gender
OS2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p2q1 [OS2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OS2, aes(x = `p2q1 [OS2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Open Science benefits individual researcher") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OS2 by field and gender
OS2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p2q1 [OS2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OS2, aes(x = `p2q1 [OS2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Open Science benefits individual researcher") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OS3 by seniority and gender
OS3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p2q1 [OS3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OS3, aes(x = `p2q1 [OS3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Open Science benefits the general public") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OS3 by field and gender
OS3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D4, `p2q1 [OS3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(OS3, aes(x = `p2q1 [OS3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D4) +
  ggtitle("Open Science benefits the general public") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")


tbl = table(coded_data$D4, coded_data$`p2q1 [OS3]`)
surveys <- read.csv('All_QC_FA.csv')
