library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")

OS1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p2q1 [OS1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(OS1, aes(x = `p2q1 [OS1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Open Science benefits \nHEP as a discipline") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

OS2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p2q1 [OS2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(OS2, aes(x = `p2q1 [OS2]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Open Science benefits \nindividual researcher") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

OS3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p2q1 [OS3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(OS3, aes(x = `p2q1 [OS3]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Open Science benefits \nthe general public") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")


grid.arrange(
  p1,
  p2,
  p3,
  nrow = 1
)

################
################
################

OS1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D4, `p2q1 [OS1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(OS1, aes(x = `p2q1 [OS1]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Open Science benefits \nHEP as a discipline") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

OS2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p2q1 [OS2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(OS2, aes(x = `p2q1 [OS2]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Open Science benefits \nindividual researcher") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

OS3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p2q1 [OS3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(OS3, aes(x = `p2q1 [OS3]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Open Science benefits \nthe general public") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")


grid.arrange(
  p1,
  p2,
  p3,
  nrow = 1
)