library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")


PRE1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p6q2E [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(PRE1, aes(x = `p6q2E [1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Analysis code should be \npeer reviewed") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

PRE2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p6q2E [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(PRE2, aes(x = `p6q2E [2]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Quality of the code shouldn't \naffect the outcome of the review") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

PRE3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p6q2E [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(PRE3, aes(x = `p6q2E [3]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Documentation should be \npeer reviewed") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

PRE4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p6q2E [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p4 <- ggplot(PRE4, aes(x = `p6q2E [4]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Data and code should be readily \ndocumented when paper is submitted") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")


grid.arrange(
  p1,
  p2,
  p3,
  p4,
  nrow = 2
)
