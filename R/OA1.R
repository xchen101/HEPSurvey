library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")

OA1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p2q2 [OA1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(OA1, aes(x = `p2q2 [OA1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("OA has changed the way I \nfind and read papers.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

OA2 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D3, `p2q2 [OA2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(OA2, aes(x = `p2q2 [OA2]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("OA has changed the way I \nsubmit papers for publication.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

grid.arrange(
  p1,
  p2,
  nrow = 1
)



OA1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D4, `p2q2 [OA1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(OA1, aes(x = `p2q2 [OA1]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("OA has changed the way I \nfind and read papers.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

# OA2 by field and gender
OA2 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(D4, `p2q2 [OA2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p4 <- ggplot(OA2, aes(x = `p2q2 [OA2]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("OA has changed the way I \nsubmit papers for publication.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

grid.arrange(
  p3,
  p4,
  nrow = 1
)