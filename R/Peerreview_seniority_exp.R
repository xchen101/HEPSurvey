library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("https://raw.githubusercontent.com/xchen101/HEPSurvey/master/Data/Base/Usable_QC_FA_coded.csv")

PRTE1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p7q1E [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(PRTE1, aes(x = `p7q1E [1]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Data") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

PRTE2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p7q1E [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(PRTE2, aes(x = `p7q1E [2]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Code") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

PRTE3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p7q1E [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(PRTE3, aes(x = `p7q1E [3]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Related Twiki pages") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

PRTE4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p7q1E [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p4 <- ggplot(PRTE4, aes(x = `p7q1E [4]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Presentation slides") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

PRTE5 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D3, `p7q1E [5]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p5 <- ggplot(PRTE5, aes(x = `p7q1E [5]`, y = freq, color = D3)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Scripted workflow") +
  labs(x = "Very unhelpful - Very helpful", y = "Frequency")

grid.arrange(
  p1,
  p2,
  p3,
  p4,
  p5,
  nrow = 3
)
