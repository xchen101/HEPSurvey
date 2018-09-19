library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

F1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(F1, aes(x = `p5q2 [1]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("How much additional work \nit takes to share.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

F2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(F2, aes(x = `p5q2 [2]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether I have the rights \nto share.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

F3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(F3, aes(x = `p5q2 [3]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("How competitive the research \narea is.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

F4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p4 <- ggplot(F4, aes(x = `p5q2 [4]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether the data or code \nis of good quality.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

F5 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [5]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p5 <- ggplot(F5, aes(x = `p5q2 [5]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether the data or code is \nessential for reproducibility.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

F6 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [6]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p6 <- ggplot(F6, aes(x = `p5q2 [6]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether I consider it to be \nuseful to other researchers.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

F7 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [7]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p7 <- ggplot(F7, aes(x = `p5q2 [7]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether I am asked to share \nby nother researchers.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

F8 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [8]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p8 <- ggplot(F8, aes(x = `p5q2 [8]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether the data or code \nwill be responsibly used.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

F9 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p5q2 [9]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p9 <- ggplot(F9, aes(x = `p5q2 [9]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Whether I am obligated to \nshare by mandates.") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

grid.arrange(
  p1,
  p2,
  p3,
  p4,
  p5,
  p6,
  p7,
  p8,
  p9,
  nrow = 3
)
