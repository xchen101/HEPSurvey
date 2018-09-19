library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gtable)
library(gridExtra)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

Doc1 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p6q1 [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p1 <- ggplot(Doc1, aes(x = `p6q1 [1]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Prefer asking people than \nread documentation") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

Doc2 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p6q1 [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p2 <- ggplot(Doc2, aes(x = `p6q1 [2]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Documentation affect the \nreproducibility of the results") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

Doc3 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p6q1 [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p3 <- ggplot(Doc3, aes(x = `p6q1 [3]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Writing documentation is not \nconsidered productive use of time") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

Doc4 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(D4, `p6q1 [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

p4 <- ggplot(Doc4, aes(x = `p6q1 [4]`, y = freq, color = D4)) + # <- change
  geom_line() +
  geom_point() +
  ggtitle("Documentation is not useful \nfor outsiders.") +
  labs(x = "Strongly disagree - Strongly agree", y = "Frequency")

grid.arrange(
  p1,
  p2,
  p3,
  p4,
  nrow = 2
)