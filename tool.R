library(tidyverse)
library(dplyr)
library(ggplot2)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

# Tool31 by seniority and gender
Tool31 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p8q3 [1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Tool3, aes(x = `p8q3 [1]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Endorsement by a trusted group") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

# Tool32 by seniority and gender
Tool32 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p8q3 [2]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Tool32, aes(x = `p8q3 [2]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Efficiency improvement of workflow") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")


# Tool33 by seniority and gender
Tool33 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p8q3 [3]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Tool33, aes(x = `p8q3 [3]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Ease of use") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")

# Tool34 by seniority and gender
Tool34 <- coded_data %>% #<- change
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p8q3 [4]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

ggplot(Tool34, aes(x = `p8q3 [4]`, y = freq, color = Sex)) + # <- change
  geom_line() +
  geom_point() +
  facet_wrap(~ D3) +
  ggtitle("Adoption rate among my peers") +
  labs(x = "Not at all affect - Affect a lot", y = "Frequency")
