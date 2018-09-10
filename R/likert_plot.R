#an attempt to reproduce from this tutorial:
#http://rnotr.com/likert/ggplot/barometer/likert-plots/

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)

coded_data <- read_csv("Data/Base/Usable_QC_FA_coded.csv")

mytitle <- "\"What is your impression about Open Science?"
mylevels <- c("Strongly disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Strongly agree")

OS1 <- coded_data %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex, D3, `p2q1 [OS1]`) %>% #<- change  
  summarize(count = n()) %>%
  mutate(freq = count / sum(count))

OS1

numlevels <- length()