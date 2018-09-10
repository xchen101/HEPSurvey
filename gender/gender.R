setwd("/Users/xiaolichen/R projects/HEP survey/HEPSurvey/Data/Base/")
data = read.csv("usable_QC_FA.csv")
library (ggplot2)
library (plotly)

p<- plot_ly(
  x = c("Male", "Female", "NA"),
  y = c(
    count_male <- length (which (data$D1 == "Male")),
    count_female <- length (which (data$D1 == "Female")),
    count_NA <- length (which (data$D1 == "Prefer not to say"))
  ),
  name = "Gender",
  type = "bar",
  text = y, textposition = 'top'
) %>%
  layout(title = "Gender of participant")
p
