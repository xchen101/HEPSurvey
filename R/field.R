setwd("/Users/xiaolichen/R projects/HEP survey/HEPSurvey/Data/Base/")
data = read.csv("usable_QC_FA.csv")
library (plotly)

x = c("EXP", "TH", "Other")

y = c(
  count_EXP <- length (which (data$D4 == "Experiment")),
  count_TH <- length (which (data$D4 == "Theory")),
  count_Other <- length (which (data$D4 == "Other")))

field <- data.frame(x, y)

field$x <- factor(field$x, levels = field[["x"]])

p<- plot_ly(
  x = ~x,
  y = ~y,
  name = "field",
  type = "bar"
) %>%
  layout(title = "Field of participant",
         xaxis = list(title = " "),
         yaxis = list(title = " "))
p
