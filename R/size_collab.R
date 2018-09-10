setwd("/Users/xiaolichen/R projects/HEP survey/HEPSurvey/Data/Base/")
data = read.csv("usable_QC_FA.csv")
library (plotly)

x = c("> 500 members", "101 - 500 members", "51 - 100 members", "20 - 50 members", "< 20 members")

y = c(
  count_1 <- length (which (data$D4_1 == "> 500 members")),
  count_2 <- length (which (data$D4_1 == "101 - 500 members")),
  count_3 <- length (which (data$D4_1 == "51 - 100 members")),
  count_4 <- length (which (data$D4_1 == "20 - 50 members")),
  count_5 <- length (which (data$D4_1 == "< 20 members")))

size <- data.frame(x, y)

size$x <- factor(size$x, levels = size[["x"]])

plot_ly(
  x = ~x,
  y = ~y,
  name = "size",
  type = "bar"
) %>%
  layout(title = "Size of collaboration",
         xaxis = list(title = " "),
         yaxis = list(title = " "))
