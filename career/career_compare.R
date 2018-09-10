library(plotly)
x <- c("0-4", "5-10", "11-15", "16-20", ">20")
y1 <- c(18, 22, 13, 10, 37)
y2 <- c(24, 21, 11, 9, 34)
data <- data.frame(x, y1, y2)

data$x <- factor(data$x, levels = data[["x"]])

p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = "INSPIRE data", marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~y2, name = 'Survey data', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(title = "Career stage distribition of HEP researchers",
         xaxis = list(title = "Years in the field"),
         yaxis = list(title = "Percentage"),
         margin = list(b = 100),
         barmode = 'group')

p
