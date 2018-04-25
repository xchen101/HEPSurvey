library(plotly)
packageVersion('plotly')

data = read.csv("career.csv")

year <- seq(1, 67)
p <- plot_ly(x = ~year) %>%
  add_trace(y = ~data$count, type = 'scatter', mode = 'lines', name = 'number of researcher') %>%
  layout(title = "Career stage distribution \n of HEP researchers",
       xaxis = list(title = "Years in the field"),
       yaxis = list (title = "Number of researchers"))

p
