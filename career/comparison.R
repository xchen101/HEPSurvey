bins <- c("0-4", "5-10", "11-15", "16-20", ">20")
INSdata <- c(18, 22, 13, 10, 37)
SURdata <- c(24, 21, 11, 9, 34)
percentage <- data.frame(bins, INSdata, SURdata)

p <- plot_ly(percentage, x = ~bins, y = ~INSdata, type = 'bar', name = 'INSPIRE stats') %>%
  add_trace(y = ~SURdata, name = 'Survey stats') %>%
  layout(
    title = "Career stage distribition \n of HEP researchers",
    xaxis = list(title = 'Years in the field'),
    yaxis = list(title = 'Percentage of sample'), 
    barmode = 'group')

p
