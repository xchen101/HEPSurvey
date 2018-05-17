df <- data.frame(
  group = c("Male", "Female", "NA"),
  value = c(
    count_male <- length (which (data$D1 == "Male")),
    count_female <- length (which (data$D1 == "Female")),
    count_NA <- length (which (data$D1 == "Prefer not to say"))
  )
)

library (ggplot2)

bp <- ggplot(df, aes(x = group, y = value, fill = group))
bp
