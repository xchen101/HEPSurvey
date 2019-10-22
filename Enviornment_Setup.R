install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)

# ========clean up========
# take out entries that didn't complete the demo/first section
demo_full <- filter(data, !is.na(D4))
# take out entries that did't complete the whole survey
completesubmission <- filter(demo_full, !is.na(submitdate))

# there are 161 entries incomplete submissions, are they salvageable? 

# drop irrelevant columns
lean <- select(completesubmission, - c(startlanguage, token, refurl, datestamp))

# group by field
th <- filter(lean, D4 == "Theory")
ex <- filter(lean, D4 == "Experiment" )
other <- filter(lean, D4 == "Other")




