install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)

# ========clean up========
# take out entries that did't complete the whole survey
completesubmission <- filter(demo_full, !is.na(submitdate))

# there are 161 entries incomplete submissions, are they salvageable? 

# drop irrelevant columns
completesubmission <- select(completesubmission, - c(startlanguage, token, refurl, datestamp))

# coerce to tibble
as_tibble(completesubmission)

# group by field
th <- filter(completesubmission, D4 == "Theory")
ex <- filter(completesubmission, D4 == "Experiment" )
other <- filter(completesubmission, D4 == "Other")
