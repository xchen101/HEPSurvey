install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)
library(tidyr)
library(scales)

# ========external data source========
# Users demography by location of home institute and member status, in year 2018. Source: CERN annual report
users <- read_csv("Data/CERN2018users.csv")

# ========clean up========
# modify country names for better readability

completesubmission <- 
  completesubmission%>% 
  mutate(D2 = as.character(D2)) %>% 
  mutate(D2 = replace(D2, D2 == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(D2 = replace(D2, D2 == "the United Kingdom of Great Britain and Northern Ireland", "UK")) %>% 
  mutate(D2 = replace(D2, D2 == "United States of America", "US")) %>% 
  mutate(D2 = replace(D2, D2 == "Iran (Islamic Republic of)", "Iran")) %>% 
  mutate(D2 = replace(D2, D2 == "Russian Federation", "Russia"))


#completesubmission <- recode(data$D2, "Venezuela (Bolivarian Republic of)" = "Venezuela")


#completesubmission[completesubmission$D2 == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"

#completesubmission1 <- gsub("Venezuela (Bolivarian Republic of)", "Venezuela", completesubmission$D2)

#completesubmission1 <- data.frame(completesubmission1)
# ========Demography========
# demo by location of home institute and field
ggplot(completesubmission) +
  geom_bar(mapping = aes(x = reorder(D2, D2, function(x) + length(x)), fill = D4)) +
  coord_flip() +
  labs(x = "Country/ Region", y = "Count") +
  guides(fill = guide_legend(title = "Field"))

# ========Open Science========

# ========Open Access========

# ========Open Data========

# ========clean up========