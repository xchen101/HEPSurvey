install.packages("tidyverse")
install.packages("dplyr")
install.packages("remotes")
install.packages("gridExtra")
remotes::install_github("vapniks/mergeutils")
library(mergeutils)
library(tidyverse)
library(dplyr)
library(tidyr)
library(scales)
library(tibble)
library(gridExtra)


# ========external data source========
# Users demography by location of home institute and member status, in year 2018. Source: CERN annual report
user <- read_csv("Data/CERN2018users.csv")

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


# ========Demography========
# Country/Region
ggplot(completesubmission) +
  geom_bar(mapping = aes(x = reorder(D2, D2, function(x) + length(x)))) +
  labs(x = "Country/ Region", y = "Count") +
  coord_flip()

# Survey participants by country of home institute and field
ggplot(completesubmission) +
  geom_bar(mapping = aes(x = reorder(D2, D2, function(x) + length(x)), fill = D4)) +
  coord_flip() +
  labs(x = "Country/ Region", y = "Count") +
  guides(fill = guide_legend(title = "Field"))

# CERN user stats from CERN annual report 2018
# modify country names for better readability
user <- user %>% 
  mutate(Country_Region = as.character(Country_Region)) %>% 
  mutate(Country_Region = replace(Country_Region, Country_Region == "People’s Republic of China", "China")) %>% 
  mutate(Country_Region = replace(Country_Region, Country_Region == "United Kingdom", "UK")) %>% 
  mutate(Country_Region = replace(Country_Region, Country_Region == "USA", "US"))

# plot CERN users by location of home institute
ggplot(user) + 
  geom_bar(mapping = aes(x = reorder(Country_Region, count), y = count, fill = status), stat = "identity") +
  labs(x = "Country/ Region", 
       y = "Count",
       caption = "AM: Associate Member, AM-P: Prestage Associate Member, 
       M: Member, O: Other, OB: Observer") +
  coord_flip() 


# compare the percentage of CERN users and survey participants by location of home institute

comparison <- read_csv("Data/comparison.csv")

ggplot(represented) + 
  geom_bar(mapping = aes(x = reorder(country, count), y = (count)/sum(count), alpha = 1/3), fill = "red", stat = "identity") + 
  geom_bar(mapping = aes(x = reorder(country, count), y = (user)/sum(user), alpha = 1/3), fill = "blue", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Country/ Region", 
       y = "Percentage", 
    #   title = "Comparison",
       caption = "Data source: CERN Annual Report") +
  #  theme(legend.position = "none") +
  #  scale_alpha_discrete(guide = FALSE) +
  guides(alpha = FALSE) +
  coord_flip()

# unrepresented countries
unrepresented <- filter(comparison, count == 0)
represented <- filter(comparison, !count == 0)
sum(unrepresented$user) / sum(comparison$user)

unrepresented <- within(unrepresented, rm(count))
write.csv(unrepresented, file = "Data/Unrepresented.csv", row.names = FALSE)

ggplot(unrepresented) + 
  geom_bar(mapping = aes(x = reorder(country, user), y = user, alpha = 1/3), fill = "blue", stat = "identity") +
  labs(x = "Country/ Region", 
       y = "Count", 
       title = "Unrepresented",
       caption = "Data source: CERN Annual Report") +
  guides(alpha = FALSE) +
  coord_flip()

# ================
# years of experience
experience <- completesubmission %>% 
  count(D3) 
experience <- mutate(experience, percentage = n/sum(n) * 100)

names(experience)[names(experience) == "n"] <- "Count"
names(experience)[names(experience) == "D3"] <- "Years of Experience"

write.csv(experience, file = "Data/experience.csv")

ggplot(experience) %>% 
  geom_bar(mapping = aes(x = D3, y = n))

# ================
# Gender
gender <- completesubmission %>% 
  count(D1) 
gender <- mutate(gender, percentage = n/sum(n) * 100)

names(gender)[names(gender) == "n"] <- "Count"
names(gender)[names(gender) == "D1"] <- "Gender"

write.csv(gender, file = "Data/gender.csv")

ggplot(completesubmission) +
  geom_bar(mapping = aes(x = D1, fill = D3)) +
  labs(x = "Gender",
       y = "Count") +
  guides(fill = guide_legend(title = "Experience level"))

ggplot(gender) + 
  geom_bar(mapping = aes(x = Gender, y = (Count)/sum(Count)), fill = "red", stat = "identity")

# ================
# Field
field <- completesubmission %>% 
  count(D4) 
field <- mutate(field, percentage = n/sum(n) * 100)

names(field)[names(field) == "n"] <- "Count"
names(field)[names(field) == "D4"] <- "Field"

write.csv(field, file = "Data/field.csv")

# Field_other
field_other <- filter(completesubmission,!is.na(D4_2))
select(field_other, D4_2)

# ========Attitude========

# ========Sharing Preference========

# ========Documentation and Peer Review========

# comment about documentation for review
comment <- filter(completesubmission,!is.na(p7q2))
comment 

# ========Tools========