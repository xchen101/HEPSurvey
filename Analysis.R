
# Housekeeping ----------------------------------------------------------------------------------------------------------------------------------------------
library(mergeutils)
library(tidyverse)
library(dplyr)
library(tidyr)
library(scales)
library(tibble)
library(gridExtra)
library(forcats)
library(reshape2)
library(broom)
library(RColorBrewer)
library(cluster)
library(gmodels)
library(ggpubr)
library(vcd)
library(lubridate)

# take out entries that did't complete the whole survey
completesubmission <- filter(data, !is.na(submitdate))

# there are 161 entries incomplete submissions, are they salvageable? 

# drop irrelevant columns
completesubmission <- select(completesubmission, - c(startlanguage, token, refurl, datestamp))

# coerce to tibble, save as RData file
completesubmission <- as_tibble(completesubmission)
save(completesubmission, file = "data.RData")

# Completion time ----------------------------------------------------------------------------------------------------------------------------------------------
start <- dmy_hms(completesubmission$startdate)
end <- dmy_hms(completesubmission$submitdate)
time.interval <- start %--% end

time.duration <- as.duration(time.interval)

summary(time.duration)

# Demography ----------------------------------------------------------------------------------------------------------------------------------------------

# Country/Region
# modify country names for better readability
completesubmission <- 
  completesubmission%>% 
  mutate(D2 = as.character(D2)) %>% 
  mutate(D2 = replace(D2, D2 == "Venezuela (Bolivarian Republic of)", "Venezuela")) %>% 
  mutate(D2 = replace(D2, D2 == "the United Kingdom of Great Britain and Northern Ireland", "UK")) %>% 
  mutate(D2 = replace(D2, D2 == "United States of America", "US")) %>% 
  mutate(D2 = replace(D2, D2 == "Iran (Islamic Republic of)", "Iran")) %>% 
  mutate(D2 = replace(D2, D2 == "Russian Federation", "Russia"))

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

  # ========external data source========
  # Users demography by location of home institute and member status, in year 2018. Source: CERN annual report
user <- read_csv("Data/CERN2018users.csv")

    # modify country names to match main data set
user <- user %>% 
  mutate(Country_Region = as.character(Country_Region)) %>% 
  mutate(Country_Region = replace(Country_Region, Country_Region == "People’s Republic of China", "China")) %>% 
  mutate(Country_Region = replace(Country_Region, Country_Region == "United Kingdom", "UK")) %>% 
  mutate(Country_Region = replace(Country_Region, Country_Region == "USA", "US"))

  # CERN users by location of home institute & membership status
ggplot(user) + 
  geom_bar(mapping = aes(x = reorder(Country_Region, count), y = count, fill = status), stat = "identity") +
  labs(x = "Country/ Region", 
       y = "Count",
       caption = "AM: Associate Member, 
       AM-P: Prestage Associate Member, 
       M: Member, O: Other, OB: Observer
       Data source: CERN Annual Report") +
  coord_flip() 

  # compare the percentage of CERN users and survey participants by location of home institute
comparison <- read_csv("Data/comparison.csv")

represented <- filter(comparison, !count == 0)

ggplot(represented) + 
  geom_bar(mapping = aes(x = reorder(country, count), y = (count)/sum(count), alpha = 1/3), fill = "red", stat = "identity") + 
  geom_bar(mapping = aes(x = reorder(country, count), y = (user)/sum(user), alpha = 1/3), fill = "blue", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Country/ Region", 
       y = "Percentage", 
       caption = "Data source: CERN Annual Report") +
  #  theme(legend.position = "none") +
  #  scale_alpha_discrete(guide = FALSE) +
  guides(alpha = FALSE) +
  coord_flip()

  # unrepresented countries
unrepresented <- filter(comparison, count == 0)
  # % of unrepresented users (2%)
sum(unrepresented$user) / sum(user$count)

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

# years of experience
experience <- completesubmission %>% 
  count(D3) 
experience <- mutate(experience, percentage = n/sum(n) * 100)

names(experience)[names(experience) == "n"] <- "Count"
names(experience)[names(experience) == "D3"] <- "Years of Experience"

write.csv(experience, file = "Data/experience.csv")

  # experience & gender
ggplot(completesubmission) +
  geom_bar(mapping = aes(x = D3, fill = D1)) +
  labs(x = "Years of Experience",
       y = "Count") +
  guides(fill = guide_legend(title = "Gender")) +
  coord_flip() 

# Gender
  # create gender stats table
gender <- completesubmission %>% 
  count(D1) 
gender <- mutate(gender, percentage = n/sum(n) * 100)

names(gender)[names(gender) == "n"] <- "Count"
names(gender)[names(gender) == "D1"] <- "Gender"

write.csv(gender, file = "Data/gender.csv")

  # plot gender count
gender.pct <- completesubmission %>% group_by(D1) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count))

ggplot(gender.pct, aes(x = D1, y = pct)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels=percent) + 
  geom_text(data=gender.pct, 
            aes(label=paste0(round(pct*100,1),"%"), 
                y=pct+0.012), 
            size=4) +
  labs(x = NULL, y = NULL)

  # plot gender count by experience
ggplot(completesubmission) +
  geom_bar(mapping = aes(x = D1, fill = D3)) +
  labs(x = "Gender",
       y = "Count") +
  guides(fill = guide_legend(title = "Experience level"))



ggplot(completesubmission, aes(x = D1, group = D4)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day") +
  facet_grid(~D4) +
  scale_y_continuous(labels = scales::percent)

ggplot(completesubmission, aes(x = D1)) + 
  geom_bar(aes(x = D1, y = (..count..)/sum(..count..)), stat = "count") + 
  scale_y_continuous(labels=scales::percent) +
  labs (x = NULL,
        y = NULL) +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), stat = "count", vjust = -.5)
  
  theme(axis.ticks.x = element_blank(),
        axis.text = element_text(size = 7))

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

  # collaboration size
summary(completesubmission$D4_1)


# Attitude ------------------------------------------------------------------------------------------------------------------------------------------------


par(mfrow=c(2,3))
for (i in 10:14) {
  plot(completesubmission[,i], main=colnames(completesubmission)[i],
       ylab = "Count", col="steelblue", las = 2)
}


# Open Data -----------------------------------------------------------------------------------------------------------------------------------------------
ODE <- select(completesubmission, 19:23)

summary(ODE)
summary <- summary(ODE)
ODE_summary <- do.call(cbind, lapply(ODE, summary))
ODE_sum <- as_tibble(ODE_summary, rownames("levels"))
names(ODE_sum)[names(ODE_sum) == "p3q1E_OD1"] <- "read/heard about OD" 
names(ODE_sum)[names(ODE_sum) == "p3q1E_OD2"] <- "tried to find OD" 
names(ODE_sum)[names(ODE_sum) == "p3q1E_OD3"] <- "used OD" 
names(ODE_sum)[names(ODE_sum) == "p3q1E_OD4"] <- "openly released OD" 
names(ODE_sum)[names(ODE_sum) == "p3q1E_OD5"] <- "never interacted with OD" 


ODE_sum$levels <- seq_len(nrow(ODE_sum))
ODE2 <- melt(ODE_sum, id.vars = "Yes/No")
ODE2 <- 
  ODE2 %>% 
  mutate("Yes/No" = as.character("Yes/No")) %>% 
  mutate("Yes/No" = replace("Yes/No", "Yes/No" == "1", "Yes")) %>% 
  mutate("Yes/No" = replace("Yes/No", "Yes/No" == "2", "No"))


ODE_sum$levels <- seq_len(nrow(ODE_sum))
ODE2 <- melt(ODE_sum, id.vars = "levels")
ODE2 <- 
  ODE2 %>% 
  mutate(levels = as.character(levels)) %>% 
  mutate(levels = replace(levels, levels == "1", "Yes")) %>% 
  mutate(levels = replace(levels, levels == "2", "No")) 

ggplot(ODE2, aes(x = variable, y = value, fill = levels)) +
  geom_bar(stat = "identity") +
  labs(x = "Factor", y = "Count") +
  scale_fill_discrete(NULL) +
  scale_fill_manual(values = c("Yes" = "grey", "No" = alpha= 0)) +
  coord_flip()

# when to share

# all freetext comments (output gathered in external freetext_comments.md file)
when_other <- filter(completesubmission, !is.na(p4q1T_O))
when_other <- select(when_other, p4q1T_O)

when_other <- filter(completesubmission, !is.na(p4q1E_O))
when_other <- select(when_other, p4q1E_O)

when_other <- filter(completesubmission, !is.na(p4q2_O))
when_other <- select(when_other, p4q2_O)

when_other <- filter(completesubmission, !is.na(p5q1_N))
when_other <- select(when_other, p5q1_N)

when_other <- filter(completesubmission, !is.na(p7q2))
when_other <- select(when_other, p7q2)

when_other


# who decide ----------------------------------------------------------------------------------------------------------------------------------------------

# modify country names for better readability
whodecide <- 
  completesubmission%>% 
  select(D4, p4q2) %>% 
  mutate(p4q2 = as.character(p4q2)) %>% 
  mutate(p4q2 = replace(p4q2, p4q2 == "Each individual researcher should decide for their work autonomously.", "Each individual researcher\n should decide for their \nwork autonomously.")) %>% 
  mutate(p4q2 = replace(p4q2, p4q2 == "Each collaboration or team should decide for their work.", "Each collaboration or\n team should decide for\n their work.")) %>% 
  mutate(p4q2 = replace(p4q2, p4q2 == "The HEP community as a whole should decide and set practices.", "The HEP community as\n a whole should decide and\n set practices.")) %>% 
  mutate(p4q2 = replace(p4q2, p4q2 == "The funding bodies or institutions should decide and develop policies for researchers to follow.", "The funding bodies or institutions\n should decide and develop\n policies for researchers to follow.")) 

ggplot(whodecide) + 
  geom_bar(mapping = aes(x = reorder(p4q2, p4q2, function(x) + length(x)), fill = D4)) +
  labs(x = NULL, y = "Count") +
  coord_flip()

# ========Sharing Preference========

# Factors
factor <- select(completesubmission, 59:67)

summary(factor)
summary <- summary(factor)
factor_summary <- do.call(cbind, lapply(factor, summary))
factor_sum <- as_tibble(factor_summary, rownames("levels"))
names(factor_sum)[names(factor_sum) == "p5q2_1"] <- "Additional work" 
names(factor_sum)[names(factor_sum) == "p5q2_2"] <- "Having the rights" 
names(factor_sum)[names(factor_sum) == "p5q2_3"] <- "Competition in the subject area" 
names(factor_sum)[names(factor_sum) == "p5q2_4"] <- "Quality of the data and code"
names(factor_sum)[names(factor_sum) == "p5q2_5"] <- "Necessary for reproduction"
names(factor_sum)[names(factor_sum) == "p5q2_6"] <- "Self-evaluated usefulness"
names(factor_sum)[names(factor_sum) == "p5q2_7"] <- "Requested by others"
names(factor_sum)[names(factor_sum) == "p5q2_8"] <- "Ensured responsible usage"
names(factor_sum)[names(factor_sum) == "p5q2_9"] <- "Policy and mandates"

factor_sum$levels <- seq_len(nrow(factor_sum))
factor2 <- melt(factor_sum, id.vars = "levels")
factor2 <- 
  factor2 %>% 
  mutate(levels = as.character(levels)) %>% 
  mutate(levels = replace(levels, levels == "1", "1 Not at all affect")) %>% 
  mutate(levels = replace(levels, levels == "2", "2 Rarely affect")) %>% 
  mutate(levels = replace(levels, levels == "3", "3 Neutral")) %>% 
  mutate(levels = replace(levels, levels == "4", "4 Somewhat affect")) %>% 
  mutate(levels = replace(levels, levels == "5", "5 Affect a lot"))  

ggplot(factor2, aes(x = variable, y = value, fill = levels)) +
  geom_bar(stat = "identity") +
  labs(x = "Factor", y = "Count") +
  scale_fill_brewer(palette = "RdBu") +
  coord_flip()

# single factor
ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_1)) +
  labs(x = "Additional work", 
       y = "Count")




# ========Documentation and Peer Review========

# comment about documentation for review
comment <- filter(completesubmission,!is.na(p7q2))
comment 
tibble <- summary
# ========Tools========