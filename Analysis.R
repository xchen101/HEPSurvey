
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
library(cowplot)

# take out entries that did't complete the whole survey
completesubmission <- filter(data, !is.na(submitdate))

# there are 161 entries incomplete submissions, are they salvageable? 

# drop irrelevant columns
completesubmission <- select(completesubmission, - c(startlanguage, token, refurl, datestamp))

# coerce to tibble, save as RData file
completesubmission <- as_tibble(completesubmission)
save(completesubmission, file = "data.RData")

# Completion time ----------------------------------------------------------------------------------------------------------------------------------------------
# ref:https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
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

# count of respondents by country
# ref: https://www.programmingr.com/count-occurrences-in-column/ 
represented <- as.data.frame(table(completesubmission$D2))
write.csv(represented, file = "Data/represented.csv", row.names = FALSE)
  # change column names
  # ref: https://www.geeksforgeeks.org/change-column-name-of-a-given-dataframe-in-r/ 
colnames(represented) <- c("country", "count")

ggplot(completesubmission) +
  geom_bar(mapping = aes(x = reorder(D2, D2, function(x) + length(x)))) +
  labs(x = "Country/ Region", 
       y = "Count",
       title = "Geographical distribution of participants") +
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
comparison <- read_csv("Data/comparison1.csv")

represented <- filter(comparison, !count == 0)
write.csv(represented, file = "Data/represented.csv", row.names = FALSE)

ggplot(represented) + 
  geom_bar(mapping = aes(x = reorder(country, count), y = (count)/sum(count), alpha = 1/3), fill = "red", stat = "identity") + 
  geom_bar(mapping = aes(x = reorder(country, count), y = (user)/sum(user), alpha = 1/3), fill = "blue", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Country/ Region", 
       y = "Percentage", 
       caption = "Data source: CERN Annual Report",
       title = "Comparison of Geographical distribution of \n survey participants and CERN user") +
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



par(mfrow=c(2,3),
    mar = c(10,5,5,5))
for (i in 10:14) {
  plot(completesubmission[,i], main=colnames(completesubmission)[i],
       ylab = "Count", col="steelblue", las = 2)
}

par(mfrow=c(2,3),
    mar = c(10,5,5,5))
plot(completesubmission$p2q1_OS1, main="OS benefits HEP",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p2q1_OS2, main="OS benefits researchers",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p2q1_OS3, main="OS benefits\n the general public",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p2q2_OA1, main="OA changed how I read",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p2q2_OA2, main="OA changed how I submit",
     ylab = "Count", col="steelblue", las = 2)


# Open Data -----------------------------------------------------------------------------------------------------------------------------------------------
# Experimentlaists' experience with Open Data
ODE <- filter(completesubmission, D4 == "Experiment")
ODE <- select(ODE, 15:19)

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
ODE2 <- reshape2::melt(ODE_sum, id.vars = "Yes/No")
ODE2 <- 
  ODE2 %>% 
  mutate("Yes/No" = as.character("Yes/No")) %>% 
  mutate("Yes/No" = replace("Yes/No", "Yes/No" == "1", "Yes")) %>% 
  mutate("Yes/No" = replace("Yes/No", "Yes/No" == "2", "No"))


ODE_sum$levels <- seq_len(nrow(ODE_sum))
ODE2 <- reshape2::melt(ODE_sum, id.vars = "levels")
ODE2 <- 
  ODE2 %>% 
  mutate(levels = as.character(levels)) %>% 
  mutate(levels = replace(levels, levels == "1", "Yes")) %>% 
  mutate(levels = replace(levels, levels == "2", "No")) 

ggplot(ODE2, aes(x = variable, y = value, fill = levels)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Count",
       title = "Experimentalists' experience with Open Data") +
  scale_fill_discrete(NULL) +
  scale_fill_manual(values = c("Yes" = "grey", "No" = alpha(NA))) +
  coord_flip() 

# Theorists' experiences with Open Data
ODT <- filter(completesubmission, D4 == "Theory")
ODT <- select(ODT, 20:25)

summary(ODT)
summary <- summary(ODT)
ODT_summary <- do.call(cbind, lapply(ODT, summary))
ODT_sum <- as_tibble(ODT_summary, rownames("levels"))
names(ODT_sum)[names(ODT_sum) == "p3q1T_OD1"] <- "read/heard about OD" 
names(ODT_sum)[names(ODT_sum) == "p3q1T_OD2"] <- "tried to find OD" 
names(ODT_sum)[names(ODT_sum) == "p3q1T_OD3"] <- "used TH OD" 
names(ODT_sum)[names(ODT_sum) == "p3q1T_OD4"] <- "use EX OD" 
names(ODT_sum)[names(ODT_sum) == "p3q1T_OD5"] <- "openly released OD"
names(ODT_sum)[names(ODT_sum) == "p3q1T_OD6"] <- "never interacted with OD" 


ODT_sum$levels <- seq_len(nrow(ODT_sum))
ODT2 <- reshape2::melt(ODT_sum, id.vars = "Yes/No")
ODT2 <- 
  ODT2 %>% 
  mutate("Yes/No" = as.character("Yes/No")) %>% 
  mutate("Yes/No" = replace("Yes/No", "Yes/No" == "1", "Yes")) %>% 
  mutate("Yes/No" = replace("Yes/No", "Yes/No" == "2", "No"))


ODT_sum$levels <- seq_len(nrow(ODT_sum))
ODT2 <- reshape2::melt(ODT_sum, id.vars = "levels")
ODT2 <- 
  ODT2 %>% 
  mutate(levels = as.character(levels)) %>% 
  mutate(levels = replace(levels, levels == "1", "Yes")) %>% 
  mutate(levels = replace(levels, levels == "2", "No")) 

ggplot(ODT2, aes(x = variable, y = value, fill = levels)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Count",
       title = "Theorists' experience with Open Data") +
  scale_fill_discrete(NULL) +
  scale_fill_manual(values = c("Yes" = "grey", "No" = alpha(NA))) +
  coord_flip() 

# Open Data attitude EX
OD2E <- filter(completesubmission, D4 == "Experiment")
OD2E <- select(OD2E, 26:29)

par(mfrow=c(2,2),
    mar = c(8,4,3,2))
plot(completesubmission$p3q2E_OD_E1, main="Data used in published \n studies should be made open",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p3q2E_OD_E2, main="Data should be accessible\n for anyone interested",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p3q2E_OD_E3, main="Open and easy access to data\n is helpful to me",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p3q2E_OD_E4, main="Open Data does not \n affect how I work",
     ylab = "Count", col="steelblue", las = 2)

# Open Data attitude TH
par(mfrow=c(2,3),
    mar = c(8,4,3,4))
plot(completesubmission$p3q2T_OD_E1, main="Data used in published \n studies should be made open",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p3q2T_OD_E2, main="Data should be accessible\n for anyone interested",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p3q2T_OD_E3, main="Open and easy access to\n theory data is helpful to me",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p3q2T_OD_E4, main="Open and easy access to exp \n data is helpful to me",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p3q2T_OD_E5, main="Open Data does not \n affect how I work",
     ylab = "Count", col="steelblue", las = 2)


# when to share TH

shareT <- filter(completesubmission, D4 == "Theory")
shareT <- select(shareT, 39:45)

summary(shareT)
summary <- summary(shareT)
shareT_summary <- do.call(cbind, lapply(shareT, summary))
shareT_sum <- as_tibble(shareT_summary, rownames("levels"))
names(shareT_sum)[names(shareT_sum) == "p4q1T_1"] <- "research complete" 
names(shareT_sum)[names(shareT_sum) == "p4q1T_2"] <- "arxiv submission" 
names(shareT_sum)[names(shareT_sum) == "p4q1T_3"] <- "journal submission" 
names(shareT_sum)[names(shareT_sum) == "p4q1T_4"] <- "journal acceptance" 
names(shareT_sum)[names(shareT_sum) == "p4q1T_5"] <- "journal publication" 
names(shareT_sum)[names(shareT_sum) == "p4q1T_6"] <- "After publication" 
names(shareT_sum)[names(shareT_sum) == "p4q1T_7"] <- "never" 


shareT_sum$levels <- seq_len(nrow(shareT_sum))
shareT2 <- reshape2::melt(shareT_sum, id.vars = "levels")
shareT2 <- 
  shareT2 %>% 
  mutate(levels = as.character(levels)) %>% 
  mutate(levels = replace(levels, levels == "1", "Yes")) %>% 
  mutate(levels = replace(levels, levels == "2", "No")) 

ggplot(shareT2, aes(x = variable, y = value, fill = levels)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Count",
       title = "Theorists' preferred data sharing time") +
  scale_fill_discrete(NULL) +
  scale_fill_manual(values = c("Yes" = "grey", "No" = alpha(NA))) +
  coord_flip() 

# when to share EX
shareE <- filter(completesubmission, D4 == "Experiment")
shareE <- select(shareE, 44:49)

summary(shareE)
summary <- summary(shareE)
shareE_summary <- do.call(cbind, lapply(shareE, summary))
shareE_sum <- as_tibble(shareE_summary, rownames("levels"))
names(shareE_sum)[names(shareE_sum) == "p4q1E_1"] <- "research complete" 
names(shareE_sum)[names(shareE_sum) == "p4q1E_2"] <- "arxiv submission" 
names(shareE_sum)[names(shareE_sum) == "p4q1E_3"] <- "journal acceptance" 
names(shareE_sum)[names(shareE_sum) == "p4q1E_4"] <- "journal publication" 
names(shareE_sum)[names(shareE_sum) == "p4q1E_5"] <- "after publication" 
names(shareE_sum)[names(shareE_sum) == "p4q1E_6"] <- "never" 


shareE_sum$levels <- seq_len(nrow(shareE_sum))
shareE2 <- reshape2::melt(shareE_sum, id.vars = "levels")
shareE2 <- 
  shareE2 %>% 
  mutate(levels = as.character(levels)) %>% 
  mutate(levels = replace(levels, levels == "1", "Yes")) %>% 
  mutate(levels = replace(levels, levels == "2", "No")) 

ggplot(shareE2, aes(x = variable, y = value, fill = levels)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Count",
       title = "Experimentalists' preferred data sharing time") +
  scale_fill_discrete(NULL) +
  scale_fill_manual(values = c("Yes" = "grey", "No" = alpha(NA))) +
  coord_flip() 

ggsave("ex_sharing_time.tiff", width = 6, height = 4)

# all freetext comments (output gathered in external freetext_comments.md file)
when_other <- filter(completesubmission, !is.na(p4q1T_O))
when_other <- select(when_other, p4q1T_O)

when_otherE <- filter(completesubmission, !is.na(p4q1E_O))
when_otherE <- select(when_otherE, p4q1E_O)

when_other <- filter(completesubmission, !is.na(p4q2_O))
when_other <- select(when_other, p4q2_O)

when_other <- filter(completesubmission, !is.na(p5q1_N))
when_other <- select(when_other, p5q1_N)

when_other <- filter(completesubmission, !is.na(p7q2))
when_other <- select(when_other, p7q2)


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


who_ex <- filter(whodecide, D4=="Experiment")


ex.pct <- who_ex %>% group_by(p4q2) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count))

who_th <- filter(whodecide, D4=="Theory")
th.pct <- who_th %>% group_by(p4q2) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count))

whodecide.pct <- whodecide %>% group_by(p4q2) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count))


ggplot(whodecide) + 
  geom_bar(mapping = aes(x = reorder(p4q2, p4q2, function(x) + length(x)), fill = D4)) +
  labs(x = NULL, y = "Count",
       title = "Who should decide the accessibility \n of data and code")+
  scale_fill_discrete(name = "Field") +
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


# when to share EX
shareExp <- select(completesubmission, 58:61)

summary(shareExp)
summary <- summary(shareExp)
shareExp_summary <- do.call(cbind, lapply(shareExp, summary))
shareExp_sum <- as_tibble(shareExp_summary, rownames("levels"))

names(shareExp_sum)[names(shareExp_sum) == "p5q1_SQ002"] <- "asked others to \n share with me" 
names(shareExp_sum)[names(shareExp_sum) == "p5q1_SQ001"] <- "shared with others before" 
names(shareExp_sum)[names(shareExp_sum) == "p5q1_SQ004"] <- "never considered sharing" 
names(shareExp_sum)[names(shareExp_sum) == "p5q1_SQ005"] <- "no interest in sharing" 


shareExp_sum$levels <- seq_len(nrow(shareExp_sum))
shareExp2 <- reshape2::melt(shareExp_sum, id.vars = "levels")
shareExp2 <- 
  shareExp2 %>% 
  mutate(levels = as.character(levels)) %>% 
  mutate(levels = replace(levels, levels == "1", "Yes")) %>% 
  mutate(levels = replace(levels, levels == "2", "No")) 

ggplot(shareExp2, aes(x = variable, y = value, fill = levels)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Count",
       title = "Experience with data sharing") +
  scale_fill_manual(values = c("Yes" = "grey", "No" = alpha(NA)), name = " ") +
  coord_flip() 




# single factor
# plotting with ggplot and cowplot

Factor1 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_1)) +
  labs(x = "Additional work", 
       y = "Count") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

Factor2 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_2)) +
  labs(x = "Have rights", 
       y = "Count") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

Factor3 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_3)) +
  labs(x = "Competitiveness", 
       y = "Count")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

Factor4 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_4)) +
  labs(x = "Quality of data and code", 
       y = "Count")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

Factor5 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_5)) +
  labs(x = "Essential for reproducibility", 
       y = "Count")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

Factor6 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_6)) +
  labs(x = "usefulness for others", 
       y = "Count")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

Factor7 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_7)) +
  labs(x = "Request from others", 
       y = "Count")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

Factor8 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_8)) +
  labs(x = "Responsible reuse", 
       y = "Count")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

Factor9 <- ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_9)) +
  labs(x = "Mandates", 
       y = "Count")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

plot_grid(Factor1, Factor2, Factor3, Factor4, Factor5, Factor6, Factor7, Factor8, Factor9)

# plotting the same data with base R and mfrow

par(mfrow=c(3,3),
    mar = c(10,5,5,5))
plot(completesubmission$p5q2_1, main="Additional work",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p5q2_2, main="Having rights",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p5q2_3, main="Competitiveness",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p5q2_4, main="Quality of data and code",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p5q2_5, main="Essential for reproducibility",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p5q2_6, main="Usefulness for others",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p5q2_7, main="Request from others",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p5q2_8, main="Responsible reuse",
     ylab = "Count", col="steelblue", las = 2)
plot(completesubmission$p5q2_9, main="Mandates",
     ylab = "Count", col="steelblue", las = 2)

# an attempt to do some statistical tests
#convert factor to numeric
which(colnames(completesubmission)=="p5q2_1" )

Factor <- select(completesubmission, 63:71)
write.csv(Factor, file = "Factor.csv")

Factor <-
  Factor %>% 
  mutate(p5q2_1 = as.numeric(p5q2_1)) %>% 
  mutate(p5q2_1 = replace(p5q2_1, p5q2_1 == "Somewhat affect", 1)) %>% 
  mutate(p5q2_1 = replace(p5q2_1, p5q2_1 == "Affect a lot", 2)) %>% 
  mutate(p5q2_1 = replace(p5q2_1, p5q2_1 == "Neutral", 0)) %>% 
  mutate(p5q2_1 = replace(p5q2_1, p5q2_1 == "Rarely affect", -1)) %>% 
  mutate(p5q2_1 = replace(p5q2_1, p5q2_1 == "Not at all affect", -2)) %>% 
  
  mutate(p5q2_2 = as.numeric(p5q2_2)) %>% 
  mutate(p5q2_2 = replace(p5q2_2, p5q2_2 == "Somewhat affect", 1)) %>% 
  mutate(p5q2_2 = replace(p5q2_2, p5q2_2 == "Affect a lot", 2)) %>% 
  mutate(p5q2_2 = replace(p5q2_2, p5q2_2 == "Neutral", 0)) %>% 
  mutate(p5q2_2 = replace(p5q2_2, p5q2_2 == "Rarely affect", -1)) %>% 
  mutate(p5q2_2 = replace(p5q2_2, p5q2_2 == "Not at all affect", -2)) %>% 
  
  mutate(p5q2_3 = as.numeric(p5q2_3)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Somewhat affect", 1)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Affect a lot", 2)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Neutral", 0)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Rarely affect", -1)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Not at all affect", -2))

  mutate(p5q2_3 = as.numeric(p5q2_3)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Somewhat affect", 1)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Affect a lot", 2)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Neutral", 0)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Rarely affect", -1)) %>% 
  mutate(p5q2_3 = replace(p5q2_3, p5q2_3 == "Not at all affect", -2))
  
  
# ========Documentation and Peer Review========

# comment about documentation for review
comment <- filter(completesubmission,!is.na(p7q2))
comment 
tibble <- summary
# ========Tools========