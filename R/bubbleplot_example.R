library(tidyquant)
library(tidyverse)
#pivot table 
# https://www.youtube.com/watch?v=YWByrQIx-uQ&t=175s 


anovatest <- completesubmission %>% select(D1, D2, D3, D4, p5q2_1)
pivot <- 
  anovatest %>% 
  group_by(p5q2_1) %>% 
  count(D3)

ggplot(pivot, aes(x=D3, y=p5q2_1, size=n)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.5, 25), name = "Frequency")


#ucalculate mean by group
# https://www.statology.org/r-mean-by-group/
cs_unf<-
  completesubmission

cs_unf$p2q1_OS1<-as.numeric(cs_unf$p2q1_OS1)
cs_unf %>% group_by(D3) %>% summarise_at(vars(p2q1_OS1), list(name = mean))

cs_unf$p2q1_OS2<-as.numeric(cs_unf$p2q1_OS2)
cs_unf %>% group_by(D3) %>% summarise_at(vars(p2q1_OS2), list(name = mean))

cs_unf$p2q1_OS3<-as.numeric(cs_unf$p2q1_OS3)
cs_unf %>% group_by(D3) %>% summarise_at(vars(p2q1_OS3), list(name = mean))


OS<-cs_unf %>% group_by(D3) %>% summarise_at(vars(p2q1_OS1, p2q1_OS2, p2q1_OS3), list(name = mean))

# calculate mean for satisfaction of research tools (had to exclude level 6 no experience)
cs_unf$p8q2_1	<-as.numeric(cs_unf$p8q2_1)
cs_unf$p8q2_2	<-as.numeric(cs_unf$p8q2_2)
cs_unf$p8q2_3	<-as.numeric(cs_unf$p8q2_3)
cs_unf$p8q2_4	<-as.numeric(cs_unf$p8q2_4)
cs_unf$p8q2_5	<-as.numeric(cs_unf$p8q2_5)
cs_unf$p8q2_6	<-as.numeric(cs_unf$p8q2_6)
cs_unf$p8q2_7	<-as.numeric(cs_unf$p8q2_7)
mean(cs_unf$p8q2_1[which(cs_unf$p8q2_1!=6)])
mean(cs_unf$p8q2_2[which(cs_unf$p8q2_2!=6)])
mean(cs_unf$p8q2_3[which(cs_unf$p8q2_3!=6)])
mean(cs_unf$p8q2_4[which(cs_unf$p8q2_4!=6)])
mean(cs_unf$p8q2_5[which(cs_unf$p8q2_5!=6)])
mean(cs_unf$p8q2_6[which(cs_unf$p8q2_6!=6)])
mean(cs_unf$p8q2_7[which(cs_unf$p8q2_7!=6)])
Satisfaction<-c(mean(cs_unf$p8q2_1[which(cs_unf$p8q2_1!=6)]),
        mean(cs_unf$p8q2_2[which(cs_unf$p8q2_2!=6)]),
        mean(cs_unf$p8q2_3[which(cs_unf$p8q2_3!=6)]),
        mean(cs_unf$p8q2_4[which(cs_unf$p8q2_4!=6)]),
        mean(cs_unf$p8q2_5[which(cs_unf$p8q2_5!=6)]),
        mean(cs_unf$p8q2_6[which(cs_unf$p8q2_6!=6)]),
        mean(cs_unf$p8q2_7[which(cs_unf$p8q2_7!=6)]))
p8q2<-c("p8q2_1", "p8q2_2", "p8q2_3", "p8q2_4", "p8q2_5", "p8q2_6", "p8q2_7")



p8q2sat<-tibble(p8q2, Satisfaction)
p8q2sat %>% 
  ggplot(aes(p8q2, Satisfaction, group = 1))+
  geom_line()+
  ylim(0,5)

#calculate mean for tool adoption impact factors
cs_unf$p8q3_1	<-as.numeric(cs_unf$p8q3_1)
cs_unf$p8q3_2	<-as.numeric(cs_unf$p8q3_2)
cs_unf$p8q3_3	<-as.numeric(cs_unf$p8q3_3)
cs_unf$p8q3_4	<-as.numeric(cs_unf$p8q3_4)
impact_factor<-c(mean(cs_unf$p8q3_1),
                 mean(cs_unf$p8q3_2),
                 mean(cs_unf$p8q3_3),
                 mean(cs_unf$p8q3_4))
impact_factor

sat<-completesubmission %>%  select(D1:D4, p8q2_1:p8q2_7)



ggplot(cs_unf, aes(x=factor(D4), y=p8q2_1))+
  geom_line(stat = "summary", fun = "mean", group=1)



OS %>% 
  ggplot(aes(D3, p2q1_OS1_name)) +
    geom_point()


cs_unf$p5q2_1<-as.numeric(cs_unf$p5q2_1)
addwork<-cs_unf %>% group_by(D3) %>% summarise_at(vars(p5q2_1), list(name = mean))


#impact of "additional work" on sharing decision by seniority
ggplot() +
  geom_count(data = completesubmission, 
             aes(x=D3, y=p5q2_1),
             shape = 22,
             fill = "black",
             alpha=0.5)+
  scale_size(range = c(.5, 25), name = "Frequency")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "Additional work", x = "", y = "")+
  geom_line(data = addwork, 
             aes(x=D3, y=name, group =1),
             color = "blue")

#impact of "additional work" on sharing decision by seniority
ggplot(cs_unf,aes(x=D3, y=p5q2_1))+
  geom_count(shape = 22,
             fill = "black",
             alpha=0.5)+
  scale_size(range = c(.5, 25), name = "Frequency")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = )+
  theme(legend.position = "none")+
  labs(title = "Additional work", x = "", y = "")+
  geom_line(stat = "summary", fun = "mean", group = 1,
            color = "blue")

#impact of "additional work" on sharing decision by field
addworkbyfield<-cs_unf %>% group_by(D4) %>% summarise_at(vars(p5q2_1), list(name = mean))
ggplot() +
  geom_count(data = completesubmission, 
             aes(x=D4, y=p5q2_1),
             shape = 22,
             fill = "black",
             alpha=0.5)+
  scale_size(range = c(.5, 25), name = "Frequency")+
 # theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "Additional work", x = "", y = "")+
  geom_line(data = addworkbyfield, 
             aes(x=D4, y=name, group=1),
             color = "blue")

#impact of "additional work" on sharing decision by gender
addworkbygender<-cs_unf %>% group_by(D1) %>% summarise_at(vars(p5q2_1), list(name = mean))
ggplot() +
  geom_count(data = completesubmission, 
             aes(x=D1, y=p5q2_1),
             shape = 22,
             fill = "black",
             alpha=0.5)+
  scale_size(range = c(.5, 25), name = "Frequency")+
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "Additional work", x = "", y = "")+
  geom_line(data = addworkbygender, 
             aes(x=D1, y=name, group=1),
             color = "blue")


base<- ggplot(addworkbygender, aes(D1, name, group=1))
base + geom_path()

#https://ggplot2.tidyverse.org/reference/geom_count.html
cs_unf$p5q2_1<-as.numeric(cs_unf$p5q2_1)


ggplot(completesubmission, aes(D3, p5q2_1)) +
  geom_count(alpha=0.5) +
  scale_size(range = c(.5, 25), name = "Frequency")

ggplot(completesubmission, aes(D1, p5q2_1)) +
  geom_count(alpha=0.5) +
  scale_size(range = c(.5, 25), name = "Frequency")

ggplot(completesubmission, aes(D4, p5q2_1)) +
  geom_count(alpha=0.5) +
  scale_size(range = c(.5, 25), name = "Frequency")

# to overlay mean on bubble chart
# switch factor levles to numeric
# https://stackoverflow.com/questions/34059017/replace-factors-with-a-numeric-value 
levels(pivot)<-1:5

pivotnew<-ddply(pivot, c("p5q2_1", "D3"), summarise,
                mean = mean(n),
                sd   = sd(n))
pivotnew


# testing mean calculation for factors
p6q1_1<-completesubmission %>% select(D3, p6q1_1)
p6q1_1$p6q1_1 <-as.numeric(p6q1_1$p6q1_1)
mean(p6q1_1$p6q1_1)

levels(p6q1_1$p6q1_1)<-1:5
r1<-with(p6q1_1, tapply(value, factor, mean))

str(p6q1_1)
A<-mean(p6q1_1$p6q1_1[p6q1_1$D3 == "less than 5 years"])

anovatest1<- as.numeric()