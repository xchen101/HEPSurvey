summary(completesubmission$p5q2_1)

ggplot(completesubmission) +
  geom_bar(mapping = aes(x = p5q2_1)) +
  labs(x = "Additional work", 
       y = "Count")

ggplot(completesubmission, aes(p5q2_1)) +
  geom_bar()
# =============================
factor <- select(completesubmission, D1:D4, p5q2_1:p5q2_9)
summary(factor)

level1 <- factor %>% count(p5q2_1)
level1$f <- "f1"
names(level1)[names(level1) == "p5q2_1"] <- "level"

level2 <- factor %>% count(p5q2_2)
level2$f <- "f2"
names(level2)[names(level2) == "p5q2_2"] <- "level"

level3 <- factor %>% count(p5q2_3)
level3$f <- "f3"
names(level3)[names(level3) == "p5q2_3"] <- "level"

rbind(level1, level2, level3)

spread(factor, key = "p5q2_1", value = count)


# =============================
summary <- summary(factor)
summary <- tibble(summary)
# =============================
a <- matrix(1:30, 5, 6)
ta <- t(a) ##-- i.e.,  a[i, j] == ta[j, i] for completesubmission i,j :
for(j in seq(ncol(a)))
  if(! completesubmission(a[, j] == ta[j, ])) stop("wrong transpose")
# =============================
# count the number of "affect a lot" in one column (1st factor)
length(factor[factor[,5] == "Affect a lot", 1])

# factor --------------------------------------------------------------------------------------------------------------------------------------------------
summary(factor)
summary <- summary(factor)
tidy(summary)
factor_summary <- do.call(cbind, lapply(factor, summary))
factor_sum <- as_tibble(factor_summary, rownames("levels"))
names(factor_sum)[names(factor_sum) == "p5q2_1"] <- "Additional work" 
names(factor_sum)[names(factor_sum) == "p5q2_2"] <- "Rights" 
names(factor_sum)[names(factor_sum) == "p5q2_3"] <- "Competition" 
names(factor_sum)[names(factor_sum) == "p5q2_4"] <- "Quality"
names(factor_sum)[names(factor_sum) == "p5q2_5"] <- "Needed for reproduction"
names(factor_sum)[names(factor_sum) == "p5q2_6"] <- "Useful"
names(factor_sum)[names(factor_sum) == "p5q2_7"] <- "Requested"
names(factor_sum)[names(factor_sum) == "p5q2_8"] <- "Responsible user"
names(factor_sum)[names(factor_sum) == "p5q2_9"] <- "Mandates"



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



levs <- unique(unlist(lapply(factor_summary, levels)))

barplot(as.matrix(factor_sum))

# count the number of "affect a lot" in completesubmission columns
AAL <- vector()
for(i in 5:ncol (factor)){
  cols[i] <- length(factor[factor[,i] == "Affect a lot", i])
}

cols

factor %>% 
  gather()


# count the number of "somewhat affect" in completesubmission columns



count_aal <- function(x) {
  
}


SA <- vector()
for(i in 1:ncol (factor)){
  cols[i] <- length(factor[factor[,i] == "Somewhat affect", i])
}


# factors

aff_levels <- c(
  "Affect a lot", "Somewhat affect", "Neutral", "Rarely affect", "Not at completesubmission affect")

pref_levels <- c(
  "Strongly agree", "Somewhat agree", "Neutral", "Somewhat disagree", "Strongly disagree"
)

completesubmission %>% 
  mutate(D3 = fct_recode(D3, 
         "Junior" = "less than 5 years",
         "Junior" = "5 - 10 years",
         "Junior" = "11 - 15 years",
         "Senior" = "16 - 20 years",
         "Senior" = "more than 20 years")) %>% 
  count(D3)


# cluster -------------------------------------------------------------------------------------------------------------------------------------------------

# **************************************************************#
# CERN open access survey
# 
# Xiaoli Chen (11/12/2018)
# **************************************************************#

# **************************************************************#
# With the a data set with 607 obs., 58 variables:
# 
#[1] "id"                      "Gender"                  "Country"                
#[4] "D3"               "Field"                   "p2q1_OS1"      
#[7] "OS_BeneIndividual"       "OS_BenePublic"           "OA_ChSeeking"           
#[10] "OA_ChSub"                "OD_aware"                "OD_seeking"             
#[13] "OD_using"                "OD_sharing"              "OD_none"                
#[16] "OD_publish"              "OD_anyone"               "OD_helpful"             
#[19] "OD_dontaffect"           "Who_decide"              "Sharing_asked"          
#[22] "Sharing_shared"          "Sharing_never"           "Sharing_not_interested" 
#[25] "Sharing_addedwork"       "Sharing_rights"          "Sharing_competition"    
#[28] "Sharing_quality"         "Sharing_essential"       "Sharing_selfeval"       
#[31] "Sharing_request"         "Sharing_responsiblyused" "Sharing_required"       
#[34] "Doc_askpeople"           "Doc_reproduc"            "Doc_unproductive"       
#[37] "Doc_unuseful"            "Rev_coderev"             "Rev_qualitynotaffect"   
#[40] "Rev_doc"                 "Rev_docreadywhenpublish" "Tool_codemanage"        
#[43] "Tool_issuetracker"       "Tool_docmanage"          "Tool_coediting"         
#[46] "Tool_eventmanage"        "Tool_notebook"           "Happy_codemanage"       
#[49] "Happy_issuetracker"      "Happy_docmanage"         "Happy_coediting"        
#[52] "Happy_refmanage"         "Happy_eventmanage"       "Happy_notebook"         
#[55] "Tool_endorsement"        "Tool_efficient"          "Tool_easy"              
#[58] "Tool_peers" 
# **************************************************************#

library(ggplot2)

completesubmission<-read.csv("HEPSurvey/Data/Processed/Coding_Scheme/completesubmission.csv", header=TRUE)
TH<-read.csv("HEPSurvey/Data/Processed/Coding_Scheme/TH.csv", header=TRUE)
EX<-read.csv("HEPSurvey/Data/Processed/Coding_Scheme/EX.csv", header=TRUE)
Other<-read.csv("HEPSurvey/Data/Processed/Coding_Scheme/Other.csv", header=TRUE)
Time<-read.csv("HEPSurvey/Data/Processed/Coding_Scheme/Time.csv", header=TRUE)
Comments<-read.csv("HEPSurvey/Data/Processed/Coding_Scheme/Comments.csv", header=TRUE)
str(completesubmission)

summary(completesubmission)

## Question Group 1 ###

summary(completesubmission$p3q1E_OD1)

SeBD<-table(completesubmission$D3, completesubmission$p2q1_OS1)
round(prop.table(SeBD,1), 2) # crosstab as proportion
fisher.test(twoway.crosstab) # Do fisher'exact test Ho: no relationship
chisq.test(SeBD) # p-value = 0.4462 

SeBI<-table(completesubmission$D3, completesubmission$OS_BeneIndividual)
round(prop.table(SeBI,1), 2) # crosstab as proportion
#fisher.test(twoway.crosstab) # Do fisher'exact test Ho: no relationship
chisq.test(SeBI) # p-value = 0.0148 

SeBP<-table(completesubmission$D3, completesubmission$OS_BenePublic)
round(prop.table(SeBP,1), 2) # crosstab as proportion
#fisher.test(twoway.crosstab) # Do fisher'exact test Ho: no relationship
chisq.test(SeBP) # p-value = 0.04994 


par(mfrow=c(1,3))
mosaicplot(SeBD)
mosaicplot(SeBI)
mosaicplot(SeBP)

#### set p-value threashold at 0.01#

## Question Group 2 ###
#plot summary of QG2#
par(mfrow=c(2,3))
for (i in 10:14) {
  plot(completesubmission[,i], main=colnames(completesubmission)[i],
       ylab = "Count", col="steelblue", las = 2)
} # need to re-order the labels


dd <- daisy(flower)
round(as.matrix(dd)[1:3, 1:3], 2)

dd <- daisy(factor)
round(as.matrix(dd)[5:13, 5:13], 2)
# Association between D3 and preception of OS and OA#
CrossTable(completesubmission$D3, completesubmission$p2q1_OS1, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q1_OS2, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q1_OS3, chisq=TRUE)            
CrossTable(completesubmission$D3, completesubmission$p2q2_OA1, chisq=TRUE)    
CrossTable(completesubmission$D3, completesubmission$p2q2_OA2, chisq=TRUE) 


#crosstable#
SeOA2 <- CrossTable(completesubmission$OA_ChSub, completesubmission$D3, chisq=TRUE) #0.0002110256
mosaic(SeOA2$t, shade=TRUE,
       legend=TRUE,
       labeling_args=list(gp_labels=gpar(fontsize=8),
                          gp_varnames=gpar(fontsize=12),
                          direction = "v", rot_labels=c(0, 0, 0, 0)), 
       legend_args=list(fontsize=8))

mosaic(SeOA2$t, shade = TRUE,
       legend = TRUE)

#threeway crosstable#
threeway.crosstab<-xtabs(~D3+Field+OA_ChSub, data=completesubmission)
mosaic(threeway.crosstab, shade=TRUE, 
       legend=TRUE,
       labeling_args=list(gp_labels=gpar(fontsize=8),
                          gp_varnames=gpar(fontsize=12),
                          direction = "v", rot_labels=c(0,0,90,0)), 
       legend_args=list(fontsize=8))

assoc(threeway.crosstab, shade=TRUE,
      legend=TRUE,
      labeling_args=list(gp_labels=gpar(fontsize=8),
                         gp_varnames=gpar(fontsize=12),
                         direction = "v", rot_labels=c(0,0,90,0)), 
      legend_args=list(fontsize=8))

# fourway crosstable#
fourway.crosstab<-xtabs(~D3+Field+OA_ChSub+Who_decide, data=completesubmission)
mosaic(fourway.crosstab, shade=TRUE, 
       legend=TRUE,
       labeling_args=list(gp_labels=gpar(fontsize=8),
                          gp_varnames=gpar(fontsize=12),
                          direction = "v", rot_labels=c(0,0,90,0)), 
       legend_args=list(fontsize=8))

assoc(fourway.crosstab, shade=TRUE,
      legend=TRUE,
      labeling_args=list(gp_labels=gpar(fontsize=8),
                         gp_varnames=gpar(fontsize=12),
                         direction = "v", rot_labels=c(0,0,90,0)), 
      legend_args=list(fontsize=8))

# Association between Gender and preception of OS and OA#
CrossTable(completesubmission$Gender, completesubmission$p2q1_OS1, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$OS_BeneIndividual, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$OS_BenePublic, chisq=TRUE)            
CrossTable(completesubmission$Gender, completesubmission$OA_ChSeeking, chisq=TRUE)    
CrossTable(completesubmission$Gender, completesubmission$OA_ChSub, chisq=TRUE) 
# Association between Field and preception of OS and OA#
CrossTable(completesubmission$Field, completesubmission$p2q1_OS1, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$OS_BeneIndividual, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$OS_BenePublic, chisq=TRUE)            
CrossTable(completesubmission$Field, completesubmission$OA_ChSeeking, chisq=TRUE)    
CrossTable(completesubmission$Field, completesubmission$OA_ChSub, chisq=TRUE) 

## Question Group 3 ###

par(mfrow=c(3,3))
for (i in 11:19) {
  plot(completesubmission[,i], main=colnames(completesubmission)[i],
       ylab = "Count", col="steelblue", las = 2)
}
# Association between D3 and preception of Open Data #
CrossTable(completesubmission$D3, completesubmission$OD_aware, chisq=TRUE) #0.002556045
CrossTable(completesubmission$D3, completesubmission$OD_seeking, chisq=TRUE) # 6.112698e-06 
CrossTable(completesubmission$D3, completesubmission$OD_using, chisq=TRUE)  
CrossTable(completesubmission$D3, completesubmission$OD_sharing, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$OD_none, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$OD_publish, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$OD_anyone, chisq=TRUE) # 7.96744e-05 
CrossTable(completesubmission$D3, completesubmission$OD_helpful, chisq=TRUE) # 0.0001433485 
CrossTable(completesubmission$D3, completesubmission$OD_dontaffect, chisq=TRUE)

#plot siginificantly associated factors#
SeODAware<-CrossTable(completesubmission$D3, completesubmission$p3q1E_OD1, chisq=TRUE) #p =  0.0001664108 
SeODSeeking<-CrossTable(completesubmission$D3, completesubmission$OD_seeking, chisq=TRUE) # 6.112698e-06 
SeODAnyone<-CrossTable(completesubmission$D3, completesubmission$OD_anyone, chisq=TRUE) # 7.96744e-05 
SeODHelpful<-CrossTable(completesubmission$D3, completesubmission$OD_helpful, chisq=TRUE) # 0.0001433485 

par(mfrow=c(2,2))
mosaic(SeODAware$t, shade=TRUE,
       legend=TRUE,
       labeling_args=list(gp_labels=gpar(fontsize=8),
                          gp_varnames=gpar(fontsize=12),
                          direction = "v", rot_labels=c(0, 0, 0, 0)), 
       legend_args=list(fontsize=8))
mosaic(SeODSeeking$t, shade=TRUE,
       legend=TRUE,
       labeling_args=list(gp_labels=gpar(fontsize=8),
                          gp_varnames=gpar(fontsize=12),
                          direction = "v", rot_labels=c(0, 0, 0, 0)), 
       legend_args=list(fontsize=8))
mosaic(SeODAnyone$t, shade=TRUE,
       legend=TRUE,
       labeling_args=list(gp_labels=gpar(fontsize=8),
                          gp_varnames=gpar(fontsize=12),
                          direction = "v", rot_labels=c(0, 0, 0, 0)), 
       legend_args=list(fontsize=8))
mosaic(SeODHelpful$t, shade=TRUE,
       legend=TRUE,
       labeling_args=list(gp_labels=gpar(fontsize=8),
                          gp_varnames=gpar(fontsize=12),
                          direction = "v", rot_labels=c(0, 0, 0, 0)), 
       legend_args=list(fontsize=8))


# Association between Gender and preception of Open Data #
CrossTable(completesubmission$Gender, completesubmission$OD_aware, chisq=TRUE) #0.002556045
CrossTable(completesubmission$Gender, completesubmission$OD_seeking, chisq=TRUE) # 6.112698e-06 
CrossTable(completesubmission$Gender, completesubmission$OD_using, chisq=TRUE)  
CrossTable(completesubmission$Gender, completesubmission$OD_sharing, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$OD_none, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$OD_publish, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$OD_anyone, chisq=TRUE) # 7.96744e-05 
CrossTable(completesubmission$Gender, completesubmission$OD_helpful, chisq=TRUE) # 0.0001433485 
CrossTable(completesubmission$Gender, completesubmission$OD_dontaffect, chisq=TRUE)
# Association between Field and preception of Open Data #
CrossTable(completesubmission$Field, completesubmission$OD_aware, chisq=TRUE) #0.006871572
CrossTable(completesubmission$Field, completesubmission$OD_seeking, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$OD_using, chisq=TRUE) #0.002195724
CrossTable(completesubmission$Field, completesubmission$OD_sharing, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$OD_none, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$OD_publish, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$OD_anyone, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$OD_helpful, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$OD_dontaffect, chisq=TRUE)

## Question Group 4 ###
CrossTable(completesubmission$D3, completesubmission$Who_decide, chisq=TRUE)  
CrossTable(completesubmission$Field, completesubmission$Who_decide, chisq=TRUE) #1.103019e-05 
CrossTable(completesubmission$Gender, completesubmission$Who_decide, chisq=TRUE) #7.366215e-14 


FiWD<-CrossTable(completesubmission$Field, completesubmission$Who_decide, chisq=TRUE) #1.103019e-05 
GeWD<-CrossTable(completesubmission$Gender, completesubmission$Who_decide, chisq=TRUE) #7.366215e-14 

par(mfrow=c(1,2))
mosaic(FiWD$t)
mosaic(GeWD$t)

## Question Group 5 ###
# Association between D3 and Sharing#
CrossTable(completesubmission$D3, completesubmission$Sharing_asked, chisq=TRUE) #1.112415e-15 
CrossTable(completesubmission$D3, completesubmission$Sharing_shared, chisq=TRUE) #0.0008975142
CrossTable(completesubmission$D3, completesubmission$Sharing_never, chisq=TRUE) #7.100692e-06 
CrossTable(completesubmission$D3, completesubmission$Sharing_not_interested, chisq=TRUE) #0.002975741
CrossTable(completesubmission$D3, completesubmission$Sharing_addedwork, chisq=TRUE) #0.002353272
CrossTable(completesubmission$D3, completesubmission$Sharing_rights, chisq=TRUE) 
CrossTable(completesubmission$D3, completesubmission$Sharing_competition, chisq=TRUE) 
CrossTable(completesubmission$D3, completesubmission$Sharing_quality, chisq=TRUE) 
CrossTable(completesubmission$D3, completesubmission$Sharing_essential, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$Sharing_selfeval, chisq=TRUE) 
CrossTable(completesubmission$D3, completesubmission$Sharing_request, chisq=TRUE) #8.04581e-05 
CrossTable(completesubmission$D3, completesubmission$Sharing_responsiblyused, chisq=TRUE) #7.817173e-05 
CrossTable(completesubmission$D3, completesubmission$Sharing_required, chisq=TRUE)
# Association between Gender and Sharing#
CrossTable(completesubmission$Gender, completesubmission$Sharing_asked, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_shared, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_never, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_not_interested, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_addedwork, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$Sharing_rights, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_competition, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_quality, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_essential, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$Sharing_selfeval, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_request, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_responsiblyused, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Sharing_required, chisq=TRUE)
# Association between Field and Sharing#
CrossTable(completesubmission$Field, completesubmission$Sharing_asked, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_shared, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_never, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_not_interested, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$Sharing_addedwork, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_rights, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_competition, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_quality, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_essential, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$Sharing_selfeval, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_request, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$Sharing_responsiblyused, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Sharing_required, chisq=TRUE)


## Question Group 6 ###
# Association between D3 and Doc #
CrossTable(completesubmission$D3, completesubmission$Doc_askpeople, chisq=TRUE) #2.415622e-06 
CrossTable(completesubmission$D3, completesubmission$Doc_reproduc, chisq=TRUE) #0.0009086682
CrossTable(completesubmission$D3, completesubmission$Doc_unproductive, chisq=TRUE) 
CrossTable(completesubmission$D3, completesubmission$Doc_unuseful, chisq=TRUE) #0.0001965802
# Association between Gender and Doc #
CrossTable(completesubmission$Gender, completesubmission$Doc_askpeople, chisq=TRUE)  
CrossTable(completesubmission$Gender, completesubmission$Doc_reproduc, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Doc_unproductive, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Doc_unuseful, chisq=TRUE) 
# Association between Field and Doc #
CrossTable(completesubmission$D3, completesubmission$Doc_askpeople, chisq=TRUE) #2.415622e-06 
CrossTable(completesubmission$D3, completesubmission$Doc_reproduc, chisq=TRUE) #0.0009086682
CrossTable(completesubmission$D3, completesubmission$Doc_unproductive, chisq=TRUE) 
CrossTable(completesubmission$D3, completesubmission$Doc_unuseful, chisq=TRUE) #0.0001965802


## Question Group 7 ###
# Association between D3 and Rev #
CrossTable(completesubmission$D3, completesubmission$Rev_coderev, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$Rev_qualitynotaffect, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$Rev_doc, chisq=TRUE) 
CrossTable(completesubmission$D3, completesubmission$Rev_docreadywhenpublish, chisq=TRUE) 
# Association between Gender and Rev #
CrossTable(completesubmission$Gender, completesubmission$Rev_coderev, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$Rev_qualitynotaffect, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$Rev_doc, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Rev_docreadywhenpublish, chisq=TRUE) 
# Association between Field and Rev #
CrossTable(completesubmission$Field, completesubmission$Rev_coderev, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$Rev_qualitynotaffect, chisq=TRUE)
CrossTable(completesubmission$Field, completesubmission$Rev_doc, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Rev_docreadywhenpublish, chisq=TRUE) 

## Question Group 8 ###
# Association between D3 and Tools #
CrossTable(completesubmission$D3, completesubmission$Tool_codemanage, chisq=TRUE) #2.805176e-20  
CrossTable(completesubmission$D3, completesubmission$Tool_issuetracker, chisq=TRUE) #8.969365e-05 
CrossTable(completesubmission$D3, completesubmission$Tool_docmanage, chisq=TRUE) #2.970106e-05 
CrossTable(completesubmission$D3, completesubmission$Tool_coediting, chisq=TRUE) #3.925664e-05 
CrossTable(completesubmission$D3, completesubmission$Tool_eventmanage, chisq=TRUE) #0.0001546139 
CrossTable(completesubmission$D3, completesubmission$Tool_notebook, chisq=TRUE) #0.002434254
CrossTable(completesubmission$D3, completesubmission$Happy_codemanage, chisq=TRUE) #2.734117e-15 
CrossTable(completesubmission$D3, completesubmission$Happy_issuetracker, chisq=TRUE) #0.0003076005
CrossTable(completesubmission$D3, completesubmission$Happy_docmanage, chisq=TRUE) #3.406743e-06
CrossTable(completesubmission$D3, completesubmission$Happy_coediting, chisq=TRUE) #0.0003620329
CrossTable(completesubmission$D3, completesubmission$Happy_refmanage, chisq=TRUE) 
CrossTable(completesubmission$D3, completesubmission$Happy_eventmanage, chisq=TRUE) #0.001040327
CrossTable(completesubmission$D3, completesubmission$Happy_notebook, chisq=TRUE) #0.0002187721
CrossTable(completesubmission$D3, completesubmission$Tool_endorsement, chisq=TRUE) #0.003637316
CrossTable(completesubmission$D3, completesubmission$Tool_efficient, chisq=TRUE) #0.0001107294
CrossTable(completesubmission$D3, completesubmission$Tool_easy, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$Tool_peers, chisq=TRUE)
# Association between Gender and Tools #
CrossTable(completesubmission$Gender, completesubmission$Tool_codemanage, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$Tool_issuetracker, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Tool_docmanage, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Tool_coediting, chisq=TRUE) #0.001154497
CrossTable(completesubmission$Gender, completesubmission$Tool_eventmanage, chisq=TRUE) #0.001173765
CrossTable(completesubmission$Gender, completesubmission$Tool_notebook, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Happy_codemanage, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Happy_issuetracker, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Happy_docmanage, chisq=TRUE) #0.001594557
CrossTable(completesubmission$Gender, completesubmission$Happy_coediting, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Happy_refmanage, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Happy_eventmanage, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Happy_notebook, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Tool_endorsement, chisq=TRUE) 
CrossTable(completesubmission$Gender, completesubmission$Tool_efficient, chisq=TRUE)
CrossTable(completesubmission$Gender, completesubmission$Tool_easy, chisq=TRUE)
CrossTable(completesubmission$D3, completesubmission$Tool_peers, chisq=TRUE)
# Association between Field and Tools #
CrossTable(completesubmission$Field, completesubmission$Tool_codemanage, chisq=TRUE) #4.331991e-09 
CrossTable(completesubmission$Field, completesubmission$Tool_issuetracker, chisq=TRUE) #9.929069e-08 
CrossTable(completesubmission$Field, completesubmission$Tool_docmanage, chisq=TRUE) #1.159599e-22
CrossTable(completesubmission$Field, completesubmission$Tool_coediting, chisq=TRUE) #9.147885e-07 
CrossTable(completesubmission$Field, completesubmission$Tool_eventmanage, chisq=TRUE) #1.520287e-22 
CrossTable(completesubmission$Field, completesubmission$Tool_notebook, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Happy_codemanage, chisq=TRUE) #1.770299e-05 
CrossTable(completesubmission$Field, completesubmission$Happy_issuetracker, chisq=TRUE) #1.369582e-07 
CrossTable(completesubmission$Field, completesubmission$Happy_docmanage, chisq=TRUE) #2.197284e-13 
CrossTable(completesubmission$Field, completesubmission$Happy_coediting, chisq=TRUE) #0.0005236116 
CrossTable(completesubmission$Field, completesubmission$Happy_refmanage, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Happy_eventmanage, chisq=TRUE) #2.845146e-09 
CrossTable(completesubmission$Field, completesubmission$Happy_notebook, chisq=TRUE) #0.002458946 
CrossTable(completesubmission$Field, completesubmission$Tool_endorsement, chisq=TRUE) 
CrossTable(completesubmission$Field, completesubmission$Tool_efficient, chisq=TRUE) #1.115923e-07 
CrossTable(completesubmission$Field, completesubmission$Tool_easy, chisq=TRUE) #0.0007118696
CrossTable(completesubmission$Field, completesubmission$Tool_peers, chisq=TRUE)

# Next, compute multiple correspondance analysis (MCA)
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
library(FactoMineR)
library(factoextra)
library(dplyr)

# clusters in QG2 #
clusterQG2 <- completesubmission %>% select(Gender, D3, Field, p2q1_OS1, OS_BeneIndividual, OS_BenePublic, OA_ChSeeking, OA_ChSub)

res.mca <- MCA(clusterQG2, graph=FALSE)
print(res.mca)
library("factoextra")
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# Cos2 of variable categories on Dim.1
fviz_cos2(res.mca, choice = "var", axes = 1)
# Cos2 of variable categories on Dim.2
fviz_cos2(res.mca, choice = "var", axes = 2)

fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "D3", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = FALSE,
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "whoDecide", # color by groups 
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_ellipses(res.mca, 1:8, geom = "point")


