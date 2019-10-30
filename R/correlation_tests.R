#### set p-value threashold at 0.01#


# demography ----------------------------------------------------------------------------------------------------------------------------------------------
# Association between seniority and gender
CrossTable(completesubmission$D3, completesubmission$D1, chisq = TRUE)
#no correlation


# OS/OA attitude ------------------------------------------------------------------------------------------------------------------------------------------

# Association between seniority and preception of OS and OA#
CrossTable(completesubmission$D3, completesubmission$p2q1_OS1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q1_OS2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q1_OS3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q2_OA1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q2_OA2, chisq = TRUE)
# no correlation

# Association between field and preception of OS and OA#
CrossTable(completesubmission$D4, completesubmission$p2q1_OS1, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p2q1_OS2, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p2q1_OS3, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p2q2_OA1, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p2q2_OA2, chisq = TRUE)
# no correlation

# Association between gender and preception of OS and OA#
CrossTable(completesubmission$D1, completesubmission$p2q1_OS1, chisq = TRUE)
CrossTable(completesubmission$D1, completesubmission$p2q1_OS2, chisq = TRUE)
CrossTable(completesubmission$D1, completesubmission$p2q1_OS3, chisq = TRUE)
CrossTable(completesubmission$D1, completesubmission$p2q2_OA1, chisq = TRUE)
CrossTable(completesubmission$D1, completesubmission$p2q2_OA2, chisq = TRUE)
# no correlation


# interaction with OD -------------------------------------------------------------------------------------------------------------------------------------
# Association between experimentalists' seniority and OD practice#
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD1, chisq = TRUE) #p =  0.0001664108 the younger researchers are more likely to be aware of Open Data
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD2, chisq = TRUE) #p =  0.0002161671 the younger researchers are more likely to have tried to look for open data
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD4, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD5, chisq = TRUE)

SeOD1<-table(completesubmission$D3, completesubmission$p3q1E_OD1) 
SeOD1 <- round(prop.table(SeOD1,1), 2) # crosstab as proportion

corrplot(SeOD1, method = "color",
         addCoef.col = "black",
         tl.col = "black")
        # p.mat = p.mat, sig.level = 0.01, insig = "blank")

SeOD2<-table(completesubmission$D3, completesubmission$p3q1E_OD2) 
SeOD2 <- round(prop.table(SeOD2,1), 2) # crosstab as proportion

# Association between size of experimental collaboration and OD practice#
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD1, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD2, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD3, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD4, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD5, chisq = TRUE)
# no correlation

# Association between theorists' seniority and OD practice#
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD6, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD4, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD5, chisq = TRUE)
# no correlation


# Attitude to OD ------------------------------------------------------------------------------------------------------------------------------------------
#Association between experimentalists' seniority and OD attitude#
CrossTable(completesubmission$D3, completesubmission$p3q2E_OD_E1, chisq = TRUE) #p =  0.002204451 
CrossTable(completesubmission$D3, completesubmission$p3q2E_OD_E2, chisq = TRUE) #p =  5.618427e-05 
CrossTable(completesubmission$D3, completesubmission$p3q2E_OD_E3, chisq = TRUE) #p =  0.002273648 
# the younger the researcher, the more likely they are to agree that: data used in publication should be made open; access to data should be provided to anyone who's interested; open data is helpful to them personally.
CrossTable(completesubmission$D3, completesubmission$p3q2E_OD_E4, chisq = TRUE)

SeAttOD1<-table(completesubmission$D3, completesubmission$p3q2E_OD_E1) 
SeAttOD1 <- round(prop.table(SeAttOD1,1), 2) # crosstab as proportion

SeAttOD2<-table(completesubmission$D3, completesubmission$p3q2E_OD_E2) 
SeAttOD2 <- round(prop.table(SeAttOD2,1), 2) # crosstab as proportion

SeAttOD3<-table(completesubmission$D3, completesubmission$p3q2E_OD_E3) 
SeAttOD3 <- round(prop.table(SeAttOD3,1), 2) # crosstab as proportion
# par(mfrow = c(1, 3))
corrplot(SeAttOD1, method = "color",
         addCoef.col = "black",
         tl.col = "black",
         title = "publication data should be open")
corrplot(SeAttOD2, method = "color",
         addCoef.col = "black",
         tl.col = "black")
corrplot(SeAttOD3, method = "color",
         addCoef.col = "black",
         tl.col = "black")


# Combining correlogram with the significance test (not working so far)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tem <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tem$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(SeAttOD1)
head(p.mat)

corrplot(SeAttOD1, method = "color",
         addCoef.col = "black",
         tl.col = "black",
         p.mat = p.mat, sig.level = 0.01, insig = "blank")


#Association between size of experimental collaboration and OD attitude#
CrossTable(completesubmission$D4_1, completesubmission$p3q2E_OD_E1, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q2E_OD_E2, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q2E_OD_E3, chisq = TRUE) 
CrossTable(completesubmission$D4_1, completesubmission$p3q2E_OD_E4, chisq = TRUE)
# no correlation

#Association between theorists' seniority and OD attitude#
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E5, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E4, chisq = TRUE)
# no correlation


# when to share -------------------------------------------------------------------------------------------------------------------------------------------
#Association between theorists' seniority and timing#
CrossTable(completesubmission$D3, completesubmission$p4q1_T1, chisq = TRUE)


# who decide ----------------------------------------------------------------------------------------------------------------------------------------------
#Association between seniority and preference
CrossTable(completesubmission$D3, completesubmission$p4q2, chisq = TRUE)

#Association between field and preference
CrossTable(completesubmission$D4, completesubmission$p4q2, chisq = TRUE) #p =  5.185171e-06  the experimentalists are significantly more likely to want the collaboraiton to decide


ggplot(completesubmission) + 
  geom_bar(mapping = aes(p4q2, fill = D4)) +
  coord_flip()


# factors -------------------------------------------------------------------------------------------------------------------------------------------------
#Association between seniority and factor
CrossTable(completesubmission$D3, completesubmission$p5q2_1, chisq = TRUE) #p =  0.0007962293 
CrossTable(completesubmission$D3, completesubmission$p5q2_2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p5q2_3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p5q2_4, chisq = TRUE) #p =  0.009009351 
CrossTable(completesubmission$D3, completesubmission$p5q2_5, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p5q2_6, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p5q2_7, chisq = TRUE) #p =  0.0008658604 
CrossTable(completesubmission$D3, completesubmission$p5q2_8, chisq = TRUE) #p =  0.0001647595 
CrossTable(completesubmission$D3, completesubmission$p5q2_9, chisq = TRUE)

ggplot(completesubmission) + 
  geom_bar(mapping = aes(p5q2_1, fill = D3), position = "dodge")

#Association between field and factor
CrossTable(completesubmission$D4, completesubmission$p5q2_1, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p5q2_2, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p5q2_3, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p5q2_4, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p5q2_5, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p5q2_6, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p5q2_7, chisq = TRUE) 
CrossTable(completesubmission$D4, completesubmission$p5q2_8, chisq = TRUE)  
CrossTable(completesubmission$D4, completesubmission$p5q2_9, chisq = TRUE) #p =  0.004851126 

ggplot(completesubmission) + 
  geom_bar(mapping = aes(p5q2_1, fill = D4), position = "dodge")


# documentation -------------------------------------------------------------------------------------------------------------------------------------------
#Association between seniority and factor
CrossTable(completesubmission$D3, completesubmission$p6q1_1, chisq = TRUE) #p =  0.0006493578 
CrossTable(completesubmission$D3, completesubmission$p6q1_2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q1_3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q1_4, chisq = TRUE) #p =  0.0004858179 

ggplot(completesubmission) + 
  geom_bar(mapping = aes(p6q1_1, fill = D4), alpha = 1/4, position = "dodge")+
  geom_bar(mapping = aes(p6q1_2, fill = D4), alpha = 1/4, position = "dodge")+
  geom_bar(mapping = aes(p6q1_3, fill = D4), alpha = 1/4, position = "dodge")+
  geom_bar(mapping = aes(p6q1_4, fill = D4), alpha = 1/4, position = "dodge")

#Association between field and factor
CrossTable(completesubmission$D4, completesubmission$p6q1_1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q1_2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q1_3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q1_4, chisq = TRUE) #p =  0.0004858179 

#Association between experimentalists' seniority and opinion on documentation
CrossTable(completesubmission$D3, completesubmission$p6q2E_1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q2E_2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q2E_3, chisq = TRUE) #p =  0.007612034 
CrossTable(completesubmission$D3, completesubmission$p6q2E_4, chisq = TRUE)

#Association between theorists' seniority and opinion on documentation
CrossTable(completesubmission$D3, completesubmission$p6q2T_1, chisq = TRUE) 
CrossTable(completesubmission$D3, completesubmission$p6q2T_2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q2T_3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p6q2T_4, chisq = TRUE)


# peer review ---------------------------------------------------------------------------------------------------------------------------------------------
#Association between experimentalists' seniority and review
CrossTable(completesubmission$D3, completesubmission$p7q1E_1, chisq = TRUE) #p =  7.687443e-05 
CrossTable(completesubmission$D3, completesubmission$p7q1E_2, chisq = TRUE) #p =  2.001491e-07 
CrossTable(completesubmission$D3, completesubmission$p7q1E_3, chisq = TRUE) #p =  0.001641725 
CrossTable(completesubmission$D3, completesubmission$p7q1E_4, chisq = TRUE) #p =  0.005836419
CrossTable(completesubmission$D3, completesubmission$p7q1E_5, chisq = TRUE)

#Association between theorists' seniority and review
CrossTable(completesubmission$D3, completesubmission$p7q1T_1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p7q1T_2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p7q1T_3, chisq = TRUE)


# tools ---------------------------------------------------------------------------------------------------------------------------------------------------
#Association between seniority and tool usage
CrossTable(completesubmission$D3, completesubmission$p8q1_1, chisq = TRUE) #p =  1.290559e-19 
CrossTable(completesubmission$D3, completesubmission$p8q1_2, chisq = TRUE) #p =  0.005205079 
CrossTable(completesubmission$D3, completesubmission$p8q1_3, chisq = TRUE) #p =  0.00122817 
CrossTable(completesubmission$D3, completesubmission$p8q1_4, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p8q1_5, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p8q1_6, chisq = TRUE)

#Association between field and tool usage
CrossTable(completesubmission$D4, completesubmission$p8q1_1, chisq = TRUE) #p =  9.741565e-10 
CrossTable(completesubmission$D4, completesubmission$p8q1_2, chisq = TRUE) #p =  2.363399e-08 
CrossTable(completesubmission$D4, completesubmission$p8q1_3, chisq = TRUE) #p =  1.527742e-23 
CrossTable(completesubmission$D4, completesubmission$p8q1_4, chisq = TRUE) #p =  2.425072e-07 
CrossTable(completesubmission$D4, completesubmission$p8q1_5, chisq = TRUE) #p =  2.073972e-23 
CrossTable(completesubmission$D4, completesubmission$p8q1_6, chisq = TRUE)

#Association between seniority and tool satisfaction
CrossTable(completesubmission$D3, completesubmission$p8q2_1, chisq = TRUE) #p =  5.217052e-14 
CrossTable(completesubmission$D3, completesubmission$p8q2_2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p8q2_3, chisq = TRUE) #p =  0.0006042218 
CrossTable(completesubmission$D3, completesubmission$p8q2_4, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p8q2_5, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p8q2_6, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p8q2_7, chisq = TRUE) #p =  0.00619531 

#Association between field and tool satisfaction
CrossTable(completesubmission$D4, completesubmission$p8q2_1, chisq = TRUE) #p =  4.810789e-06 
CrossTable(completesubmission$D4, completesubmission$p8q2_2, chisq = TRUE) #p =  2.917223e-08 
CrossTable(completesubmission$D4, completesubmission$p8q2_3, chisq = TRUE) #p =  3.522442e-14 
CrossTable(completesubmission$D4, completesubmission$p8q2_4, chisq = TRUE) #p =  0.0001697296 
CrossTable(completesubmission$D4, completesubmission$p8q2_5, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p8q2_6, chisq = TRUE) #p =  5.419208e-10 
CrossTable(completesubmission$D4, completesubmission$p8q2_7, chisq = TRUE) #p =  0.0009206579 


# tool factor ---------------------------------------------------------------------------------------------------------------------------------------------
#Association between seniority and tool factor
CrossTable(completesubmission$D3, completesubmission$p8q3_1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p8q3_2, chisq = TRUE) #p =  0.006345903 
CrossTable(completesubmission$D3, completesubmission$p8q3_3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p8q3_4, chisq = TRUE)

#Association between field and tool factor
CrossTable(completesubmission$D4, completesubmission$p8q3_1, chisq = TRUE) #p =  0.007267048 
CrossTable(completesubmission$D4, completesubmission$p8q3_2, chisq = TRUE) #p =  2.205989e-08 
CrossTable(completesubmission$D4, completesubmission$p8q3_3, chisq = TRUE) #p =  0.0002018648 
CrossTable(completesubmission$D4, completesubmission$p8q3_4, chisq = TRUE) #p =  0.007016096 

toolfactor <- completesubmission[, 101:105]
cortoolfactor <- round(cor(toolfactor), 2)
melted_toolfactor <- melt(toolfactor)
head(melted_toolfactor)
ggplot(melted_toolfactor, aes(x = ))