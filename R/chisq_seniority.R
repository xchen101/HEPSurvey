# chi-square test for attitudes against seniority 
# http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
# fisher's exact test https://www.r-bloggers.com/contingency-tables-%E2%80%93-fisher%E2%80%99s-exact-test/ 

OS1 <- table(coded_data$`p2q1 [OS1]`, coded_data$D3)
chisq.test(OS1)
fisher.test(OS1)
#X-squared = 2.7453, df = 4, p-value = 0.6013
OS1 <- table(coded_data$`p2q1 [OS1]`, coded_data$D4)
chisq.test(OS1)
#X-squared = 6.2263, df = 8, p-value = 0.6219
OS1

OS2 <- table(coded_data$`p2q1 [OS2]`, coded_data$D3)
chisq.test(OS2)
fisher.test(OS2)
#X-squared = 5.4652, df = 4, p-value = 0.2428

OS3 <- table(coded_data$`p2q1 [OS3]`, coded_data$D3)
chisq.test(OS3)
fisher.test(OS3)
#X-squared = 12.772, df = 4, p-value = 0.01245

OA1 <- table(coded_data$`p2q2 [OA1]`, coded_data$D3)
chisq.test(OA1)
#X-squared = 8.389, df = 4, p-value = 0.07832

OA2 <- table(coded_data$`p2q2 [OA2]`, coded_data$D3)
chisq.test(OA2)
fisher.test(OA2)
#X-squared = 21.887, df = 4, p-value = 0.000211


#experiment
piggy <- table(coded_data$p4q2, coded_data$D4)
ctble = cbind(piggy[,"EX"], piggy[,"Other"] + piggy[,"TH"])
chisq.test(ctble)
#X-squared = 21.446, df = 4, p-value = 0.0002582

