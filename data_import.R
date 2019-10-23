#$Rev: 12179 $ .
data <- read_csv("Data/survey_77428_R_data_file.csv", sep=",", quote = "'", na.strings=c("","\"\""), stringsAsFactors=FALSE)

data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "id"
names(data)[1] <- "id"

data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "submitdate"
names(data)[2] <- "submitdate"

#Field hidden

data[, 3] <- as.character(data[, 3])
attributes(data)$variable.labels[3] <- "startlanguage"
names(data)[3] <- "startlanguage"

data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "token"
names(data)[4] <- "token"

data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "datestamp"
names(data)[5] <- "datestamp"

data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "startdate"
names(data)[6] <- "startdate"

data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "refurl"
names(data)[7] <- "refurl"

data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- "What is your gender?"
data[, 8] <- factor(data[, 8], levels=c("M","F","NA"),labels=c("Male","Female","Prefer not to say"))
names(data)[8] <- "D1"

data[, 9] <- as.numeric(data[, 9])
attributes(data)$variable.labels[9] <- "Where is your home institute located?"
data[, 9] <- factor(data[, 9], levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196),labels=c("Afghanistan","Albania","Algeria","Andorra","Angola","Antigua and Barbuda","Argentina","Armenia","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bhutan","Bolivia (Plurinational State of)","Bosnia and Herzegovina","Botswana","Brazil","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cabo Verde","Cambodia","Cameroon","Canada","Central African Republic","Chad","Chile","China","Colombia","Comoros","Congo","Cook Islands","Costa Rica","Côte d’Ivoire","Croatia","Cuba","Cyprus","Czech Republic","Democratic People’s Republic of Korea","Congo","Denmark","Djibouti","Dominica","Dominican Republic","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Grenada","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras","Hungary","Iceland","India","Indonesia","Iran (Islamic Republic of)","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Kuwait","Kyrgyzstan","Lao People’s Democratic Republic","Latvia","Lebanon","Lesotho","Liberia","Libya","Lithuania","Luxembourg","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Mauritania","Mauritius","Mexico","Micronesia (Federated States of)","Monaco","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Niue","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Puerto Rico","Qatar","Republic of Korea","Republic of Moldova","Romania","Russian Federation","Rwanda","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","San Marino","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Sudan","Spain","Sri Lanka","Sudan","Suriname","Swaziland","Sweden","Switzerland","Syrian Arab Republic","Tajikistan","Thailand","The former Yugoslav Republic of Macedonia","Timor-Leste","Togo","Tokelau","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Tuvalu","Uganda","Ukraine","United Arab Emirates","the United Kingdom of Great Britain and Northern Ireland","United Republic of Tanzania","United States of America","Uruguay","Uzbekistan","Vanuatu","Venezuela (Bolivarian Republic of)","Viet Nam","Yemen","Zambia","Zimbabwe"))
names(data)[9] <- "D2"

data[, 10] <- as.character(data[, 10])
attributes(data)$variable.labels[10] <- "Approximately how many years have you been working in high-energy physics?"
data[, 10] <- factor(data[, 10], levels=c("YR1","YR2","YR3","YR4","YR5"),labels=c("less than 5 years","5 - 10 years","11 - 15 years","16 - 20 years","more than 20 years"))
names(data)[10] <- "D3"

data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "What is your domain?"
data[, 11] <- factor(data[, 11], levels=c("EXP","TH","OTR"),labels=c("Experiment","Theory","Other"))
names(data)[11] <- "D4"

data[, 12] <- as.character(data[, 12])
attributes(data)$variable.labels[12] <- "How large is your experimental collaboration?"
data[, 12] <- factor(data[, 12], levels=c("SZ1","SZ2","SZ3","SZ4","SZ5"),labels=c("> 500 members","101 - 500 members","51 - 100 members","20 - 50 members","< 20 members"))
names(data)[12] <- "D4_1"

data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- "Please specify:"
names(data)[13] <- "D4_2"

data[, 14] <- as.numeric(data[, 14])
attributes(data)$variable.labels[14] <- "[I think practising Open Science benefits HEP as a discipline. ] Base on the information above (and your previous understanding), what is your impression about Open Science?"
data[, 14] <- factor(data[, 14], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[14] <- "p2q1_OS1"

data[, 15] <- as.numeric(data[, 15])
attributes(data)$variable.labels[15] <- "[I think practising Open Science benefits me as a researcher.] Base on the information above (and your previous understanding), what is your impression about Open Science?"
data[, 15] <- factor(data[, 15], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[15] <- "p2q1_OS2"

data[, 16] <- as.numeric(data[, 16])
attributes(data)$variable.labels[16] <- "[I think practising Open Science benefits the general public.] Base on the information above (and your previous understanding), what is your impression about Open Science?"
data[, 16] <- factor(data[, 16], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[16] <- "p2q1_OS3"

data[, 17] <- as.numeric(data[, 17])
attributes(data)$variable.labels[17] <- "[OA has changed the way I find and read papers.] According to your experience, how much do you agree with the following statements about Open Access (OA)?"
data[, 17] <- factor(data[, 17], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[17] <- "p2q2_OA1"

data[, 18] <- as.numeric(data[, 18])
attributes(data)$variable.labels[18] <- "[OA has changed the way I submit papers for publication.] According to your experience, how much do you agree with the following statements about Open Access (OA)?"
data[, 18] <- factor(data[, 18], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[18] <- "p2q2_OA2"

data[, 19] <- as.numeric(data[, 19])
attributes(data)$variable.labels[19] <- "[I have read/ heard about open data.] How have you interacted with Open Data?"
data[, 19] <- factor(data[, 19], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[19] <- "p3q1E_OD1"

data[, 20] <- as.numeric(data[, 20])
attributes(data)$variable.labels[20] <- "[I have tried to find available open data.] How have you interacted with Open Data?"
data[, 20] <- factor(data[, 20], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[20] <- "p3q1E_OD2"

data[, 21] <- as.numeric(data[, 21])
attributes(data)$variable.labels[21] <- "[I have used open data.] How have you interacted with Open Data?"
data[, 21] <- factor(data[, 21], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[21] <- "p3q1E_OD3"

data[, 22] <- as.numeric(data[, 22])
attributes(data)$variable.labels[22] <- "[I have openly released some of my data.] How have you interacted with Open Data?"
data[, 22] <- factor(data[, 22], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[22] <- "p3q1E_OD4"

data[, 23] <- as.numeric(data[, 23])
attributes(data)$variable.labels[23] <- "[I have never interacted with open data.] How have you interacted with Open Data?"
data[, 23] <- factor(data[, 23], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[23] <- "p3q1E_OD5"

data[, 24] <- as.numeric(data[, 24])
attributes(data)$variable.labels[24] <- "[I have read/ heard about open data.] How have you interacted with Open Data?"
data[, 24] <- factor(data[, 24], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[24] <- "p3q1T_OD1"

data[, 25] <- as.numeric(data[, 25])
attributes(data)$variable.labels[25] <- "[I have tried to find available open data.] How have you interacted with Open Data?"
data[, 25] <- factor(data[, 25], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[25] <- "p3q1T_OD2"

data[, 26] <- as.numeric(data[, 26])
attributes(data)$variable.labels[26] <- "[I have used open data from other theorists.] How have you interacted with Open Data?"
data[, 26] <- factor(data[, 26], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[26] <- "p3q1T_OD3"

data[, 27] <- as.numeric(data[, 27])
attributes(data)$variable.labels[27] <- "[I have used open data from the experiments.] How have you interacted with Open Data?"
data[, 27] <- factor(data[, 27], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[27] <- "p3q1T_OD6"

data[, 28] <- as.numeric(data[, 28])
attributes(data)$variable.labels[28] <- "[I have openly released some of my data.] How have you interacted with Open Data?"
data[, 28] <- factor(data[, 28], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[28] <- "p3q1T_OD4"

data[, 29] <- as.numeric(data[, 29])
attributes(data)$variable.labels[29] <- "[I have never interacted with open data.] How have you interacted with Open Data?"
data[, 29] <- factor(data[, 29], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[29] <- "p3q1T_OD5"

data[, 30] <- as.numeric(data[, 30])
attributes(data)$variable.labels[30] <- "[The data used in published studies should be made open.] How much do you agree with the following statements about open data?"
data[, 30] <- factor(data[, 30], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[30] <- "p3q2E_OD_E1"

data[, 31] <- as.numeric(data[, 31])
attributes(data)$variable.labels[31] <- "[Access to research data should be provided to anyone who\'s interested to see them.] How much do you agree with the following statements about open data?"
data[, 31] <- factor(data[, 31], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[31] <- "p3q2E_OD_E2"

data[, 32] <- as.numeric(data[, 32])
attributes(data)$variable.labels[32] <- "[Open and easy access to research data from past studies is helpful to me.] How much do you agree with the following statements about open data?"
data[, 32] <- factor(data[, 32], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[32] <- "p3q2E_OD_E3"

data[, 33] <- as.numeric(data[, 33])
attributes(data)$variable.labels[33] <- "[Whether data from my study will be open does not affect how I work.] How much do you agree with the following statements about open data?"
data[, 33] <- factor(data[, 33], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[33] <- "p3q2E_OD_E4"

data[, 34] <- as.numeric(data[, 34])
attributes(data)$variable.labels[34] <- "[I think the data used in published studies should be made open.] How much do you agree with the following statements about open data?"
data[, 34] <- factor(data[, 34], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[34] <- "p3q2T_OD_E1"

data[, 35] <- as.numeric(data[, 35])
attributes(data)$variable.labels[35] <- "[I think access to research data should be provided to anyone who\'s interested to see them.] How much do you agree with the following statements about open data?"
data[, 35] <- factor(data[, 35], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[35] <- "p3q2T_OD_E2"

data[, 36] <- as.numeric(data[, 36])
attributes(data)$variable.labels[36] <- "[Open and easy access to research data from past studies is helpful to me.] How much do you agree with the following statements about open data?"
data[, 36] <- factor(data[, 36], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[36] <- "p3q2T_OD_E3"

data[, 37] <- as.numeric(data[, 37])
attributes(data)$variable.labels[37] <- "[Open and easy access to experiment data from past studies is helpful to me.] How much do you agree with the following statements about open data?"
data[, 37] <- factor(data[, 37], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[37] <- "p3q2T_OD_E5"

data[, 38] <- as.numeric(data[, 38])
attributes(data)$variable.labels[38] <- "[Whether data from my study will be open does not affect how I work.] How much do you agree with the following statements about open data?"
data[, 38] <- factor(data[, 38], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[38] <- "p3q2T_OD_E4"

data[, 39] <- as.numeric(data[, 39])
attributes(data)$variable.labels[39] <- "[As soon as the research is completed.] When do you think is it appropriate to openly share research data and code?"
data[, 39] <- factor(data[, 39], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[39] <- "p4q1T_1"

data[, 40] <- as.numeric(data[, 40])
attributes(data)$variable.labels[40] <- "[When the paper is submitted to arxiv.] When do you think is it appropriate to openly share research data and code?"
data[, 40] <- factor(data[, 40], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[40] <- "p4q1T_2"

data[, 41] <- as.numeric(data[, 41])
attributes(data)$variable.labels[41] <- "[When the paper is submitted to a journal.] When do you think is it appropriate to openly share research data and code?"
data[, 41] <- factor(data[, 41], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[41] <- "p4q1T_3"

data[, 42] <- as.numeric(data[, 42])
attributes(data)$variable.labels[42] <- "[When the paper is accepted for publication.] When do you think is it appropriate to openly share research data and code?"
data[, 42] <- factor(data[, 42], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[42] <- "p4q1T_4"

data[, 43] <- as.numeric(data[, 43])
attributes(data)$variable.labels[43] <- "[When the paper is published.] When do you think is it appropriate to openly share research data and code?"
data[, 43] <- factor(data[, 43], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[43] <- "p4q1T_5"

data[, 44] <- as.numeric(data[, 44])
attributes(data)$variable.labels[44] <- "[Some time after the publication of the paper.] When do you think is it appropriate to openly share research data and code?"
data[, 44] <- factor(data[, 44], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[44] <- "p4q1T_6"

data[, 45] <- as.numeric(data[, 45])
attributes(data)$variable.labels[45] <- "[It is not appropriate to share data and code.] When do you think is it appropriate to openly share research data and code?"
data[, 45] <- factor(data[, 45], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[45] <- "p4q1T_7"

data[, 46] <- as.numeric(data[, 46])
attributes(data)$variable.labels[46] <- "[Other] When do you think is it appropriate to openly share research data and code?"
data[, 46] <- factor(data[, 46], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[46] <- "p4q1T_8"

data[, 47] <- as.character(data[, 47])
attributes(data)$variable.labels[47] <- "Please specify:"
names(data)[47] <- "p4q1T_O"

data[, 48] <- as.numeric(data[, 48])
attributes(data)$variable.labels[48] <- "[When the manuscript is ready for internal review.] When do you think is it appropriate to openly release research data and code used in a study?"
data[, 48] <- factor(data[, 48], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[48] <- "p4q1E_1"

data[, 49] <- as.numeric(data[, 49])
attributes(data)$variable.labels[49] <- "[When the manuscript is submitted to arXiv and/or journal.] When do you think is it appropriate to openly release research data and code used in a study?"
data[, 49] <- factor(data[, 49], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[49] <- "p4q1E_2"

data[, 50] <- as.numeric(data[, 50])
attributes(data)$variable.labels[50] <- "[When the manuscript is accepted for publication] When do you think is it appropriate to openly release research data and code used in a study?"
data[, 50] <- factor(data[, 50], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[50] <- "p4q1E_3"

data[, 51] <- as.numeric(data[, 51])
attributes(data)$variable.labels[51] <- "[At the same time as the publication of the paper.] When do you think is it appropriate to openly release research data and code used in a study?"
data[, 51] <- factor(data[, 51], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[51] <- "p4q1E_4"

data[, 52] <- as.numeric(data[, 52])
attributes(data)$variable.labels[52] <- "[Some time after the publication of the paper.] When do you think is it appropriate to openly release research data and code used in a study?"
data[, 52] <- factor(data[, 52], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[52] <- "p4q1E_5"

data[, 53] <- as.numeric(data[, 53])
attributes(data)$variable.labels[53] <- "[It is not appropriate to share data and code.] When do you think is it appropriate to openly release research data and code used in a study?"
data[, 53] <- factor(data[, 53], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[53] <- "p4q1E_6"

data[, 54] <- as.numeric(data[, 54])
attributes(data)$variable.labels[54] <- "[Other] When do you think is it appropriate to openly release research data and code used in a study?"
data[, 54] <- factor(data[, 54], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[54] <- "p4q1E_7"

data[, 55] <- as.character(data[, 55])
attributes(data)$variable.labels[55] <- "Please specify:"
names(data)[55] <- "p4q1E_O"

data[, 56] <- as.numeric(data[, 56])
attributes(data)$variable.labels[56] <- "In your opinion, who should decide the accessibility of research data and code?"
data[, 56] <- factor(data[, 56], levels=c(1,2,3,4,5),labels=c("Each individual researcher should decide for their work autonomously.","Each collaboration or team should decide for their work.","The HEP community as a whole should decide and set practices.","The funding bodies or institutions should decide and develop policies for researchers to follow.","Other"))
names(data)[56] <- "p4q2"

data[, 57] <- as.character(data[, 57])
attributes(data)$variable.labels[57] <- "Please specify:"
names(data)[57] <- "p4q2_O"

data[, 58] <- as.numeric(data[, 58])
attributes(data)$variable.labels[58] <- "[I have asked others to share with me before. ] What\'s your experience with data / code sharing?"
data[, 58] <- factor(data[, 58], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[58] <- "p5q1_SQ002"

data[, 59] <- as.numeric(data[, 59])
attributes(data)$variable.labels[59] <- "[I have shared with others before.] What\'s your experience with data / code sharing?"
data[, 59] <- factor(data[, 59], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[59] <- "p5q1_SQ001"

data[, 60] <- as.numeric(data[, 60])
attributes(data)$variable.labels[60] <- "[I have never considered sharing.] What\'s your experience with data / code sharing?"
data[, 60] <- factor(data[, 60], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[60] <- "p5q1_SQ004"

data[, 61] <- as.numeric(data[, 61])
attributes(data)$variable.labels[61] <- "[I have no interest in sharing.] What\'s your experience with data / code sharing?"
data[, 61] <- factor(data[, 61], levels=c(1,0),labels=c("Yes","Not selected"))
names(data)[61] <- "p5q1_SQ005"

data[, 62] <- as.character(data[, 62])
attributes(data)$variable.labels[62] <- "You have chosen \"I have no interest in sharing\", what is the main reason for this?"
names(data)[62] <- "p5q1_N"

data[, 63] <- as.numeric(data[, 63])
attributes(data)$variable.labels[63] <- "[How much additional work it takes to share.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 63] <- factor(data[, 63], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[63] <- "p5q2_1"

data[, 64] <- as.numeric(data[, 64])
attributes(data)$variable.labels[64] <- "[Whether I have the rights to share.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 64] <- factor(data[, 64], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[64] <- "p5q2_2"

data[, 65] <- as.numeric(data[, 65])
attributes(data)$variable.labels[65] <- "[How competitive the research area is.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 65] <- factor(data[, 65], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[65] <- "p5q2_3"

data[, 66] <- as.numeric(data[, 66])
attributes(data)$variable.labels[66] <- "[Whether the data or code is of good quality.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 66] <- factor(data[, 66], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[66] <- "p5q2_4"

data[, 67] <- as.numeric(data[, 67])
attributes(data)$variable.labels[67] <- "[Whether the data or code is essential for reproducing the study.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 67] <- factor(data[, 67], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[67] <- "p5q2_5"

data[, 68] <- as.numeric(data[, 68])
attributes(data)$variable.labels[68] <- "[Whether I consider it to be useful to other researchers.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 68] <- factor(data[, 68], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[68] <- "p5q2_6"

data[, 69] <- as.numeric(data[, 69])
attributes(data)$variable.labels[69] <- "[Whether I am asked to share by other researchers.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 69] <- factor(data[, 69], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[69] <- "p5q2_7"

data[, 70] <- as.numeric(data[, 70])
attributes(data)$variable.labels[70] <- "[Whether the data or code will be responsibly used.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 70] <- factor(data[, 70], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[70] <- "p5q2_8"

data[, 71] <- as.numeric(data[, 71])
attributes(data)$variable.labels[71] <- "[Whether I am obligated to share by mandates.] How much does each of the following factors affect your decision on data and code sharing?"
data[, 71] <- factor(data[, 71], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[71] <- "p5q2_9"

data[, 72] <- as.numeric(data[, 72])
attributes(data)$variable.labels[72] <- "[I prefer to ask people personally for explanations / instructions than going through documentation.] How much do you agree with the following statements about the documentation of research process/ steps information:"
data[, 72] <- factor(data[, 72], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[72] <- "p6q1_1"

data[, 73] <- as.numeric(data[, 73])
attributes(data)$variable.labels[73] <- "[Well documented process information directly affects the reproducibility / reuse of the results] How much do you agree with the following statements about the documentation of research process/ steps information:"
data[, 73] <- factor(data[, 73], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[73] <- "p6q1_2"

data[, 74] <- as.numeric(data[, 74])
attributes(data)$variable.labels[74] <- "[Writing documentation is not usually a productive use of time in my research.] How much do you agree with the following statements about the documentation of research process/ steps information:"
data[, 74] <- factor(data[, 74], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[74] <- "p6q1_3"

data[, 75] <- as.numeric(data[, 75])
attributes(data)$variable.labels[75] <- "[Documentation is of little use to people who are not directly involved in the study.] How much do you agree with the following statements about the documentation of research process/ steps information:"
data[, 75] <- factor(data[, 75], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[75] <- "p6q1_4"

data[, 76] <- as.numeric(data[, 76])
attributes(data)$variable.labels[76] <- "[Analysis code should be formally reviewed as part of the peer review process for publication in journals.] According to your experience, how much do you agree with the following statements about peer review:"
data[, 76] <- factor(data[, 76], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[76] <- "p6q2E_1"

data[, 77] <- as.numeric(data[, 77])
attributes(data)$variable.labels[77] <- "[As long as the result is reproducible, the quality of the code should not affect the outcome of the review.] According to your experience, how much do you agree with the following statements about peer review:"
data[, 77] <- factor(data[, 77], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[77] <- "p6q2E_2"

data[, 78] <- as.numeric(data[, 78])
attributes(data)$variable.labels[78] <- "[The documentation status of an analysis should be evaluated as part of the external peer review process.] According to your experience, how much do you agree with the following statements about peer review:"
data[, 78] <- factor(data[, 78], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[78] <- "p6q2E_3"

data[, 79] <- as.numeric(data[, 79])
attributes(data)$variable.labels[79] <- "[Data and code used in an analysis should be readily documented by the time the paper is submitted for publication.] According to your experience, how much do you agree with the following statements about peer review:"
data[, 79] <- factor(data[, 79], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[79] <- "p6q2E_4"

data[, 80] <- as.numeric(data[, 80])
attributes(data)$variable.labels[80] <- "[Code produced and used in a study should be reviewed as part of the publication review process.] According to your experience, how much do you agree with the following statements about peer review as part of publication in a journal:"
data[, 80] <- factor(data[, 80], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[80] <- "p6q2T_1"

data[, 81] <- as.numeric(data[, 81])
attributes(data)$variable.labels[81] <- "[As long as the result is reproducible, the quality of the code should not affect the outcome of the review.] According to your experience, how much do you agree with the following statements about peer review as part of publication in a journal:"
data[, 81] <- factor(data[, 81], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[81] <- "p6q2T_2"

data[, 82] <- as.numeric(data[, 82])
attributes(data)$variable.labels[82] <- "[The documentation status of a study should be evaluated as part of the publication review process.] According to your experience, how much do you agree with the following statements about peer review as part of publication in a journal:"
data[, 82] <- factor(data[, 82], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[82] <- "p6q2T_3"

data[, 83] <- as.numeric(data[, 83])
attributes(data)$variable.labels[83] <- "[Data and code used in a study should be readily documented by the time the paper is submitted for publication.] According to your experience, how much do you agree with the following statements about peer review as part of publication in a journal:"
data[, 83] <- factor(data[, 83], levels=c(1,2,3,4,5),labels=c("Strongly disagree","Somewhat disagree","Neutral","Somewhat agree","Strongly agree"))
names(data)[83] <- "p6q2T_4"

data[, 84] <- as.numeric(data[, 84])
attributes(data)$variable.labels[84] <- "[Data] How much do you think providing access to the following analysis information in addition to the manuscript will help strengthen the current peer review process?"
data[, 84] <- factor(data[, 84], levels=c(1,2,3,4,5),labels=c("Very unhelpful","Somewhat unhelpful","Neutral","Somewhat helpful","Very helpful"))
names(data)[84] <- "p7q1E_1"

data[, 85] <- as.numeric(data[, 85])
attributes(data)$variable.labels[85] <- "[Code] How much do you think providing access to the following analysis information in addition to the manuscript will help strengthen the current peer review process?"
data[, 85] <- factor(data[, 85], levels=c(1,2,3,4,5),labels=c("Very unhelpful","Somewhat unhelpful","Neutral","Somewhat helpful","Very helpful"))
names(data)[85] <- "p7q1E_2"

data[, 86] <- as.numeric(data[, 86])
attributes(data)$variable.labels[86] <- "[Related Twiki pages] How much do you think providing access to the following analysis information in addition to the manuscript will help strengthen the current peer review process?"
data[, 86] <- factor(data[, 86], levels=c(1,2,3,4,5),labels=c("Very unhelpful","Somewhat unhelpful","Neutral","Somewhat helpful","Very helpful"))
names(data)[86] <- "p7q1E_3"

data[, 87] <- as.numeric(data[, 87])
attributes(data)$variable.labels[87] <- "[Presentation slides] How much do you think providing access to the following analysis information in addition to the manuscript will help strengthen the current peer review process?"
data[, 87] <- factor(data[, 87], levels=c(1,2,3,4,5),labels=c("Very unhelpful","Somewhat unhelpful","Neutral","Somewhat helpful","Very helpful"))
names(data)[87] <- "p7q1E_4"

data[, 88] <- as.numeric(data[, 88])
attributes(data)$variable.labels[88] <- "[Scripted workflow (e.g. bash script, makefile, etc.)] How much do you think providing access to the following analysis information in addition to the manuscript will help strengthen the current peer review process?"
data[, 88] <- factor(data[, 88], levels=c(1,2,3,4,5),labels=c("Very unhelpful","Somewhat unhelpful","Neutral","Somewhat helpful","Very helpful"))
names(data)[88] <- "p7q1E_5"

data[, 89] <- as.numeric(data[, 89])
attributes(data)$variable.labels[89] <- "[Data files] Do you think providing access to the following information for peer review will help strengthen the current review process?"
data[, 89] <- factor(data[, 89], levels=c(1,2,3,4,5),labels=c("Very unhelpful","Somewhat unhelpful","Neutral","Somewhat helpful","Very helpful"))
names(data)[89] <- "p7q1T_1"

data[, 90] <- as.numeric(data[, 90])
attributes(data)$variable.labels[90] <- "[Scripts] Do you think providing access to the following information for peer review will help strengthen the current review process?"
data[, 90] <- factor(data[, 90], levels=c(1,2,3,4,5),labels=c("Very unhelpful","Somewhat unhelpful","Neutral","Somewhat helpful","Very helpful"))
names(data)[90] <- "p7q1T_2"

data[, 91] <- as.numeric(data[, 91])
attributes(data)$variable.labels[91] <- "[Documented methods] Do you think providing access to the following information for peer review will help strengthen the current review process?"
data[, 91] <- factor(data[, 91], levels=c(1,2,3,4,5),labels=c("Very unhelpful","Somewhat unhelpful","Neutral","Somewhat helpful","Very helpful"))
names(data)[91] <- "p7q1T_3"

data[, 92] <- as.character(data[, 92])
attributes(data)$variable.labels[92] <- "Any other comment:"
names(data)[92] <- "p7q2"

data[, 93] <- as.numeric(data[, 93])
attributes(data)$variable.labels[93] <- "[Source code management e.g. GitHub / GitLab] How often do you use the following collaborative tools?"
data[, 93] <- factor(data[, 93], levels=c(1,2,3,4,5,6,7),labels=c("Daily","Weekly","Monthly","Once every 3 months","Twice a year","Once a year","Less than once a year"))
names(data)[93] <- "p8q1_1"

data[, 94] <- as.numeric(data[, 94])
attributes(data)$variable.labels[94] <- "[Issue tracker e.g. JIRA / Trello] How often do you use the following collaborative tools?"
data[, 94] <- factor(data[, 94], levels=c(1,2,3,4,5,6,7),labels=c("Daily","Weekly","Monthly","Once every 3 months","Twice a year","Once a year","Less than once a year"))
names(data)[94] <- "p8q1_2"

data[, 95] <- as.numeric(data[, 95])
attributes(data)$variable.labels[95] <- "[Documentation management e.g. TWikis / Wikis] How often do you use the following collaborative tools?"
data[, 95] <- factor(data[, 95], levels=c(1,2,3,4,5,6,7),labels=c("Daily","Weekly","Monthly","Once every 3 months","Twice a year","Once a year","Less than once a year"))
names(data)[95] <- "p8q1_3"

data[, 96] <- as.numeric(data[, 96])
attributes(data)$variable.labels[96] <- "[Multi-user editors e.g. Overleaf / ShareLaTex, Google Doc / Slide / Sheet] How often do you use the following collaborative tools?"
data[, 96] <- factor(data[, 96], levels=c(1,2,3,4,5,6,7),labels=c("Daily","Weekly","Monthly","Once every 3 months","Twice a year","Once a year","Less than once a year"))
names(data)[96] <- "p8q1_4"

data[, 97] <- as.numeric(data[, 97])
attributes(data)$variable.labels[97] <- "[Event managers e.g. Indico] How often do you use the following collaborative tools?"
data[, 97] <- factor(data[, 97], levels=c(1,2,3,4,5,6,7),labels=c("Daily","Weekly","Monthly","Once every 3 months","Twice a year","Once a year","Less than once a year"))
names(data)[97] <- "p8q1_5"

data[, 98] <- as.numeric(data[, 98])
attributes(data)$variable.labels[98] <- "[Interactive notebooks e.g. Jupyter/ IPython Notebook] How often do you use the following collaborative tools?"
data[, 98] <- factor(data[, 98], levels=c(1,2,3,4,5,6,7),labels=c("Daily","Weekly","Monthly","Once every 3 months","Twice a year","Once a year","Less than once a year"))
names(data)[98] <- "p8q1_6"

data[, 99] <- as.numeric(data[, 99])
attributes(data)$variable.labels[99] <- "[Source code management tools e.g. GitHub / GitLab] How satisfied are you with the collaborative tools you are using? "
data[, 99] <- factor(data[, 99], levels=c(1,2,3,4,5,6),labels=c("Very dissatisfied","Moderately dissatisfied","Neither satisfied nor dissatisfied","Moderately satisfied","Very satisfied","No experience"))
names(data)[99] <- "p8q2_1"

data[, 100] <- as.numeric(data[, 100])
attributes(data)$variable.labels[100] <- "[Issue tracker e.g. JIRA / Trello] How satisfied are you with the collaborative tools you are using? "
data[, 100] <- factor(data[, 100], levels=c(1,2,3,4,5,6),labels=c("Very dissatisfied","Moderately dissatisfied","Neither satisfied nor dissatisfied","Moderately satisfied","Very satisfied","No experience"))
names(data)[100] <- "p8q2_2"

data[, 101] <- as.numeric(data[, 101])
attributes(data)$variable.labels[101] <- "[TWikis / Wikis] How satisfied are you with the collaborative tools you are using? "
data[, 101] <- factor(data[, 101], levels=c(1,2,3,4,5,6),labels=c("Very dissatisfied","Moderately dissatisfied","Neither satisfied nor dissatisfied","Moderately satisfied","Very satisfied","No experience"))
names(data)[101] <- "p8q2_3"

data[, 102] <- as.numeric(data[, 102])
attributes(data)$variable.labels[102] <- "[Multi-user editors e.g. Overleaf / ShareLaTex, Google Doc / Slide / Sheet] How satisfied are you with the collaborative tools you are using? "
data[, 102] <- factor(data[, 102], levels=c(1,2,3,4,5,6),labels=c("Very dissatisfied","Moderately dissatisfied","Neither satisfied nor dissatisfied","Moderately satisfied","Very satisfied","No experience"))
names(data)[102] <- "p8q2_4"

data[, 103] <- as.numeric(data[, 103])
attributes(data)$variable.labels[103] <- "[Reference manager tools e.g. Mendeley / Zotero] How satisfied are you with the collaborative tools you are using? "
data[, 103] <- factor(data[, 103], levels=c(1,2,3,4,5,6),labels=c("Very dissatisfied","Moderately dissatisfied","Neither satisfied nor dissatisfied","Moderately satisfied","Very satisfied","No experience"))
names(data)[103] <- "p8q2_5"

data[, 104] <- as.numeric(data[, 104])
attributes(data)$variable.labels[104] <- "[Event managers e.g. Indico] How satisfied are you with the collaborative tools you are using? "
data[, 104] <- factor(data[, 104], levels=c(1,2,3,4,5,6),labels=c("Very dissatisfied","Moderately dissatisfied","Neither satisfied nor dissatisfied","Moderately satisfied","Very satisfied","No experience"))
names(data)[104] <- "p8q2_6"

data[, 105] <- as.numeric(data[, 105])
attributes(data)$variable.labels[105] <- "[Interactive notebooks e.g. Jupyter/ IPython Notebook] How satisfied are you with the collaborative tools you are using? "
data[, 105] <- factor(data[, 105], levels=c(1,2,3,4,5,6),labels=c("Very dissatisfied","Moderately dissatisfied","Neither satisfied nor dissatisfied","Moderately satisfied","Very satisfied","No experience"))
names(data)[105] <- "p8q2_7"

data[, 106] <- as.numeric(data[, 106])
attributes(data)$variable.labels[106] <- "[Endorsement by a trusted group (analysis team / collaboration).] How much do the following factors impact your decision in adopting collaborative tools in your workflow?"
data[, 106] <- factor(data[, 106], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[106] <- "p8q3_1"

data[, 107] <- as.numeric(data[, 107])
attributes(data)$variable.labels[107] <- "[Improvement in efficiency compared to previous workflows.] How much do the following factors impact your decision in adopting collaborative tools in your workflow?"
data[, 107] <- factor(data[, 107], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[107] <- "p8q3_2"

data[, 108] <- as.numeric(data[, 108])
attributes(data)$variable.labels[108] <- "[Ease of use.] How much do the following factors impact your decision in adopting collaborative tools in your workflow?"
data[, 108] <- factor(data[, 108], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[108] <- "p8q3_3"

data[, 109] <- as.numeric(data[, 109])
attributes(data)$variable.labels[109] <- "[Adoption rate among people around me.] How much do the following factors impact your decision in adopting collaborative tools in your workflow?"
data[, 109] <- factor(data[, 109], levels=c(1,2,3,4,5),labels=c("Not at all affect","Rarely affect","Neutral","Somewhat affect","Affect a lot"))
names(data)[109] <- "p8q3_4"

