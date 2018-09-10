library(rworldmap)
setwd("/Users/xiaolichen/R projects/HEP survey/HEPSurvey/country")
country = read.csv("book1.csv")

n <- joinCountryData2Map(country, joinCode = "NAME", nameJoinColumn = "country")
mapCountryData (n, 
                nameColumnToPlot = "count", 
                mapTitle = "Geographic distribution of survey takers",
                colourPalette = "white2Black",
                catMethod = "pretty")
