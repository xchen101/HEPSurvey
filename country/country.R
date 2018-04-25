library(rworldmap)
setwd("/Users/xiaolichen/R projects/HEP survey/HEPSurvey/country")
country = read.csv("country.csv")

n <- joinCountryData2Map(country, joinCode = "NAME", nameJoinColumn = "country")
mapCountryData (n, 
                nameColumnToPlot = "count", 
                mapTitle = "Distribution of survey takers",
                colourPalette = "white2Black",
                catMethod = "pretty")
