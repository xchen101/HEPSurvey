## [Combining plots](https://www.statmethods.net/advgraphs/layout.html)
2 rows and 2 columns
```
par(mfrow=c(2,2))
```
3 rows and 1 columns
```
par(mfrow=c(3,1))
```
See link for *layout()*
[Arranging plots with par(mfrow) and layout()](https://bookdown.org/ndphillips/YaRrr/arranging-plots-with-parmfrow-and-layout.html)


## [Filtering data with dplyr](https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e)
```
library(dplyr)
flight %>%
  select(FL_DATE, CARRIER, ORIGIN, ORIGIN_CITY_NAME, ORIGIN_STATE_ABR, DEP_DELAY, DEP_TIME, ARR_DELAY, ARR_TIME) %>%
  filter(CARRIER == "UA" & ORIGIN == "SFO")
```
"!=" not equal
```
filter(CARRIER != "UA" & ORIGIN == "SFO")
```
‘%in%’
```
filter(CARRIER %in% c("UA", "AA"))
```
See link for more.

## [Heatmap](https://www.r-bloggers.com/how-to-make-a-simple-heatmap-in-ggplot2/)
```
ggplot(data = df.team_data, aes(x = metrics, y = teams)) +
  geom_tile(aes(fill = performance))
```

## [Mosaic plots and association plots](https://www.statmethods.net/advgraphs/mosaic.html)
```
library(vcd)
mosaic(HairEyeColor, shade=TRUE, legend=TRUE)
```
```
library(vcd)
assoc(HairEyeColor, shade=TRUE)
```
### [Rotate labels with mosaic plots](https://stackoverflow.com/questions/33492061/rotating-y-axis-labels-with-mosaic-plots)
```
rot_labels=c(0,90,0,0)
help( labeling_border, pac=vcd)
```

## [MCA - Multiple Correspondence Analysis in R: Essentials](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/)



Book:
[Practical Guide to Cluster Analysis in R](https://www.datanovia.com/en/product/practical-guide-to-cluster-analysis-in-r/)
