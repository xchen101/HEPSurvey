#an attempt to reproduce from this tutorial:
#http://rnotr.com/likert/ggplot/barometer/likert-plots/

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)

factor <- read.csv("Data/Processed/factor1.csv")

mytitle <- "Factors that affect sharing decision"
mylevels <- c("Not at all affect", "Rarely affect", "Neutral", "Somewhat affect", "Affect a lot")

numlevels<-length(factor[1,])-1
numcenter<-ceiling(numlevels/2)+1
factor$midvalues<-factor[,numcenter]/2
factor2<-cbind(factor[,1],factor[,2:ceiling(numlevels/2)],
            factor$midvalues,factor$midvalues,factor[,numcenter:numlevels+1])
colnames(factor2)<-c("outcome",mylevels[1:floor(numlevels/2)],"midlow",
                  "midhigh",mylevels[numcenter:numlevels])

numlevels<-length(mylevels)+1
point1<-2
point2<-((numlevels)/2)+1
point3<-point2+1
point4<-numlevels+1
mymin<-(ceiling(max(rowSums(factor2[,point1:point2]))*4)/4)*-100
mymax<-(ceiling(max(rowSums(factor2[,point3:point4]))*4)/4)*100

numlevels<-length(factor[1,])-1
temp.rows<-length(factor2[,1])
pal<-brewer.pal((numlevels-1),"RdBu")
pal[ceiling(numlevels/2)]<-"#DFDFDF"
legend.pal<-pal
pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
       pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)])

factor3<-melt(factor2,id="outcome")
factor3$col<-rep(pal,each=temp.rows)
factor3$value<-factor3$value*100
factor3$outcome<-str_wrap(factor3$outcome, width = 40)
factor3$outcome<-factor(factor3$outcome, levels = factor2$outcome[order(-(factor2[,5]+factor2[,6]+factor2[,7]))])
highs<-na.omit(factor3[(length(factor3[,1])/2)+1:length(factor3[,1]),])
lows<-na.omit(factor3[1:(length(factor3[,1])/2),])
lows <- lows[rev(rownames(lows)),]
lows$col <- factor(lows$col, levels = c("#CA0020","#F4A582", "#DFDFDF"))

ggplot() + 
  geom_bar(data=highs, aes(x = outcome, y=value, fill=col), position="stack", stat="identity") +
  geom_bar(data=lows, aes(x = outcome, y=-value, fill=col), position="stack", stat="identity") +
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_identity("Percent", labels = mylevels, breaks=legend.pal, guide="legend") + 
  theme_fivethirtyeight() + 
  coord_flip() +
  labs(title=mytitle, y="",x="") +
  theme(plot.title = element_text(size=14, hjust=0.5)) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax))

