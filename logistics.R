setwd("~/Documents/Delma")
install.packages("ggplot2")
library(ggplot2)
library(reshape2)
d <- read.csv("eastafrica_logistics.csv")
dw <- read.csv("world_logistics.csv")
names(dw) <- tolower(names(dw))
names(d) <- tolower(names(d))
e2014 <- subset(d, year==2014)
y2014 <- subset(dw, year==2014)
summary(y2014$overall_score)
summary(e2014$overall_score)

#African (e) versus all countries (y) for each year
e2012 <- subset(d, year==2012)
y2012 <- subset(dw, year==2012)
e2010 <- subset(d, year==2010)
y2010 <- subset(dw, year==2010)
e2007 <- subset(d, year==2007)
y2007 <- subset(dw, year==2007)

# get the mean of the first 4 variables, by species
by(y2014, y2014$year, summary)
by(e2014, e2014$year, summary)

#Bar graph of overall_score by country filled with infrastructure score
ggplot(data=e2014, aes(x=country, y=overall_score, fill=infra_score)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")

#Subset and reshape to make clustered bar graph of each component
e2014.m <- subset(e2014, select = c(country, overall_score, customs_score, infra_score, shipments_score,
                                    log_quality_score, tracking_score, timeliness_score))
e2014.m <- melt(e2014.m, id.vars='country')
ggplot(e2014.m, aes(variable, value)) + geom_bar(aes(fill = country), position = "dodge", stat="identity")

#Comparison to other regions



means <- function(year,) {
  x<-as.array(year)
  apply(x,2,mean)
}
  
  
  