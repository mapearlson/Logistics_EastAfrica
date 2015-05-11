setwd("~/Documents/Delma")
install.packages("ggplot2")
library(stats)
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
e2014.m <- e2014
e2014.m <- subset(e2014, select = c(country, overall_score, customs_score, infra_score, shipments_score,
                                    log_quality_score, tracking_score, timeliness_score))
names(e2014.m) <- c("country", "Overall", "Customs", "Infrastructure", "Shipments", "Quality", "Tracking", "Timeliness")
e2014.m <- melt(e2014.m, id.vars='country')
cbbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#333BFF", "#CC6600", "#9633FF", "#E2FF33")
ggplot(e2014.m, aes(variable, value)) + geom_bar(aes(fill = country), position = "dodge", stat="identity") + scale_fill_manual(values=cbbPalette) +
  ggtitle("East African LPI Scores") + xlab("Components") + ylab("Scores")

#Comparison to other global regions
y2014.2 <- y2014
y2014.2$region <- NA

#Subset East Africa
y2014.2$region[y2014$country=="Germany" | y2014$country=="Belgium" | 
               y2014$country=="Bulgaria" | y2014$country=="Croatia" | 
                y2014$country=="Cyprus" | y2014$country=="Cyprus" | 
                y2014$country=="Czech Republic" | y2014$country=="Denmark"|
                 y2014$country=="Estonia" | y2014$country=="Finland" |
               y2014$country=="France" |  y2014$country=="Austria" |
                 y2014$country=="Greece" | y2014$country=="Hungary" |
                 y2014$country=="Ireland" | y2014$country=="Italy" |
                 y2014$country=="Latvia" | y2014$country=="Lithuania" |
               y2014$country=="Luxembourg" | y2014$country=="Malta" |
               y2014$country=="Netherlands" | y2014$country=="Poland" |
                 y2014$country=="Romania" | y2014$country=="Slovakia" |
                 y2014$country=="Slovenia" | y2014$country=="Spain" |
                 y2014$country=="Sweden" | y2014$country=="United Kingdom"] <- "EU"

#Subset East Africa
y2014.2$region[y2014$country=="Burundi" | y2014$country=="Dijibouti"] <- "EA"
y2014.2$region[y2014$country=="Eritrea" | y2014$country=="Ethiopia"] <- "EA"
y2014.2$region[y2014$country=="Kenya" | y2014$country=="Madagascar"] <- "EA"
y2014.2$region[y2014$country=="Malawi" | y2014$country=="Mozambique"] <- "EA"
y2014.2$region[y2014$country=="Rwanda" | y2014$coutnry=="Somalia"] <- "EA"
y2014.2$region[y2014$country=="Tanzania" | y2014$country=="Zambia"] <- "EA"
y2014.2$region[y2014$country=="Zimbabwe"] <- "EA"

#Subset Latin America
y2014.2$region[y2014$country=="Argentina" | y2014$country=="Belize"] <- "LA" 
y2014.2$region[y2014$country=="Bolivia" | y2014$country=="Brazil"] <- "LA" 
y2014.2$region[y2014$country=="Chile" | y2014$country=="Colombia"] <- "LA"
y2014.2$region[y2014$country=="Costa Rica" | y2014$country=="Ecuador"] <- "LA"
y2014.2$region[y2014$country=="El Salvador" | y2014$coutnry=="Guatemala"] <- "LA"
y2014.2$region[y2014$country=="Honduras" | y2014$country=="Mexico"] <- "LA"
y2014.2$region[y2014$country=="Nicaragua" | y2014$country=="Panama"] <- "LA"
y2014.2$region[y2014$country=="Paraguay" | y2014$country=="Peru"] <- "LA"
y2014.2$region[y2014$country=="Uruguay" | y2014$country=="Venezuela"] <- "LA"

#Subset Subsaharan-Africa
y2014.2$region[y2014$country=="Angola" | y2014$country=="Benin" | y2014$country=="Botswana"] <- "SA" 
y2014.2$region[y2014$country=="Burkina Faso" | y2014$country=="Cameroon" | y2014$country=="Central African Republic" ] <- "SA"
y2014.2$region[y2014$country=="Chad" | y2014$country=="Congo" | y2014$country=="Côte d'Ivoire"] <- "SA"
y2014.2$region[y2014$country=="Dem. Rep. of the Congo" | y2014$country=="Equatorial Guinea"] <- "SA"
y2014.2$region[y2014$country=="Gabon" | y2014$country=="Ghana" | y2014$country=="Guinea-Bissau"] <- "SA"
y2014.2$region[y2014$country=="Lesotho" | y2014$country=="Liberia" | y2014$country=="Malawi"] <- "SA"
y2014.2$region[y2014$country=="Nambia" | y2014$country=="Nigeria"] <- "SA"
y2014.2$region[y2014$country=="Senegal" | y2014$country=="Sierra Leone"| y2014$country=="South Africa"] <- "SA"
y2014.2$region[y2014$country=="South Sudan" | y2014$country=="Swaziland" | y2014$country=="Togo" | y2014$country=="Uganda"] <- "SA"

y2014.2m <- na.omit(y2014.2)
y2014.2m <- subset(y2014.2m, select = c(region, overall_score, customs_score, infra_score, shipments_score,
                                    log_quality_score, tracking_score, timeliness_score))
y2014.2m <- melt(y2014.2m, id.vars='region')
ggplot(y2014.2m, aes(variable, value)) + geom_bar(aes(fill = region), position = "dodge", stat="identity")

##Exports of goods and services as a percentage of gdp
gdp <- read.csv("gdp.csv")
names(gdp) <- c("country", "code", "year", "gdp")
y2012.2 <- merge(y2012, gdp, by="country")
y2012.2$lngdp <- log(y2012.2$gdp)
qplot(lngdp, overall_score, data = y2012.2, geom = c("point", "smooth"))

#y2012.2 <- subset(y2012.2, select = c(region, overall_score, customs_score, infra_score, shipments_score,
#log_quality_score, tracking_score, timeliness_score))
#Graph using ggplot instead of qplot for spline
ggplot(y2012.2, aes(x=lngdp, y=overall_score)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region

#3d plot with FDI
fdi <- read.csv("fdi.csv")
names(fdi) <- c("country", "year", "fdi")
y2012.3 <- merge(y2012.2, fdi, by="country")

install.packages("Rcmdr")
library(Rcmdr)
library(rgl)
scatter3d(y2012.3$overall_score, y2012.3$lngdp, y2012.3$fdi)

install.packages("scatterplot3d")
library(scatterplot3d) 
s3d <-scatterplot3d(y2012.3$fdi, y2012.3$lngdp, y2012.3$overall_score, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")

#Linear Model (y2012.3$overall_score~y2012.3$fdi + y2012.3$lngdp)
lm.fit <- lm(overall_score~ fdi + lngdp, data=y2012.3)
summary(lm.fit)

#Seems to be one possibly problematic outlier when including lnGDP, overall_score, and fdi
qqPlot(lm.fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(lm.fit) # leverage plots
qqnorm(lm.fit$res)
qqline(lm.fit$res)
hist(lm.fit$res)

plot(, lm.fit$res)
plot(Lot, results$res)
plot(lm.fit$fitted, lm.fit$res)
points(lm.fit$fitted[22], lm.fit$res[22], col='red')

cook <- cooks.distance(lm.fit)
plot(cook,ylab="Cooks distances")
points(22,cook[22],col="red")

#Burundi is the outlier, analysis again exluding Burundi
y2012.4 <- y2012.3[-22,]
lm.fit2 <- lm(lngdp ~ fdi + overall_score, data=y2012.4)
summary(lm.fit2)

#Many of the countries receive 0 in FDI which is problematic; exlcuding F~DI
lm.fit3 <- lm(lngdp ~ overall_score, data=y2012.4)
summary(lm.fit3)

#Call:
#  lm(formula = lngdp ~ overall_score, data = y2012.4)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.3589 -0.9628 -0.0418  0.9907  4.3467 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    17.0139     0.6626   25.68   <2e-16 ***
#  overall_score   2.7024     0.2256   11.98   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.515 on 146 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.4956,  Adjusted R-squared:  0.4922 
#F-statistic: 143.5 on 1 and 146 DF,  p-value: < 2.2e-16

#Biased upward, but the beta of 2.7024 when exponentiated 
#can be interpreted as a 15% increase in GDP($).  
plot(y2012.4$overall_score, y2012.4$lngdp, xlab="Overall Score", ylab="Ln(GDP)", main="Figure 3: OLS for All Countries")  
abline(lm(y2012.4$lngdp ~ y2012.4$overall_score), col='red')

scatterplot3d(y2012.4$fdi, y2012.4$lngdp, y2012.4$overall_score, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")

cook2 <- cooks.distance(lm.fit3)
plot(cook2,ylab="Cooks distances")

#Log-log Model
y2012.4$lnscore <- log(y2012.4$overall_score)
lm.log.log <- lm(lngdp ~ lnscore, data=y2012.4)
summary(lm.log.log)

#Call:
#  lm(formula = lngdp ~ lnscore, data = y2012.4)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.4431 -1.0507 -0.0914  0.9929  4.2699 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  16.5302     0.7090   23.32   <2e-16 ***
#  lnscore       7.9477     0.6699   11.86   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.523 on 146 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.4909,  Adjusted R-squared:  0.4874 
#F-statistic: 140.8 on 1 and 146 DF,  p-value: < 2.2e-16

#We expect a change in overall score of 1% to change the GDP by 8 percent

#Go back in to isolated 2012 East Africa
#Comparison to other global regions
y2012.5 <- y2012.4
y2012.5$region <- NA

#Subset EU
y2012.5$region[y2012.4$country=="Germany" | y2012.4$country=="Belgium" | 
                 y2012.4$country=="Bulgaria" | y2012.4$country=="Croatia" | 
                 y2012.4$country=="Cyprus" | y2012.4$country=="Cyprus" | 
                 y2012.4$country=="Czech Republic" | y2012.4$country=="Denmark"|
                 y2012.4$country=="Estonia" | y2012.4$country=="Finland" |
                 y2012.4$country=="France" |  y2012.4$country=="Austria" |
                 y2012.4$country=="Greece" | y2012.4$country=="Hungary" |
                 y2012.4$country=="Ireland" | y2012.4$country=="Italy" |
                 y2012.4$country=="Latvia" | y2012.4$country=="Lithuania" |
                 y2012.4$country=="Luxembourg" | y2012.4$country=="Malta" |
                 y2012.4$country=="Netherlands" | y2012.4$country=="Poland" |
                 y2012.4$country=="Romania" | y2012.4$country=="Slovakia" |
                 y2012.4$country=="Slovenia" | y2012.4$country=="Spain" |
                 y2012.4$country=="Sweden" | y2012.4$country=="United Kingdom"] <- "EU"

#Subset East Africa
y2012.5$region[y2012.4$country=="Burundi" | y2012.4$country=="Dijibouti"] <- "EA"
y2012.5$region[y2012.4$country=="Eritrea" | y2012.4$country=="Ethiopia"] <- "EA"
y2012.5$region[y2012.4$country=="Kenya" | y2012.4$country=="Madagascar"] <- "EA"
y2012.5$region[y2012.4$country=="Malawi" | y2012.4$country=="Mozambique"] <- "EA"
y2012.5$region[y2012.4$country=="Rwanda" | y2012.4$coutnry=="Somalia"] <- "EA"
y2012.5$region[y2012.4$country=="Tanzania" | y2012.4$country=="Zambia"] <- "EA"
y2012.5$region[y2012.4$country=="Zimbabwe"] <- "EA"

#Subset Latin America
y2012.5$region[y2012.4$country=="Argentina" | y2012.4$country=="Belize"] <- "LA" 
y2012.5$region[y2012.4$country=="Bolivia" | y2012.4$country=="Brazil"] <- "LA" 
y2012.5$region[y2012.4$country=="Chile" | y2012.4$country=="Colombia"] <- "LA"
y2012.5$region[y2012.4$country=="Costa Rica" | y2012.4$country=="Ecuador"] <- "LA"
y2012.5$region[y2012.4$country=="El Salvador" | y2012.4$coutnry=="Guatemala"] <- "LA"
y2012.5$region[y2012.4$country=="Honduras" | y2012.4$country=="Mexico"] <- "LA"
y2012.5$region[y2012.4$country=="Nicaragua" | y2012.4$country=="Panama"] <- "LA"
y2012.5$region[y2012.4$country=="Paraguay" | y2012.4$country=="Peru"] <- "LA"
y2012.5$region[y2012.4$country=="Uruguay" | y2012.4$country=="Venezuela"] <- "LA"

#Subset Subsaharan-Africa
y2012.5$region[y2012.4$country=="Angola" | y2012.4$country=="Benin" | y2012.4$country=="Botswana"] <- "SA" 
y2012.5$region[y2012.4$country=="Burkina Faso" | y2012.4$country=="Cameroon" | y2012.4$country=="Central African Republic" ] <- "SA"
y2012.5$region[y2012.4$country=="Chad" | y2012.4$country=="Congo" | y2012.4$country=="Côte d'Ivoire"] <- "SA"
y2012.5$region[y2012.4$country=="Dem. Rep. of the Congo" | y2012.4$country=="Equatorial Guinea"] <- "SA"
y2012.5$region[y2012.4$country=="Gabon" | y2012.4$country=="Ghana" | y2012.4$country=="Guinea-Bissau"] <- "SA"
y2012.5$region[y2012.4$country=="Lesotho" | y2012.4$country=="Liberia" | y2012.4$country=="Malawi"] <- "SA"
y2012.5$region[y2012.4$country=="Nambia" | y2012.4$country=="Nigeria"] <- "SA"
y2012.5$region[y2012.4$country=="Senegal" | y2012.4$country=="Sierra Leone"| y2012.4$country=="South Africa"] <- "SA"
y2012.5$region[y2012.4$country=="South Sudan" | y2012.4$country=="Swaziland" | y2012.4$country=="Togo" | y2012.4$country=="Uganda"] <- "SA"

y2012.5m <- na.omit(y2012.5)
y2012.5m <- subset(y2012.5m, select = c(region, overall_score, customs_score, infra_score, shipments_score,
                                        log_quality_score, tracking_score, timeliness_score))
y2012.5m <- melt(y2012.5m, id.vars='region')
ggplot(y2014.2m, aes(variable, value)) + geom_bar(aes(fill = region), position = "dodge", stat="identity")

#Subset EA for correlation and LM
y2012.5.EA <- subset(y2012.5, region=="EA")
cor(y2012.5.EA$lngdp, y2012.5.EA$overall_score)
cor(y2012.5.EA)
cor(y2012.5.EA$lngdp, y2012.5.EA$overall_score, use="complete.obs")

lm.fit.EA <- lm(lngdp ~ overall_score, data=y2012.5.EA)
summary(lm.fit.EA)

#Subset EA and SAfor correlation and LM
y2012.5.Af <- subset(y2012.5, region==c("EA", "SA"))
lm.fit.Af <- lm(lngdp ~ overall_score, data=y2012.5.Af)
summary(lm.fit.Af)

y2012.5$lnscore <- log(y2012.5$overall_score)
y2012.5.Af$lnscore <- log(y2012.5.Af$overall_score)
lm.fit.Aflog <- lm(lngdp ~ lnscore, data=y2012.5.Af)
summary(lm.fit.Aflog)
