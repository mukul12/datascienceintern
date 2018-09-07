# Project Title:  Influence of Car Features and Specifications on Mileage.
# NAME: Mukul Milind Puranik
# EMAIL: puranikmukul8@gmail.com
# COLLEGE / COMPANY: Dwarkadas J. Sanghvi College of Engineering.(DJSCOE) 



auto.df <- read.csv(paste("autocars.csv", sep=""))
View(auto.df)
summary(autoorig.df)
library(psych)
attach(auto.df)
str(auto.df)
describe(auto.df)

##The dependent variable here is the miles per gallon value



hist(auto.df$cylinders, 
     main="Number of Cylinders in a Engine",    # add labels
     xlab="Cylinders",
     ylab="Count", 
     breaks=5,
     col="grey")
boxplot(auto.df$cylinders, horizontal=TRUE,
        xlab="", las=1,main="Cylinders")

hist(auto.df$weight, 
     main="Weight of Car",    # add labels
     xlab="Weight",
     ylab="Count", 
     breaks=5,
     col="grey")
boxplot(auto.df$weight, horizontal=TRUE,
        xlab="", las=1,main="Weight")
str(auto.df)

##converting the horsepower values into numeric
auto.df$horsepower<-as.numeric(levels(auto.df$horsepower))[auto.df$horsepower]

hist(auto.df$horsepower, 
     main="Horsepower of Car",    # add labels
     xlab="Horsepower",
     ylab="Count", 
     breaks=5,
     col="grey")
boxplot(auto.df$horsepower, horizontal=TRUE,
        xlab="", las=1,main="Horsepower")

hist(auto.df$displacement, 
     main="Displacement of Piston inside Engine of Car",    # add labels
     xlab="Displacement",
     ylab="Count", 
     breaks=3,
     col="grey")
boxplot(auto.df$displacement, horizontal=TRUE,
        xlab="", las=1,main="Displacement")
table(auto.df$cylinders)
table(auto.df$origin)
table(auto.df$model_year)


## Checking out Correlations using Scatter plots
plot(auto.df$acceleration,auto.df$mpg,
     cex=0.7, 
     main="Effect of Acceleration power on Mileage", 
     xlab="Displacement", 
     ylab="Mileage/Miles per gallon" )
line1<-lm(auto.df$mpg~auto.df$acceleration)
abline(line1)
summary(line1)
plot(auto.df$displacement,auto.df$mpg,
     cex=0.7, 
     main="Effect of Displacement on Mileage", 
     xlab="Displacement", 
     ylab="Mileage/Miles per gallon" )
line1<-lm(auto.df$mpg~auto.df$displacement)
abline(line1)
summary(line1)

plot(auto.df$cylinders,auto.df$mpg,
     cex=0.7, 
     main="Effect of Number of Cylinders on Mileage", 
     xlab="X Cylindered Engine", 
     ylab="Mileage/Miles per gallon" )
line<-lm(auto.df$mpg~auto.df$cylinders)
abline(line)
summary(line)

plot(auto.df$horsepower,auto.df$mpg,
     cex=0.7, 
     main="Effect of Engine Horsepower on Mileage", 
     xlab="Horsepower", 
     ylab="Mileage/Miles per gallon" )
line<-lm(auto.df$mpg~auto.df$horsepower)
abline(line)
summary(line)

plot(auto.df$weight,auto.df$mpg,
     cex=0.7, 
     main="Effect of Weight of car on Mileage", 
     xlab="Weight", 
     ylab="Mileage/Miles per gallon" )
line<-lm(auto.df$mpg~auto.df$weight)
abline(line)
summary(line)

plot(auto.df$weight,auto.df$model_year,
     cex=0.7, 
     main="Change in Weight of car over years", 
     xlab="Weight", 
     ylab="Model Year" )
line<-lm(auto.df$model_year~auto.df$weight)
abline(line)
summary(line)

plot(auto.df$origin,auto.df$mpg,
     cex=0.7, 
     main="Effect of Origin on Mileage", 
     xlab="Origin", 
     ylab="Mileage/Miles per gallon" )
line<-lm(auto.df$mpg~auto.df$origin)
abline(line)
summary(line)

plot(auto.df$model_year,auto.df$mpg,
     cex=0.7, 
     main="Effect of Model Year on Mileage", 
     xlab="Model Year", 
     ylab="Mileage/Miles per gallon" )
line<-lm(auto.df$mpg~auto.df$model_year)
abline(line)
summary(line)


## Hence the miles per gallon or the mileage of a car is relatively more dependent on the 
## 1) Weight of the car
## 2) The Displacement(distance between TDC and BDC) of car.
## 3) The horsepower of the car.
## 4)
autoorig<-auto.df
library(corrgram)
autoorig$horsepower<-ifelse(is.na(auto.df$horsepower)  , 104.5, auto.df$horsepower)## just adding to see correlation.

##correlation matrix
cor(autoorig[, c(1,3:5)])

corrgram(autoorig[c(1,3:5)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Mileage Data")
auto.df<-autoorig

str(autoorig)
##-----------------------------------------------------------------------------

## Highest accuracy for a multiregression model we are getting for weight+model_year+ origin
## It seems that this model is actually not plotting the factors responsible for fuel consumption but rather its showing the trend
## and the upgradations in the automobile industry, like the trends of increasing fuel efficient engines in subsequent years(as shown by model_years)
## also the origin is the model number eg: 1,2,3,4 of the same car, hence as the origin goes on increasing, its quite obvious to see the Fuel Efficiency to go on increasing(i.e mpg)
## Last but not the least the model correctly shows the change in weight accross the years leading to Higher MPG.
Model1 <- mpg ~  weight + model_year + origin
fit1 <- lm(Model1, data = auto.df)
summary(fit1)

## This model is the relation we see in order to determine what constitutes as an attribute which has significant role in determining fuel efficiency.
Model2<- mpg~ weight + horsepower + horsepower*weight
fit2 <- lm(Model2, data = auto.df)
summary(fit2)

