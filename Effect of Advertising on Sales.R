## ----read----------------------------------------------------------------
# Read the data
advertising.df <- read.csv(paste("AdvertisingData.csv", sep=""))
View(advertising.df)

## ----summarize-----------------------------------------------------------
# summarize the data
attach(advertising.df)
library(psych)
describe(advertising.df)[,1:5]

## ------------------------------------------------------------------------
# checking  data types of the data fields
str(advertising.df)

## ------------------------------------------------------------------------
Model1 <- Sales ~ TV + Radio + Newspaper
fit1 <- lm(Model1, data = advertising.df)
summary(fit1)


## ------------------------------------------------------------------------
# R-squared
summary(fitTV)$r.squared 

# F-statistic
summary(fitTV)$fstatistic

## ------------------------------------------------------------------------
# p-values of  Model 1
summary(fit1)$coefficients[,4]   

## ------------------------------------------------------------------------
# confidence interval
confint(fit1)

