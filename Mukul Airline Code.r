# Analysis of Airline Ticket Pricing
# NAME: Mukul Milind Puranik  
# EMAIL: puranikmukul8@gmail.com
# COLLEGE / COMPANY: Dwarkadas J. Sanghvi College of Engineering. (DJSCOE)


airline.df <- read.csv(paste("SixAirlinesDataV2.csv", sep=""))
airline.df
View(airline.df)
##finding the number of international flights and domestic ones
table(airline.df$Airline,airline.df$IsInternational)
## Getting the general idea of the data.
summary(airline.df)
## Checking out prospective reasons for the price discrepancy.
boxplot(airline.df$PricePremium, horizontal=TRUE,
        xlab="", las=1,main="PricePremium")
boxplot(airline.df$WidthPremium, horizontal=TRUE,
        xlab="", las=1,main="WidthPremium")
boxplot(airline.df$PitchPremium, horizontal=TRUE,
        xlab="", las=1,main="PitchPremium")
boxplot(airline.df$PriceEconomy, horizontal=TRUE,
        xlab="", las=1,main="PriceEconomy")
boxplot(airline.df$WidthEconomy, horizontal=TRUE,
        xlab="", las=1,main="WidthEconomy")
boxplot(airline.df$PitchEconomy, horizontal=TRUE,
        xlab="", las=1,main="PitchEconomy")
table(airline.df$PitchPremium)

## Difference in prices based on airlines
plot(airline.df$Airline, airline.df$PriceEconomy,
     cex=0.7, 
     main="Price Difference per airline", 
     xlab="Airline", 
     ylab="PriceRelative" )

library(car)
#Checking out the changes in prices based on the airlines
scatterplotMatrix(formula = ~ Airline + PriceEconomy + PricePremium, cex=0.6,
                  data=airline.df)
scatterplotMatrix(formula = ~ Airline + PriceRelative, cex=0.6,
                  data=airline.df)
scatterplotMatrix(formula = ~ WidthDifference + PriceRelative, cex=0.6,
                  data=airline.df)

library(corrgram)
cor(airline.df[, c(3,6:18)])
corrgram(airline.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Airline variables")

#seeing the relative difference between the prices based on if the flight is international or domestic
plot(airline.df$IsInternational, airline.df$PriceRelative,
     cex=0.7, 
     main="Price Difference based on type of Flight", 
     xlab="Type", 
     ylab="RelativePrice" ) ## Hence it can be seen that the International flights tend to contribute alot towards
                    ##the price difference between Premium and Economy.Since the relative value for domestic flights is zero.

##Checking whether the price discrepancy indeed is airline relative.
plot(airline.df$Airline, airline.df$PriceRelative,
     cex=0.7, 
     main="Price Difference per airline", 
     xlab="Airline", 
     ylab="Price" )
## from the plot we can easily conclude that indeed some airlines(like Air France or Delta) do not have considerable change in their prices 
## between the economy and the premium seats. Hence Airline also contributes towards the effective change in prices of Economy and Premium.


mytable <- xtabs(~ Airline+PriceRelative+IsInternational, data=airline.df)
mytable
summary(mytable)

t.test(airline.df$PriceRelative~airline.df$IsInternational)
## Since p<0.01 we can sufficiently conclude that the relative price depends on if the price is international or not.



t.test(airline.df$WidthEconomy,airline.df$WidthPremium)
## from the above T tests we get to know that indeed there is a difference in width length of premium and economy seats, with the width of Premium seats being larger.
## Checking the effect of width difference with the change in relative price
plot(airline.df$WidthDifference, airline.df$PriceRelative,
     cex=0.7, 
     main="Relative Change in Price per change in width", 
     xlab="Width Difference", 
     ylab="Relative Price" )## As you can see from the given plot the higher the change in the width between premium and economy, the higher is the relative price.
##This indicates that indeed the width of the seat in premium does have a considerable factor when considering difference in price of the seat.
line<-lm(airline.df$PriceRelative~airline.df$WidthDifference)
summary(line)
#the given regression line also indicates that indeed the higher the difference in the width between premium and economy, higher is the price, thereby indicating that 
# the prices are affected by the width of the seat. Also since p< 0.01 it is indicative that both the coefficients are of considerable weightage.
abline(line)

## doing the same analysis as above for pitch difference, checking if the pitch difference plays a significant role in determining the price change.
t.test(airline.df$PitchEconomy,airline.df$PitchPremium)
## Different means hence reject null hypothesis that there is no difference in the means of the pitch between premium and economy seats. It also shows that since p<0.01 we reject the null hypothesis and accept that in general the pitch of premium seats is larger.
plot(airline.df$PitchDifference, airline.df$PriceRelative,
     cex=0.7, 
     main="Relative Change in Price per change in pitch", 
     xlab="Pitch Difference", 
     ylab="Relative Price" )
line<-lm(airline.df$PriceRelative~airline.df$PitchDifference)
## Hence pitch difference is also directly proportional to increase in price.
abline(line)
summary(line)

plot(airline.df$PercentPremiumSeats, airline.df$PriceRelative,
     cex=0.7, 
     main="Price", 
     xlab="Percent Premium seats", 
     ylab="Relative Price" )
line<-lm(airline.df$PriceRelative~airline.df$PercentPremiumSeats)
abline(line)
summary(line)

##Here we can see that as the number of premium seats increases in a plane, the price of the same ticket in general decreases,
##This can be seen from the regression line generated. This ofcourse is a general assumption, there are many cases where this won't be true.

agg.data <- aggregate(PriceRelative ~ Airline + IsInternational, 
                      data=airline.df, mean)
agg.data
## Checking Mean price difference between the airlines considering both the domestic as well as international flights.



airline.df <- read.csv(paste("biopics.csv", sep=""))
View(airline.df)
