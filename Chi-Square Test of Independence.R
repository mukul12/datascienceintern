## ------------------------------------------------------------------------
library(vcd)

## ----(i) Arthritis dataset-----------------------------------------------
library (vcd)
head(Arthritis)
titanic.df <- read.csv(paste("Titanic_data.csv", sep=""))
 dean.df<- read.csv(paste("Data - Deans Dilemma.csv", sep=""))
 summary(dean.df)
 View(titanic.df)
View(dean.df)
deantable <- with(dean.df, table(Placement))
deantable
prop.table(deantable)
newdata <- dean.df[ which(dean.df$Placement_B=='1'), ]
summary(newdata)
table1 <- xtabs(~Gender+Salary , data=dean.df)
table1<-table(mean(newdata$Salary), newdata$Gender.B)
aggregate(newdata$Salary, by=list(Gender = newdata$Gender), mean)

hist(newdata$Percent_MBA, 
     main="MBA Performance of placed students",    # add labels
     xlab="MBA Percentage",
     ylab="Count", 
     breaks=3,
     col="grey") 
newdata1 <- dean.df[ which(dean.df$Placement_B=='0'), ]


par(mfrow=c(1,2))
with(newdata, hist(newdata$Percent_MBA, 
                   main="MBA Performance of placed students",    # add labels
                   xlab="MBA Percentage",
                   ylab="Count", 
                   breaks=3,
                   col="grey") )
with(newdata1, hist(newdata1$Percent_MBA, 
                    main="MBA Performance of Non-placed students",    # add labels
                    xlab="MBA Percentage",
                    ylab="Count", 
                    breaks=3,
                    col="grey") )
par(mfrow=c(1,1))


boxplot(newdata$Salary ~ newdata$Gender, horizontal=TRUE,
        ylab="Gender", xlab="Salary", las=1,
        main="Comparison of Salaries of Males and Females")
newdata2 <- dean.df[ which(dean.df$Placement_B=='1' & dean.df$S.TEST=="1"), ]
library(car)
scatterplotMatrix(formula = ~ Salary + Percent_MBA + Percentile_ET, cex=1.3,
                  data=newdata2, diagonal="density")
library(psych)


##Day2-1
library(MASS)
library(psych)
View(UScrime)
attach(UScrime)
table(So)
## So
##  0  1 
## 31 16
summary(Prob)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00690 0.03270 0.04210 0.04709 0.05445 0.11980
boxplot(Prob ~ So, data=UScrime, xlab="Southern State (yes=1, no=0)", ylab="Prob. of Imprisonment")
aggregate(UScrime$Prob, by=list(UScrime$So), FUN=mean)
##   Group.1          x
## 1       0 0.03851265
## 2       1 0.06371269
t.test(Prob ~ So, data=UScrime)
aggregate(Salary~Gender,data=newdata,FUN = mean)
t.test(Salary ~ Gender.B, data=dean.df)


aggregate(Age~Survived,data=titanic.df,FUN = mean)
t.test(titanic.df$Age,titanic.df$Survived, paired = TRUE)
t.test(U1, U2, paired=TRUE)

##-------------------------------------------------------------------------------------
store.df<- read.csv(paste("Store24.csv", sep=""))

summary(store.df$Profit)
sd(store.df$Profit)
sd(store.df$MTenure)
sd(store.df$CTenure)
summary(store.df$MTenure)
summary(store.df$CTenure)
newdata1 <- store.df[order(store.df$Profit),]
newdata2 <- store.df[order(-store.df$Profit),]
head(newdata1,10)
head(newdata2,10)

library(MASS)


plot(store.df$Profit, cust.df$online.spend,
     cex=0.7,
     col=my.col[cust.df$email], pch=my.pch[cust.df$email], 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)" )
library(ggplot2)
df <- data.frame(MTenure = store.df$MTenure, Profit = store.df$Profit)
p <- ggplot(df, aes(MTenure, Profit)) + geom_point() + theme_classic()
ggExtra::ggMarginal(p, type = "boxplot")
regline <- lm(Profit ~ MTenure, data = store.df)
abline(regline)

cor(store.df[, c(1:14)])
cor.test(store.df$Profit, store.df$MTenure)
cor.test(store.df$Profit, store.df$CTenure)
line <- lm(Profit~ MTenure +  CTenure + Comp + Pop + PedCount + Res + Hours24 + Visibility, data=store.df)
line$coefficients
summary(line)
abline(line)
library(corrgram)
corrgram(newdata1, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Store variables")

plot(cust.df$sat.service, cust.df$sat.selection, 
     xlab="Sat, Service", ylab="Sat, Selection")

##--------------------------------------------------------------------------------------
aggregate(titanic.df, by=list(country=store.df$country), sum)
library(psych)

describe(titanic.df)
mytable2<- xtabs(~Survived+Sex, data=titanic.df)
mytable2
addmargins(prop.table(mytable2,2))
a<-table(titanic.df$Survived)
prop.table(a)

mytable <- xtabs(~Survived+Pclass+Sex, data=titanic.df)
mytable
prop.table(addmargins(mytable))
ftable(mytable)
mytable <- xtabs()
ftable(mytable, c(1,3))







## ----(ii) 2-Way Table----------------------------------------------------
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
addmargins(mytable)

## ----(iii) Pearson-------------------------------------------------------
chisq.test(mytable)

## ----(iv) 2-way table----------------------------------------------------
library(vcd)
mytable <- xtabs(~Improved+Sex, data=Arthritis)
addmargins(mytable)

## ----(v) Pearson---------------------------------------------------------
chisq.test(mytable)

