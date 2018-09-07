# Analysis of MBA SALARIES
# NAME: Mukul Milind Puranik  
# EMAIL: puranikmukul8@gmail.com
# COLLEGE / COMPANY: Dwarkadas J. Sanghvi College of Engineering. (DJSCOE)


mbasal.df <- read.csv(paste("MBA Starting Salaries Data.csv", sep=""))
View(mbasal.df)

summary(mbasal.df)
library(psych)
describe(mbasal.df)
boxplot(mbasal.df$age, horizontal=TRUE,
        xlab="", las=1,main="Ages")
boxplot(mbasal.df$gmat_tot, horizontal=TRUE,
        xlab="", las=1,main="Total Gmat Scores")
boxplot(mbasal.df$gmat_tpc, horizontal=TRUE,
        xlab="", las=1,main="Gmat Total percentile")
boxplot(mbasal.df$salary, horizontal=TRUE,
        xlab="", las=1,main="Salary")
boxplot(mbasal.df$work_yrs, horizontal=TRUE,
        xlab="", las=1,main="Experience")


## Checking the effect of prior Work experience on salary received by adding a new column to the dataframe called isPlaced.
## 999 is counted as placed since they don't want to disclose their salary, but that indeed tells us that they are placed.
## Also 998 are considered as not placed since we don't know anything about them.
newcol = as.integer(mbasal.df$salary > 0 & mbasal.df$salary!=998)
newcol
mbasal.df$isPlaced <- newcol
View(mbasal.df)
newdata1 <- mbasal.df[ which(mbasal.df$salary >= 1000), ]##disregarding undisclosed salaries and those who didnt get any salary, to check if there is a relation between number of prior experience to the final placed people's salary.
newdata1
# Plotting Work Years vs Salary earned.
plot(newdata1$work_yrs,newdata1$salary,
     cex=0.7, 
     main="Effect of work exp on salary", 
     xlab="Work_yrs", 
     ylab="Salary" )
line<-lm(newdata1$salary~newdata1$work_yrs)
abline(line)
summary(line)
##
mydata1 = mbasal.df[ which(mbasal.df$isPlaced == 0), ]
mydata2 = mbasal.df[ which(mbasal.df$isPlaced== 1), ]
par(mfrow=c(1,2))
with(mydata1, hist(mydata1$age, 
                   main="Age of Non-placed Students",    # add labels
                   xlab="Age",
                   ylab="Count", 
                   breaks=3,
                   col="grey") )
with(mydata2, hist(mydata2$age, 
                    main="Age of placed students",    # add labels
                    xlab="Age",
                    ylab="Count", 
                    breaks=3,
                    col="grey") )## as it can be seen from the histograms, age doesnt play a factor in acquiring jobs for MBA graduates of this university. Since in both the cases## the amount of students placed and nonplaced is quite similar for all ages.
par(mfrow=c(1,1))

library(corrgram)
cor(mbasal.df)
corrgram(mbasal.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of MBA Students Data")
## Looking at the corrgram it can be infered that indeed if the student is not placed, his satisfaction of the MBA program is less. This is quite obvious but an important insight nevertheless.

mydata2
mytable <- xtabs(~ salary+quarter, data=mydata2)
margin.table(mytable,2)
addmargins(mytable)
plot(newdata1$quarter,newdata1$salary,
     cex=0.7, 
     main="Effect of Percentile on salary", 
     xlab="Quarter", 
     ylab="Salary" )
line<-lm(newdata1$salary~newdata1$quarter)
abline(line)
summary(line)
## It is evident from the mytable table that indeed the people in who are in 1st quartile have secured higher number of jobs.

## Plotting only those who have disclosed their salary against the satisfaction rating to draw interpretations, since if we include 999 it could lead to discrepancies in the linear regression model.

plot(newdata1$satis,newdata1$salary,
     cex=0.7, 
     main="Effect of Satisfaction on salary", 
     xlab="Satisfaction", 
     ylab="Salary" )
line<-lm(newdata1$salary~newdata1$satis)
abline(line)
summary(line)
mytable1 <- xtabs(~ salary+satis, data=mydata2)
addmargins(mytable1)
## satisfaction has no relation with the salary hence proved. Since the p value is >0.01 and the r squared value is a mere 0.001.


## Checking against those who scored well in MBA wrt to their starting salary amongst those who got placed.

plot(mydata2$f_avg,mydata2$salary,
     cex=0.7, 
     main="Effect of MBA Scores on salary", 
     xlab="Score", 
     ylab="Salary" )
line<-lm(mydata2$salary~mydata2$f_avg)
abline(line)
summary(line)

## no significant effect.

## checking with respect to gender.

plot(newdata1$sex,newdata1$salary,
     cex=0.7, 
     main="Effect of Gender on salary", 
     xlab="Gender", 
     ylab="Salary" )
line<-lm(newdata1$salary~newdata1$sex)
abline(line)
summary(line)
## checking if indeed the null hypothesis of gender independence with respect to salary value is indeed true using Chi squared test.
genderTable <- xtabs(~salary+sex, data=newdata1)
salary.mean <- aggregate(salary ~ sex, data=newdata1, mean)
salary.mean
t.test(newdata1$salary~newdata1$sex)
chisq.test(genderTable)
## p value > 0.01 hence indeed the null hypothesis is true that it isnt gender dependent.
## Checking with respect to gmat total score
plot(newdata1$gmat_tpc,newdata1$salary,
     cex=0.7, 
     main="Effect of Gmat on salary", 
     xlab="Gmat", 
     ylab="Salary" )
line<-lm(newdata1$salary~newdata1$gmat_tpc)
abline(line)
summary(line)
#not effective. P>0.01

plot(newdata1$frstlang,newdata1$salary,
     cex=0.7, 
     main="Effect of Language on salary", 
     xlab="Gender", 
     ylab="Salary" )
line<-lm(newdata1$salary~newdata1$frstlang+newdata1$gmat_tpc+newdata1$sex)
abline(line)
summary(line)
## Somewhat effective with P<0.01 but the Rsquared value is very low.## Hence many errors involved.

##The best model that fits the data of placed students is that which is used related to work_years.

mydata3 = mbasal.df[ which(mbasal.df$salary!=998), ]
gotjob= mydata3[ which(mydata3$isPlaced == 1),]
notgotjob=mydata3[ which(mydata3$isPlaced == 0),]
jobchecker <- xtabs(~ isPlaced+satis, data=mydata3)
addmargins(jobchecker)
chisq.test(jobchecker)## p>0.01 hence we can safely conclude that the null hypothesis is true that indeed the placement value is independent of satisfaction level.


jobchecker
## Experiments down there, can be ignored.
##jobchecker <- xtabs(~ isPlaced+age, data=mydata3)
mydata3<-mbasal.df
i=1;
while(i<=274){
  if(mbasal.df[i,]$salary==998)
  {
    newdata123[i]<-binary123(2);
    i <- i+1;
  }
  else
  {
    newdata123[i] <- mbasal.df[i,]$isPlaced;
    i<- i+1;
  }
}## Randomising the isplaced for data which has no response.
mbasal.df[1,]
     warnings()             
newdata123
binary123<-function(y){
 y <- sample(0:1, 1, replace=TRUE)
 return(y)
}
##temp$newsalary<- newdata123
##newdata2 <- temp[ which(temp$news > 0), ]
##temp<-mbasal.df
##newdata2
media
mydata3$isPlaced <- newdata123
mydata3$satis <-newcol123123
mydata3
summary(mydata3)
View(mydata3)
train <- mydata3[1:199,]
test <- mydata3[200:274,]
model <- glm(isPlaced ~.,family=binomial(link='logit'),data=train)
newcol123123 = ifelse(mydata3$salary==998  , mean(mydata3$salary), mydata3$salary)
summary(model)
##View(titanic.df)
##train <- titanic.df[1:800,]
##test <- titanic.df[801:889,]
##model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
##summary(model)


anova(model, test="Chisq")
fitted.results <- predict(model,newdata=subset(test,select=c(1:13)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$isPlaced)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix
library(caret)
confusionMatrix(data=fitted.results, reference=test$isPlaced)
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(1:13)), type="response")
pr <- prediction(p, test$isPlaced)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
