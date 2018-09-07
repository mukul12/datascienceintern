
summary(biopics.df)
library(psych)
attach(biopics.df)
describe(biopics.df)
biopics.df[5]
as.numeric(sub('\\$','',as.character(biopics.df[5])))
biopics.df[as.numeric(sub('\\$','',as.character(c[5])))]
as.numeric(levels(biopics.df[5]))[biopics.df[5]]
i=0;
require(stringi)
x<-biopics.df[5]
stri_sub(x$box_office, -1 )
i=1;
i=i+1;
i
x$box_office[2]
while(i<=761)
{
  if(stri_sub(x$box_office[i], -1 ) =="K"){
    x$box_office[i] <- as.numeric(sub('K','',as.character(x$box_office[i])));
    i=i+1;
  }
  else if(stri_sub(x$box_office[i], -1 ) == "M" ) {
    x$box_office[i] <- as.numeric(sub('M','',as.character(x$box_office[i])))*1000;
    i= i+1;
   
     
  }else{i=i+1;}
}
warnings()
x
as.numeric(levels(x$box_office))[x$box_office]
x$box_office<-sub('\\$','',as.character(x$box_office))
x
as.numeric(sub('K','',as.character(x$box_office)))

stri_sub(x$box_office[4], -1 )

stri_sub(x$box_office[2], -1 )=="M"## c("M")

biopics.df$box_office<- x$box_office
newdata1 <- mbasal.df[ which(mbasal.df$salary >= 1000), ]
bio.df<-biopics.df[which(biopics.df$box_office >=0),]

View(bio.df)
plot(as.numeric(bio.df$subject_sex),bio.df$box_office,
     cex=0.7, 
     main="Effect of work exp on salary", 
     xlab="Type of Subject", 
     ylab="Box Office" )
Model1 <- box_office ~ as.numeric(bio.df$subject_sex)
fit1 <- lm(Model1, data = bio.df)
summary(fit1)
#Here now we have box office collection as the Y variable and x variables are race, gender/sex, year, type of subject
str(bio.df)
as.numeric(levels(bio.df$box_office))[bio.df$box_office]
bio.df$box_office<-as.numeric(as.character(bio.df$box_office))
bio.df
as.numeric(bio.df$type_of_subject)



as.numeric(bio.df$year_release)
abline(fit1)
biopics.df
library("rio")
require(readxl)
library(readxl)
install_formats('xslx')
x<-import("auto-mpg.xslx")
read_excel("auto-mpg.xslx")
read.table("auto.data", fileEncoding="UTF-16", dec=",")
