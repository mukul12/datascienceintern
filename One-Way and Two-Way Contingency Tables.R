## ----Summarize-----------------------------------------------------------
library(vcd) #this package contains the Arthritis dataset used here
View(Arthritis) # view the Arthritis dataset
library(psych)
describe(Arthritis$Age) # Summary Statistics of the columns in the Arthritis dataset

## ----(i) One-Way Table of Frequency--------------------------------------
mytable1 <- with(Arthritis, table(Improved))
mytable1  # frequencies

## ----(ii) Proportions----------------------------------------------------
prop.table(mytable1) # proportions
prop.table(mytable1)*100 # percentages

## ----(iii) xtabs---------------------------------------------------------
mytable1 <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable1 # frequencies

## ----(iv) Proportions----------------------------------------------------
margin.table(mytable1,1) #row sums
prop.table(mytable1, 1) # row proportions

## ----(v) Proportions-----------------------------------------------------
margin.table(mytable1, 2) # column sums
prop.table(mytable1, 2) # column proportions

## ----(vi) Cell Proportions-----------------------------------------------
prop.table(mytable1) # cell proportions

## ----(vii) Sums----------------------------------------------------------
addmargins(mytable1) # add row and column sums to table

## ----(viii) Cell Proportions---------------------------------------------
addmargins(prop.table(mytable1))

## ----(ix) CrossTable-----------------------------------------------------
library(gmodels) # ensure that this package is installed
CrossTable(Arthritis$Treatment, Arthritis$Improved)

