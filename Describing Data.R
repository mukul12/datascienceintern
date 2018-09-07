###################################
#  ### READING AND VIEWING A DATASET ###
############


############

#1) Read the data using read.csv
store.df <- read.csv(paste("StoreData.csv", sep=""))

#2) View the data frame in R
View(store.df)
############

#3a) Summarize the data using summary()
summary(store.df)

#3b) Summarize the data using describe()
library(psych)
describe(store.df)
describe(store.df$p1sales)
describe(store.df[ , c(2, 4:9)])

#4) Viewing a subset of a data frame
head(store.df)
tail(store.df)
library(car) 
some(store.df) #requires package car 

#5) Dimensions of a data frame
dim(store.df)

#6a) Summarizing a Discrete Variable
table(store.df$p1price)
table(store.df$p1prom)

#6b) 2-way Contingency Table for Discrete Variables
table(store.df$p1price, store.df$p1prom)

#7) Summarizing a Continuous Variable
min(store.df$p1sales)
max(store.df$p1sales)
mean(store.df$p1sales)
sd(store.df$p1sales)
summary(store.df$p1sales)

#8) Aggregate() and by()

# Total Sales of Product 2 by Country
aggregate(store.df$p2sales, by=list(country=store.df$country), sum)

# Average Sales of Project 1 by Store
aggregate(store.df$p1sales, by=list(StoreID = store.df$storeNum), mean)

# Average Sales of Project 1 by Store
by(store.df$p1sales, store.df$storeNum, mean)

# Average Sales of Project 1 by Store and Year (2001, 2002)
by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

#9) apply()
# Average of store.df columns 2-9
apply(store.df[, 2:9], MARGIN=2, FUN=sum)
apply(store.df[, 2:9], 2, mean)

# Standard Deviation of store.df columns 2-9
apply(store.df[, 2:9], 2, sd)

# Applying a User-defined function
apply(store.df[, 2:9], 2, function(x) { mean(x) - median(x) } )


# 10) HISTOGRAMS

# 10a) Draw a Histogram
hist(store.df$p1sales)

# 10a) Add Labels to a Histogram
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",    # add labels
     xlab="Product 1 Sales (Units)",
     ylab="Count" )           

# 10b) Choose how many breaks
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=10,             # more columns 
     col="peachpuff")       # color the bars

# 10c) Choosing colors
colors()


# 10d) Add limits to the axes
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     xlim=c(50,200), ylim=c(0,500),  # add limits to the axes
     breaks=10,             # more columns 
     col="lightblue")       # color the bars

# 10e) Density
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales", ylab="Relative frequency",
     breaks=30, col="lightblue", freq=FALSE)

lines(density(store.df$p1sales, bw=10),  # bw = smoothing
      type="l", col="darkred", lwd=2)    # lwd = line width



############
# 11) BOXPLOTS

# 11a) Draw a BoxPlot
boxplot(store.df$p2sales)


# 11b) Add Labels
boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)

# 11c) Generate Box Plots for each store branch
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
        ylab="Store", xlab="Weekly unit sales", las=1,
        main="Weekly Sales of P2 by Store")

# 11d) Colors
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
        ylab="Store", xlab="Weekly unit sales", las=1,
        main="Weekly Sales of P2 by Store",
        col=c("red","blue","green","yellow")
)

# 11e) Do in-store promotions increase sales?
boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n", 
        ylab="P2 promoted in store?", xlab="Weekly sales",
        main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2), labels=c("No", "Yes"))

