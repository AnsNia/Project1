######################################## FINAL PROJECT######################################

######### LOADING THE DATA FILE ---------- 
SprStr <- read.csv("C:/Users/Anshum/Downloads/new supermarket data/SampleSuperstore.csv")
SprStr

str(SprStr)
summary(SprStr)

sum(SprStr$Sales)
sum(SprStr$Profit)

## Changing the class of some columns from character to factor
SprStr$Ship.Mode <- as.factor(SprStr$Ship.Mode)
SprStr$Segment <- as.factor(SprStr$Segment)
SprStr$State <- as.factor(SprStr$State)
SprStr$City <- as.factor(SprStr$City)
SprStr$Region <- as.factor(SprStr$Region)
SprStr$Category <- as.factor(SprStr$Category)
SprStr$Sub.Category <- as.factor(SprStr$Sub.Category)
SprStr$Country <- as.factor(SprStr$Country)

summary(SprStr)

is.null(SprStr)


library(tidyverse)
library(dplyr)

####  Removing Country and Postal.Code columns from the data2
SprStr2 = select(SprStr, -Country, -Postal.Code)
SprStr2
summary(SprStr2)


## New dataset containing only Sales, Discount AND Profit
SprStr_num <- SprStr2 %>% select(Sales, Discount, Profit, Quantity)
SprStr_num
summary(SprStr_num)

#######  install Hmisc package #####
library(Hmisc)


##  Hmisc package function
describe(SprStr2$Ship.Mode)
describe(SprStr2$Segment)
describe(SprStr2$City)
describe(SprStr2$Region)
describe(SprStr2$State)
describe(SprStr2$Category)
describe(SprStr2$Sub.Category)
describe(SprStr2$Sales)


## install ggplot2 package
library(ggplot2)


##  Histogram of the Sales
ggplot(SprStr2, aes(x=Sales)) +
  geom_histogram(binwidth=50) +
  ggtitle("Histogram of Sales count")



## making histogram of Sales where the most of the data is concentrated
a = ggplot(SprStr2, aes(x=Sales))
a + geom_histogram(bins = 50) +
  ggtitle("Histogram of Sales where most of the data lies") +
  theme(plot.title=element_text(hjust=0.5)) +
  coord_cartesian(xlim=c(0,5000)) +
  ylab(NULL)

## Density plot of the Sales
ggplot(SprStr2, aes(x=Sales)) +
  geom_density() +
  ggtitle("Density Plot of the Sales")

  
## making density plot of Sales where the most of the data is concentrated
a + geom_density() +
  ggtitle("Density curve of sales where the most the data is concentrated") +
  theme(plot.title=element_text(hjust=0.5)) +
  coord_cartesian(xlim=c(0,5000)) +
  ylab(NULL)

## five point summary of Sales
fivenum(SprStr2$Sales)

## box-plot of Sales
boxplot(SprStr2$Sales, horizontal=TRUE, main="Box-plot of the Sales", xlab="Count of Sales")

## box-plot making data proportions more readable leaving most of the outliers
boxplot(SprStr2$Sales, ylim=c(0,500),col="cyan", horizontal=TRUE, notch=TRUE, main="More readable Box-plot ignoring most outliers", xlab="Count of Sales")

## Plotting the Sales and comparing
## Sales vs Ship.Mode
ggplot(SprStr2, aes(x=Ship.Mode, y=Sales, fill=Ship.Mode)) +
  geom_col() + 
  scale_fill_discrete(guide="none") +
  ggtitle("Sales Vs Shipping Mode") +
  theme(plot.title=element_text(hjust=0.5))

## Plotting the Sales vs Quantity sold by Ship.Mode
ggplot(data=SprStr2, aes(x=Quantity, y=Sales, fill= Ship.Mode)) +
  geom_bar(stat="identity") +
  ggtitle("Sales vs Quantity by Shipping Mode")

## Plotting the Sales vs Profit earned by Ship.Mode
ggplot(data = SprStr2, aes(x= Sales, y=Profit, color=Ship.Mode)) +
   geom_point() +
  ggtitle("Sales vs Profit by Shipping Mode")

## Plotting the Sales vs Discount given by Ship.Mode
ggplot() +
  ggtitle("Sales vs Discount by Shipping Mode") +
  geom_point(data = SprStr, aes(x=Discount, y= Sales, color=Ship.Mode)) 
  
## Plotting the Sales vs Category sold by Ship.Mode
ggplot(data=SprStr2, aes(x=Category, y=Sales, fill= Ship.Mode)) +
  geom_bar(stat="identity") +
  ggtitle("Sales vs Category by Shipping Mode")

## Plotting the Sales vs Subcategory sold by Ship.Mode
ggplot(data=SprStr2, aes(x=Sub.Category, y=Sales, fill= Ship.Mode)) +
  geom_bar(stat="identity") +
  ggtitle("Sales vs Subcategory by Shipping Mode")

## install RColorBrewer package
library(RColorBrewer)
## Profit Vs Discount
ggplot(SprStr2, aes(x=Discount, y=Profit, fill=Ship.Mode)) +
  geom_col() +
  ggtitle("Profit vs Discount by Ship.Mode") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_colour_brewer(palette="YlOrRd") +
  scale_fill_brewer(palette = "BuPu")


#########

## Counting the distribution in Region 
countsR1 <- table(SprStr2$Region)
countsR1
bp1 <- barplot(countsR1, main="Frequency in Regions", xlab="Region", col=c("purple","pink", "lightgreen", "yellow"))
legend(x="topright", legend=c("Central", "East", "South", "West"), fill=c("purple","pink", "lightgreen", "yellow"))
text(bp1, countsR1, labels=countsR1)

## Quantity of goods purchased by Region
countsR2 <- table(SprStr2$Region, SprStr2$Quantity)
countsR2
bp2 <- barplot(countsR2, main="Total Quantity by Region", xlab="Quantity", col=c("purple","pink", "lightgreen", "yellow"))
legend(x="topright", legend=c("Central", "East", "South", "West"), fill=c("purple","pink", "lightgreen", "yellow"))


#### Profit vs Sub category  by Region
ggplot() +
  geom_bar(data=SprStr2, aes(x=Sub.Category, y=Profit, fill=Region), stat="identity") +
  theme(axis.text.x= element_text(angle=90, vjust=0.5, hjust=1)) +
  ggtitle("Profit vs Sub Category by Region")

  
#### Sales vs Category by Region
ggplot() +
  geom_bar(data=SprStr2, aes(x=Category, y=Sales, fill=Region), stat="identity") +
  theme(axis.text.x= element_text(angle=90, vjust=0.5, hjust=1)) +
  ggtitle("Sales vs Category by Region")


#### Profit vs Category by Region
ggplot() +
  geom_bar(data=SprStr2, aes(x=Category, y=Profit, fill=Region), stat="identity") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))+
  ggtitle("Profit vs Category by Region")


## Sales of Segment
ggplot(SprStr2, aes(x=Segment, y=Sales, fill=Segment)) +
  geom_col() +
  scale_fill_discrete(guide="none") +
  ggtitle("Sales of Segments")

## Sales of Sub Category
ggplot(SprStr2, aes(x=Sub.Category, y=Sales, fill=Category)) +
  geom_col() +
  ggtitle("Sales of Sub Categories") +
  xlab("Sub Categories") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

## Profit vs Discount by Sub Category 
ggplot() +
  ggtitle("Profit vs Discount by Sub.Category") +
  geom_bar(data=SprStr2, aes(x=Discount, y=Profit, fill=Sub.Category), stat="identity") +
             theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

## Plotting the Subcategory vs Category sold by Segment
ggplot(data=SprStr2, aes(x=Sub.Category, y=Category, fill= Segment)) +
  geom_bar(stat="identity") +
  ggtitle("Subcategory vs Category by Segment")

## Sales vs Profit by Discount
ggplot() +
  ggtitle("Sales vs Profit by Discount") +
  geom_point(data=SprStr_num, aes(x=Sales, y=Profit, color=Discount))




########### Wordcloud 

install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

library(tm)
library(SnowballC)
library(wordcloud)

wordcloud(words= SprStr2$Sub.Category, min.freq=1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Dark2"))

##########
library(corrplot)


pairs(SprStr_num)


cp <- SprStr_num
View(cp)
cp.cor=cor(cp)
cp.cor
corrplot(cp.cor, main="Correlation Plot")

















