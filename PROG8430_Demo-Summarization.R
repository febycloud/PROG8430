##################################################
### PROG8430                                    ##
### Demonstrates some summarization             ## 
##################################################
#                                               ##
##################################################
# Written by David Marsh
# ID: 8643279
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("~/Codes/PROG8430")

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

#if(!require(dplyr)){install.packages("dplyr")}
#library("dplyr")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

##################################################
### Read in Data                                ##
##################################################

Elect08 <- readRDS("Elect2008.Rds")

str(Elect08)
head(Elect08,10)

##################################################
### Measures of Central Tendency                ##
##################################################

mean(Elect08$Obama)

mean(Elect08$Obama, trim=0.1)

median(Elect08$Obama)

weighted.mean(Elect08$Obama, w=Elect08$Population)
weighted.mean(Elect08[Elect08$Region=="III","Obama"], w=Elect08[Elect08$Region=="III","Population"], na.rm=TRUE)


##################################################
### Measures of Dispersion                      ##
##################################################

var(Elect08$Obama)

sd(Elect08$Obama)

mad(Elect08$Obama)

IQR(Elect08$Obama)

range(Elect08$Obama)

quantile(Elect08$Obama) 

quantile(Elect08$Obama, c(.32, .57, .98)) 

summary(Elect08$Obama)


##################################################
### Create Tables                               ##
##################################################

#Create a Variable to see if Obama won a State

Elect08$Win <- "M"
Elect08$Win <- with(Elect08, ifelse(Obama > McCain, "O", Win))
str(Elect08)
Elect08$Win <- as.factor(Elect08$Win)
str(Elect08)

#Frequency Table

Table2 <- table(Elect08$Region, Elect08$Win)
Table2
margin.table(Table2, 1) # A frequencies (summed over B)
margin.table(Table2, 2) # B frequencies (summed over A)

prop.table(Table2) # cell percentages
prop.table(Table2, 1) # row percentages
prop.table(Table2, 2) # column percentages

#States Obama won above a certain amount

Elect08[Elect08$Obama > 65, c("State", "Unemployment", "Income", "Obama")]

#Aggregate Percentage Won by Region

Table1 <- aggregate(Elect08[,3:4], by=list(Elect08$Region), FUN=mean, na.rm=TRUE)
Table1


Table1 <- aggregate(Elect08[,3:5], by=list(Elect08$Region), FUN=mean, na.rm=TRUE)
Table1


##################################################
### Bar Charts                                  ##
##################################################

#Alaska and Hawaii lack detailed info so remove them

Elect <- Elect08[!(Elect08$State %in% c("Alaska", "Hawaii")), ]

str(Elect)

#Percent Catholic by State

barchart(State ~ Catholic, data=Elect, main="Percent Catholic by State",
            xlab="Percent Catholic", ylab="State")

#Religious Affliation by State 

barchart(State ~ Catholic + Protestant + Non.religious + Other,
              data=Elect, stack=TRUE, main="Religious Affiliation by State",
              xlab="Percent Affiliation", ylab="State", 
              auto.key=list(space='bottom'))

#We can compare side by side as well

barchart(Region ~ Catholic + Protestant + Non.religious + Other,
              data=Elect, beside=TRUE, main="Religious Affiliation by Region",
              xlab="Percent Affiliation", ylab="Region", 
              auto.key=list(space='bottom'))


##################################################
### Histograms                                  ##
##################################################

#Histogram of Percent Support 

histogram( ~ Obama, dat=Elect08, main="Obama Percent Support")

#Forcing Breaks

histogram( ~ Obama, dat=Elect08, breaks=10, main="Obama Percent Support")

#Style type = (count, density, percent)

histogram( ~ Obama, dat=Elect08, breaks=10, type = "count",
            main="Obama Count of Support")

##################################################
### Box Plots                                   ##
##################################################

#Box Plot of McCain Support 

bwplot(Elect08$McCain, data=Elect08, main="Distribution of McCain Support",
       xlab="Percent of McCain Support", pch = '|')

#Comparing by Regions

bwplot(McCain ~ Region, data=Elect08, 
               main="Distribution of McCain Support by Region",
              xlab="Regions",  pch = '|')


str(Elect08)

# Correlation Coefficient

cor.test(Elect$Turnout, Elect$Income)
#Pearson defaults, but assumes normality

cor.test(Elect$Turnout, Elect$Income, method="spearman")


##################################################
### Scatter Plots                               ##
##################################################

# Basic chart
xyplot(Turnout ~ Income, data=Elect, main="Turnout by Income, 2008")

#You can change some formatting

xyplot(Turnout ~ Income, data=Elect, color="violet", pch=20,
       main="Turnout by Income, 2008")

#Multiple plots - by region

xyplot(Turnout ~ Income|Region, data=Elect,
       layout = c(5,2),
       color="violet", pch=20,
       main="Turnout by Income, 2008" ) 




