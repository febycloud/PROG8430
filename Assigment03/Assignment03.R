##################################################
### PROG8430                                    ##
### Assignment 03                               ## 
##################################################
#                                               ##
##################################################
# Written by Fei Yun
# ID: 8680643
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
setwd("~/Codes/PROG8430/Assigment03")
##################################################
### Install Libraries                           ##
##################################################
install.packages("pastecs")
install.packages("smooth")
install.packages("tseries")
install.packages("TTR")
##################################################
### Assignment 03                               ##
##################################################


##################################################
### Section 01                                  ##
##################################################
#Data Transformation
#1. Read in the Welland data and transform it into an appropriate time
#series datatype.
#read data
Temperatue<-read.csv('Welland20F.csv',header = TRUE)
#convert data to time series
tsWelland_FY<-ts(Temperatue$Temp,frequency = 12,start = c(1985,1))
head(tsWelland_FY)

#Descriptive Data Analysis
#1. Summarize the precipitation information (mean, std dev, etc.)
summary(tsWelland_FY)

#2. Plot the time series data and make note of anything significant you observe.
plot.ts(tsWelland_FY,main='Averange Tempreature of Welland',ylim=c(-10,25))

#3. Decompose the times series data in to the constituent components.
#Comment on each (any trends you observe, etc.)
decoWelland_FY<-decompose(tsWelland_FY,type = 'additive')
decoWelland_FY
plot(decoWelland_FY)
#we can see the seasonal data amplitude doesn't change with times(year) change
#the seasonal data amplitude is almost same every year so additive model is the best

#4. Determine if the time series is stationary.
library(tseries)
adf.test(tsWelland_FY)
#p-value<0.05 stationary
#5. Deseasonalize the information and plot the result.
seasonal_adj_Welland_FY<-tsWelland_FY-decoWelland_FY$seasonal
plot.ts(seasonal_adj_Welland_FY,main='Deseasonal Temperature of Welland',ylim=c(-10,25))

#6. Add any comments about what you observe: seasonality of precipitation, trends, etc.
# I observe trend at 1991-1992，the temperature was going to a big increase
##################################################
### Section 02                                  ##
##################################################
#Data Transformation
#1. Read in the Waterloo data and transform it into an appropriate time
#series datatype.
#read data
Precip<-read.csv('Waterloo20F.csv',header = TRUE)
head(Precip)
#convert data to time series
tsWaterloo_FY<-ts(Precip$Precip,frequency = 1,start = c(1970))
head(tsWaterloo_FY)

#Descriptive Data Analysis
#1. Summarize the information (mean, std dev, etc.)
summary(tsWaterloo_FY)
library(pastecs)
stat.desc(tsWaterloo_FY)
#2. Plot the time series data and make note of anything significant you observe.
plot.ts(tsWaterloo_FY,main='Total Precipitation of Waterloo',ylim=c(670,1200))
#the year of 1990 is lowest,the precipitation seems increasing by year

#3. Smooth the precipitation chart using a moving average. Try 3 different 
#values for the moving average and choose the one you think best shows the trend (if any).
library(TTR)
WaterlooSMA10_FY<-SMA(tsWaterloo_FY,n=10)
plot.ts(WaterlooSMA10,main='Total Precipitation of Waterloo',ylim=c(670,1200))
# this lost too much details
WaterlooSMA5_FY<-SMA(tsWaterloo_FY,n=5)
plot.ts(WaterlooSMA5_FY,main='Total Precipitation of Waterloo',ylim=c(670,1200))
# this has details and can clear its trends and moving
WaterlooSMA2_FY<-SMA(tsWaterloo_FY,n=2)
plot.ts(WaterlooSMA2_FY,main='Total Precipitation of Waterloo',ylim=c(670,1200))
# this can not identify its trends
#4. Determine if the time series is stationary.
library(tseries)
adf.test(tsWaterloo_FY)
#p-value>0.05 non-stationary

#5. Create an autocorrelation chart (using acf) and comment on which lags are significant. 
#Do previous values seem to influence current values?
acf(tsWaterloo_FY)
#lag3,lag8,lag11 are significant
#seems previous values does not influence current values

#3
#1. Create a simple moving average forecast of precipitation in Waterloo 
#for five years beyond the data provided. Graph your results along with a 75% prediction interval.
smaWaterloo_FY<-sma(tsWaterloo_FY)
smaWaterloo_FY
smaWaterloo_FY<-forecast(smaWaterloo_FY,h=5,level=0.75)
plot(smaWaterloo_FY)
#2. Create an exponentially smoothed forecast of precipitation in Waterloo 
#for five years beyond the data provided. Graph your results along with a 75% prediction interval.
exsWaterloo_FY<-es(tsWaterloo_FY)
exsWaterloo_FY
exsWaterloo_FY<-forecast(exsWaterloo_FY,h=5,level = 0.75)
plot(exsWaterloo_FY)
#3. Compare the two forecasts you created in steps 1 and 2 above. 
#Which forecast seems superior? Why?
#We can see the forecast based on simple moving ,the precipitation was between 889 to 914
#and based on exponetially smoothed forecast the precipitation was just at 909
# the actual precipitation of 1996-2000 is 1043.0,865.7,656.5,811.4,933.9
#the simple moving forecast has higher accuracy,which is better model,because simple moving
#calculate the average of data,but exponential gives more weight to recent data, here the precipitation
#is not influenced by previous year's data,so simple moving is better .
