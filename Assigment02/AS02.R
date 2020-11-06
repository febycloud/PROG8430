##################################################
### PROG8430                                    ##
### Assignment 02                               ## 
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
setwd("~/Codes/PROG8430/Assigment02")
##################################################
### Install Libraries                           ##
##################################################
install.packages("pastecs")
install.packages("corrgram")
##################################################
### Assignment 02                               ##
##################################################
Diamond<-read.table("diamond_val2.txt",header = TRUE)

#Data Transformation 
#1
names(Diamond)<-c('Price_FY','Carat.Size_FY','Source_FY','Year_FY','Clar_FY','Col_FY','Cut_FY','Val_FY')
head(Diamond)
#Descriptive Data Analysis 
#1
summary(Diamond)

par(mfrow=c(3,3))
sapply(names(Diamond), function(cname){
  if(is.numeric(Diamond[[cname]]))
    print(hist(Diamond[[cname]],main=cname))
})
par(mfrow=c(1,1))

#Outliers
#1
par(mfrow=c(3,3))
sapply(names(Diamond), function(cname){
  if(is.numeric(Diamond[[cname]]))
    print(boxplot(Diamond[[cname]],main=cname))
})
par(mfrow=c(1,1))
#Exploratory Analysis
#1
par(mfrow=c(3,3))
sapply(names(Diamond), function(cname){
  if(is.numeric(Diamond[[cname]]))
    print(qqnorm(Diamond[[cname]],main=cname))
})
par(mfrow=c(1,1))

NumDiamd<-Diamond[-c(3)]
DiaNrm<-lapply(NumDiamd,shapiro.test)
DiaNrm
str(DiaNrm[[4]])
DiaRes<-sapply(DiaNrm, '[',c("statistic","p.value"))
DiaRest<-t(DiaRes)
DiaRest
#2
library(corrgram)
corrgram(NumDiamd,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pie,
         text.panel=panel.txt,main="Diamond stats")
res<-cor(NumDiamd,method = "spearman")
round(res,2)

#Model Development

