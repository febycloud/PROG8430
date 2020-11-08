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
#Fullmodel
Dia_lm = lm(Price_FY~Carat.Size_FY+Year_FY+Clar_FY+Col_FY+Cut_FY+Val_FY,data=NumDiamd,na.action = na.omit)
summary(Dia_lm)
#Stepwise
Stp_Dia_lm=step(Dia_lm,details=TRUE)
summary(Stp_Dia_lm)
#Forward
min_model<-lm(Price_FY~1,data=NumDiamd,action=na.omit)
Fwd_Dia_lm=step(min_model,direction = "forward",scope = (~Carat.Size_FY+Year_FY+Clar_FY+Col_FY+Cut_FY+Val_FY),details=TRUE)
summary(Fwd_Dia_lm)

#Model Evaluation
DiaFit<-predict(Dia_lm)
FullDiaRes<-residuals(Dia_lm)
StpDiaFit<-predict(Stp_Dia_lm)
StpDiaRes<-residuals(Stp_Dia_lm)
FwdDiaFit<-predict(Fwd_Dia_lm)
FwdDiaRes<-residuals(Fwd_Dia_lm)

#Numercial
shapiro.test(FullDiaRes)
shapiro.test(StpDiaRes)
shapiro.test(FwdDiaRes)

#Graphical
par(mfrow=c(2,2))
plot(Dia_lm)
par(mfrow=c(1,1))
par(mfrow=c(2,2))
plot(Stp_Dia_lm)
par(mfrow=c(1,1))
par(mfrow=c(2,2))
plot(Fwd_Dia_lm)
par(mfrow=c(1,1))

