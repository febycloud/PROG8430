##################################################
### PROG8430                                    ##
### Assignment 01                               ## 
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
setwd("~/Codes/PROG8430/Assigment01")
##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################
#If the library is not already downloaded, download it

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(HSAUR)){install.packages("HSAUR")}
library("HSAUR")
if(!require(tidyr)){install.packages("tidyr")}
library("tidyr")
install.packages("tidyverse")
install.packages("ggpubr")
##################################################
### Read in Data                                ##
##################################################
Elect15 <- read.csv("AS01.csv",header=TRUE,sep= ",",row.names = 1)
str(Elect15)
##################################################
### Task      1-1                               ##
##################################################
tapply(Elect15$LPV, Elect15$Prov, sum)
##################################################
### Task      1-2                               ##
##################################################
Elect<-subset(Elect15,Elect15$Prov=="ON")
x1<-Elect$Tru
w1<-Elect$Electors
weighted.mean(x1,w1)
##################################################
### Task      1-3                               ##
##################################################
Elewin<-subset(Elect15,Elect15$LIB_Win==1)
sd(Elewin$ecc_sat,na.rm=TRUE)
##################################################
### Task      1-4                               ##
##################################################
quantile(sum(Elect15$Female,na.rm=TRUE),.28,na.rm=TRUE)
##################################################
### Task      1-5                               ##
##################################################
mad(Elect$TO,center = median(Elect$TO),constant = 1.4826)


##################################################
### Task      2-1                               ##
##################################################
BQP<-aggregate(Elect15$BQP_Win,by=list(Prov=Elect15$Prov),FUN=sum)
CPC<-aggregate(Elect15$CPC_Win,by=list(Prov=Elect15$Prov),FUN=sum)
LIB<-aggregate(Elect15$LIB_Win,by=list(Prov=Elect15$Prov),FUN=sum)
NDP<-aggregate(Elect15$NDP_Win,by=list(Prov=Elect15$Prov),FUN=sum)
GRN<-aggregate(Elect15$GRN_Win,by=list(Prov=Elect15$Prov),FUN=sum)
Partwin<-cbind.data.frame(BQP$x,CPC$x,LIB$x,NDP$x,GRN$x)
colnames(Partwin)<-c("BQP","CPC","LIB","NDP","GRN")
rownames(Partwin)<-GRN$Prov
Partwin<-Partwin/rowSums(Partwin)
##################################################
### Task      2-2                               ##
##################################################
GenderE<-Elect15[!(Elect15$Prov=="NU"|Elect15$Prov=="NT"|Elect15$Prov=="YT"),]
libM<-sum(subset(GenderE$Male,GenderE$LIB_Win==1))
bqpM<-sum(subset(GenderE$Male,GenderE$BQP_Win==1))
cpcM<-sum(subset(GenderE$Male,GenderE$CPC_Win==1))
ndpM<-sum(subset(GenderE$Male,GenderE$NDP_Win==1))
grnM<-sum(subset(GenderE$Male,GenderE$GRN_Win==1))
libF<-sum(subset(GenderE$Female,GenderE$LIB_Win==1))
bqpF<-sum(subset(GenderE$Female,GenderE$BQP_Win==1))
cpcF<-sum(subset(GenderE$Female,GenderE$CPC_Win==1))
ndpF<-sum(subset(GenderE$Female,GenderE$NDP_Win==1))
grnF<-sum(subset(GenderE$Female,GenderE$GRN_Win==1))

Party=c("LIB","BQP","CPC","NDP","GRN")
Male=c(libM,bqpM,cpcM,ndpM,grnM)
Female=c(libF,bqpF,cpcF,ndpF,grnF)
sex<-rbind(Male,Female)
barplot(sex,names.arg = Party,beside = T,legend.text = c("Male","Female"),
        main ="Gender supoort by Party" )

##################################################
### Task      2-3                               ##
##################################################
TruHist<-Elect15$Tru
hist(TruHist,main = "Trudeau supoorter saitisfaction")
##################################################
### Task      2-4                               ##
##################################################
libH<-subset(GenderE$ecc_sat,GenderE$Win_Party=="LIB")
bqpH<-subset(GenderE$ecc_sat,GenderE$Win_Party=="BQP")
cpcH<-subset(GenderE$ecc_sat,GenderE$Win_Party=="CPC")
ndpH<-subset(GenderE$ecc_sat,GenderE$Win_Party=="NDP")
grnH<-subset(GenderE$ecc_sat,GenderE$Win_Party=="GRN")
boxplot(libH,bqpH,cpcH,ndpH,grnH,
        names=Party,main="Economic Satisfaction by Party")

##################################################
### Task      2-5                               ##
##################################################
TruHist<-Elect15$Tru
hist(TruHist,main = "Trudeau supoorter saitisfaction")

LibHist<-Elect15$Lib
hist(LibHist,main = "LIB supoorter saitisfaction")

plot(TruHist,LibHist,main = "PartyLeader vs Party")
abline(lm(TruHist~LibHist))
cor.test(TruHist,LibHist)

##################################################
### Task      3-1                               ##
##################################################
qqnorm(Elect15$LPV)
qqline(Elect15$LPV)
t.test(Elect15$LPV)
##################################################
### Task      3-2                               ##
##################################################
LIB_Win<-subset(Elect15$LPV,Elect15$LIB_Win==1)
LIB_LOSE<-subset(Elect15$LPV,Elect15$LIB_Win==0)
var.test(LIB_Win,LIB_LOSE,alternative="two.sided")


         
