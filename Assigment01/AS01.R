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
Elect<-subset(Elect15,Elect15$Prov=="BC")
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
GenderE <- aggregate(GenderE[c("Male", "Female")], by = list(GenderE$Win_Party), FUN = sum, rm.na = TRUE)
barchart(Group.1~ Male+Female,
         data = GenderE,
         beside = TRUE,main="Gender win by party",auto.key=list(),xlab="Population")

##################################################
### Task      2-3                               ##
##################################################
TruHist<-Elect15$Tru
hist(TruHist,main = "Trudeau supoorter saitisfaction")
##################################################
### Task      2-4                               ##
##################################################
GenderE<-Elect15[!(Elect15$Prov=="NU"|Elect15$Prov=="NT"|Elect15$Prov=="YT"),]
bwplot(Lib~Win_Party,data=GenderE,main="Platical Party Saitisfaction by Party",ylab="Saitisfaction" ,pch="|")

##################################################
### Task      2-5                               ##
##################################################
TruHist<-Elect15$Tru
hist(TruHist,main = "Trudeau supoorter saitisfaction")

LibHist<-Elect15$Lib
hist(LibHist,main = "LIB supoorter saitisfaction")

plot(TruHist,LibHist,main = "PartyLeader vs Party")
abline(lm(LibHist~TruHist))
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
LIB_Win<-mean(subset(Elect15$LPV,Elect15$LIB_Win==1))
LIB_LOSE<-mean(subset(Elect15$LPV,Elect15$LIB_Win==0))

LIB_Win<-subset(Elect15$LPV,Elect15$LIB_Win==1)
LIB_LOSE<-subset(Elect15$LPV,Elect15$LIB_Win==0)
var.test(LIB_Win,LIB_LOSE,alternative="two.sided")


         
