##################################################
### PROG8430                                    ##
### Assignment 1                                ## 
##################################################
#                                               ##
##################################################
# Written by David Marsh
# ID: 8643279
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear all plots
if(!is.null(dev.list())) dev.off()

# Clear entire console
cat("\014") 

# Clean and clear theworkspace
rm(list=ls())

#Set work directory to an appropriate location
setwd("C:/Users/David/Documents/Data")

options(scipen=9)

##################################################
### NOTE - Comments are included for your       ##
### to help you understand the code. In your    ##
### submission it needs to be changed to        ##
### your understanding of the steps you are     ##
### taking.                                     ##
##################################################

##################################################
## Install Libraries/Packages                   ##
##################################################

install.packages("fortunes")  #downloads package
library("fortunes")           #"attaches" package 

###################################################
## Run package functions                         ##
###################################################

fortune(floor(runif(1, min=1, max=387)))
fortune(123)

detach(package:fortunes)      #detaches (unloads) the package fortunes

