#author:kkitonga and ewayagi
#Date: Tue 12, July 2022
#Content:Autocorrelation tests in Time series analysis 
#Data source: data("USDistLag")

#=====================clearing the memory======================================#
rm(list=ls()) 
gc() 
cat("\f") 
ls() 

#==================Loading Libraries===========================================#
library(wooldridge)
library(dynlm) 
library(stats)
library(lmtest)
library(dplyr)
library(ggplot2)

#if the libraries are not installed the use command #install.package("package name")

#================================Loading Data==================================#
data("USDistLag")

#================================Descriptive===================================#
??USDistLag                       #checking what the data is about
data1=USDistLag                   #store USDistLag to data1
summary(data1)                    #brief summary of the data
variable.names(data1)             #gives you the names of the variables
View(data1)                       #to view the data 

#================================DATA VISUALIZATION============================#

#Scatter plot:ScatConsGnpR
data1Df = as.data.frame(data1)    #converting data1 to a data frame 
data1Df %>%
  ggplot(aes(gnp,consumption))+
  geom_point(color="purple")+
  labs(title="Scatter plot of consumption versus gnp")

#========================Dynamic Linear Regression=============================#
#1.Define independent and dependent variables
#independent:gnp
#Dependent;consumption

#2.Define and fit the model
Reg1 = dynlm(consumption ~ gnp + L(gnp,1), data = data1)

#3.Regression results
summary(Reg1)

#====================Autocorrelation Test======================================#
                   #1. Visual Inspection: ACF Plot
#Plot name: acfplotR

acf(Reg1$residuals, type = "correlation")

                   #2. Statistical Tests 
            #========Durbin-Watson Test==========#

dwtest(Reg1)

            #========Breusch-Godfrey Test=========#

bgtest(Reg1)

#================================Ending R-session==============================#
dev.off()
rm(list=ls())
gc() 
cat("\f")
