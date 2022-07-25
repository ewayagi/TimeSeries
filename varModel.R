
#======================CLEARING MEMORY=========================#
rm(list=ls()) 
gc() 
cat("\f") 
ls() 

#========================INTRODUCTION==========================#

#AUTHOR:kkitonga
#Date:22/07/2022
#topic:Vector autorregression model

#=======================INSTALL PACKAGES=======================#

install.packages("vars")    #do this step only IF NOT installed
install.packages("AER")     #do this step only IF NOT installed
install.packages("dplyr")   #do this step only IF NOT installed
install.packages("ggplot2") #do this step only IF NOT installed
install.packages("tseries") #do this step only IF NOT installed


#========================LOAD LIBRARY==========================#

library(vars)                
library(AER)                 
library(dplyr)               
library(ggplot2)             
library(tseries)             

#=========================LOAD DATA============================#

#data to be used
data(USMacroG)

#about dataset
?USMacroG

#duplicating dataset;converting to dataframe
USMacroG1 = na.omit(data.frame(USMacroG))
print(USMacroG1)

#====================STATIONARITY CHECK=========================#

                    #1.Visual inspection

#Step 1.a: simple plots
plot(USMacroG1$m1)            
plot(USMacroG1$interest)            
plot(USMacroG1$unemp) 
plot(USMacroG1$inflation)

#Step 1.b: autocorrelation plots acf plots
acf(USMacroG1$m1)   
acf(USMacroG1$unemp)   
acf(USMacroG1$inflation)   
acf(USMacroG1$interest)   


                   #2.statistical tests
#step 2:augmented dickey_fuller test
#if p-value less than significant level we reject null
#and conclude stationary


adf.test(USMacroG1$unemp)                       #non-stationary
adf.test(USMacroG1$m1)                          #non-stationary
adf.test(USMacroG1$interest)                    #stationary
adf.test(USMacroG1$inflation)                   #non-stationary

#==========================DIFFERENCING=========================#

#1.Difference inflation and unemployment;create df

data_var<-data.frame(inf_diff = diff(USMacroG1$inflation),
                      unemp_diff = diff(USMacroG1$unemp))

#2.binding:inf_diff and unemp_diff
#makes it easier when i call VAR commands

data_diff<-cbind(data_var$inf_diff,data_var$unemp_diff)
colnames(data_diff) <- cbind("inf_diff","unemp_diff")

    
#3.testing stationary for differences variables
adf.test(data_var$inf_diff)                      #stationary
adf.test(data_var$unemp_diff)                    #stationary

#======================VARS REGRESSION==========================#

#1.Establishing optimal lag
lagselect <-VARselect(data_diff, lag.max = 10, type = c("const", "trend", "both", "none"),
                       season = NULL, exogen = NULL)

#2.Getting information on selection criteria
lagselect$selection

#3.running var with optimal recommended lag 
VarModel <- VAR(data_diff,p=3)
summary(VarModel)

#4.Check stability
Varstab <- stability(VarModel)
plot(Varstab)

#Error in plot.new() : figure margins too large:
#if you get this maximize plot window

#5.model residuals for autocorrelation
autocorr <- serial.test(VarModel)
autocorr

#6.Granger causality
grangeinf <-causality(VarModel,cause="inf_diff")  #inflation
grangeinf

grangeunemp <-causality(VarModel,cause="unemp_diff") #unemployment
grangeunemp

#=====================END  R session===========================#

dev.off()
rm(list=ls())
gc() 
cat("\f")




