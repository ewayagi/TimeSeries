#author:ewayagi
#Date: Tue 12, July 2022
#Content:3-Variable VAR Model
#Data source: data("USDistLag")

#=====================clearing the memory======================================#
rm(list=ls()) 
gc() 
cat("\f") 
ls() 
dev.off()

#==================Loading Libraries===========================================#
library(AER) 
library(vars)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(mFilter)
library(dynlm) 
library(fUnitRoots) 
library(dplyr)
library(ggplot2)
library(plotly)

#===========================loading the data===================================#
data("USMacroG")
??USMacroG
vardata = USMacroG
#convert the data to a data.frame
vardataDf = as.data.frame(vardata)
#confirm the class of the vardata
class(vardataDf)
#call the variable names
variable.names(vardataDf)

#=======================defining the time series variables=====================#

tsgdp = ts(vardataDf$gdp, start = c(1950,1,4), frequency = 4)
tsinvest = ts(vardataDf$invest, start = c(1950,1,4), frequency = 4)
tsconsumption = ts(vardataDf$consumption, start = c(1950,1,4), frequency = 4)

#===========================variable visualization=============================#
#=======================Grouped scatter Plots==================================#
#Plot name: Grouped scatter Plots

par(mfrow=c(2,2)) #allows for more than 1 plot on the same page

plot(vardataDf$consumption, vardataDf$gdp, main = "gdp vs. consumption")
plot(vardataDf$invest, vardataDf$gdp, main = "gdp vs. investment")
plot(vardataDf$consumption, vardataDf$invest, main = "consumption vs. investment")

#===========================Non- grouped scatter plots=========================#
#Plot name: GDP - Cons ScatPlot

vardataDf %>%
  ggplot(aes(consumption,gdp))+
  geom_point(color="purple")+
  labs(title="Scatter plot of gdp versus consumption")

#Plot name: GDP - Invest ScatPlot
vardataDf %>%
  ggplot(aes(invest,gdp))+
  geom_point(color="purple")+
  labs(title="Scatter plot of gdp versus investment")

#Plot name: Cons - Invest ScatPlot
vardataDf %>%
  ggplot(aes(consumption,invest))+
  geom_point(color="purple")+
  labs(title="Scatter plot of investment versus consumption")

#====================Testing for stationarity==================================#
#============================visual plots======================================#
#Plot name: StationaryPlots1
ts_plot(cbind(tsgdp, tsconsumption, tsinvest))

#OR
#plot.ts(cbind(tsgdp, tsconsumption, tsinvest))

#======================Phillips-Perron Unit Root Test for Stationary===========#

pp.test(tsgdp)
pp.test(tsinvest)
pp.test(tsconsumption)
#All var are non-stationary

#======================Or use ADF method=======================================#
#this requires that you specify the number of lags hence not reliable

#adfTest(tsgdp,lags=0)
#adfTest(tsinvest,lags=0)
#adfTest(tsconsumption,lags=0)

#=========Log transformation and differencing for stationarity=================#

dlntsgdp = diff(log(tsgdp))
dlntsconsumption = diff(log(tsconsumption))
dlntsinvest = diff(log(tsinvest))

#======================Repeat the tests========================================#
#Plots
#Plot name: StationaryPlots2

ts_plot(cbind(dlntsgdp, dlntsconsumption, dlntsinvest))

#OR
#plot.ts(cbind(dlntsgdp, dlntsconsumption, dlntsinvest))

#Phillips-Perron Unit Root Test

pp.test(dlntsgdp)
pp.test(dlntsinvest)
pp.test(dlntsconsumption)
#All variables are stationary

#========================VAR model=============================================#
#=====================Determining the optimal lag==============================#

variables = cbind(dlntsgdp, dlntsinvest, dlntsconsumption)

#change the variable names
colnames(variables) = cbind("GDP" , "INVESTMENT" , "CONSUMPTION")

#selecting the lags
lags = VARselect(variables, lag.max = 10, type = "const")

#view the selection
lags$selection

#the VAR model 
varmodel = VAR(variables, p = 1, type = "const", season = NULL, 
               exog = NULL, ic = "AIC")
summary(varmodel)

#===========================Model diagnostic===================================#

#1. Serial correlation
s_corr = serial.test(varmodel)
s_corr
#no serial correlation 

#2. Heteroscedasticity
HS = arch.test(varmodel)
HS
#homoscedasticity

# 3. Normality 
Norm = normality.test(varmodel)
Norm
#non-normal

#4. Structural break test 
stability1 = stability(varmodel)
stability1
plot(stability1)
#Plot name: StabilityPlots
#Stable

#==========================Granger Causality===================================#

GrangerGDP<- causality(varmodel, cause = "GDP")
GrangerGDP

GrangerINVEST<- causality(varmodel, cause = "INVESTMENT")
GrangerINVEST

GrangerCONS<- causality(varmodel, cause = "CONSUMPTION")
GrangerCONS

#==========================variance decomposition==============================#

vard <- fevd(varmodel, n.ahead = 5)
vard
plot(vard)
#Plot name: VARDPlots

#============================forecast==========================================#
#the model

forecast = predict(varmodel, n.ahead = 5, ci = 0.95)
forecast

#Forecasted Plots

#Plot name: GDPFSPlots
fanchart(forecast, names = "GDP", main = "Fanchart for GDP", 
         xlab = "Horizon", ylab = "GDP")

#Plot name: InvestFSPlots
fanchart(forecast, names = "INVESTMENT", main = "Fanchart for INVESTMENT", 
         xlab = "Horizon", ylab = "INVESTMENT")

#Plot name: ConsFSPlots
fanchart(forecast, names = "CONSUMPTION", main = "Fanchart for CONSUMPTION", 
         xlab = "Horizon", ylab = "CONSUMPTION")

#==========================ending R session====================================#
rm(list=ls()) 
gc() 
cat("\f") 
ls() 
dev.off()
