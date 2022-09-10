#==================CLEARING MEMORY=========================#
rm(list=ls()) 
gc() 
cat("\f") 
ls() 

#==================INTRODUCTION============================#

#Author:kkitonga and ewayagi
#Date:09/09/2022
#Topic:Vector autorregression model
#Time series:StockMarkets

#==================PACKAGE INSTALLATION===============#

#1.Do this if you have not installed the packages
#install.packages('vars')
#install.packages('tseries')
#install.packages('lmtesT')
#install.packages('urca')
#install.packages('psych')
#=================LOADING LIBRARIES========================#

library(vars)                              #VAR model
library(lmtest)                           #granger
library(urca)                             #Cointegration
library(tseries)
library(gridExtra)
library(psych)
#==================DATA====================================#

#1.Loading dataset
data(EuStockMarkets)

#2.About data
?EuStockMarkets

#3.Dataframe of only variables but for 200 observations
df = na.omit(data.frame(ts(EuStockMarkets[1600:1800,1:4])))


#===============CREATING RETURNS DATAFRAME===========#

#1.Get returns of stock market prices
DAX_re <- diff(ts(df$DAX))
SMI_re <- diff(ts(df$SMI))
CAC_re <- diff(ts(df$CAC))
FTSE_re <- diff(ts(df$FTSE))

#2.Bind them into a dataframe
df_re <-cbind(DAX_re,SMI_re,CAC_re,FTSE_re)
print(df_re)
#3.Renaming columns in df_re
colnames(df_re) <- c('DAX','SMI','CAC','FTSE')

#==================EDA===============================#

#i.Create descriptives function
descrip <-function(df){
                   p1<-print('first five observations')
                   first<-head(df)
                   p2<-print('last five observations')
                   tail<-tail(df)
                   p3<-print('five point summary')
                   five<-describe(df,fast=TRUE)
                   mylist <- list(p1,first,p2,
                                 tail,p3,five)
                   return(mylist)
}

#ii.run function on dataframe
descrip(df_re)

#==================VISUALIZATION===========================#

par(mfrow=c(2,2))
plot(DAX_re,type='l',col="blue",
     ylab='DAX',
     main='DAX returns ')

plot(SMI_re,type='l',col="blue",
     ylab='SMI',
     main='SMI returns')

plot(CAC_re,type='l',col="blue",
     ylab='CAC',
     main='CAC returns')

plot(FTSE_re,type='l',col="blue",
     ylab='FTSE',
     main='FTSE returns')
par(mfrow=c(1,1))

#i.we can observe variances change with time
#ii.probably not stationary

#==================STATIONARITY=============================#

#i.Visual inspection :ACF PLOTS
par(mfrow=c(2,2))
acf(DAX_re,main='ACF:DAX returns',col='blue')
acf(SMI_re,main='ACF:SMI returns ',col='blue')
acf(CAC_re,main='ACF:CAC returns',col='blue')
acf(FTSE_re,main='ACF:FTSE returns ',col='blue')
par(mfrow=c(1,1))

#seems stationary

#ii.Visual inspection :PACF PLOTS
par(mfrow=c(2,2))
pacf(DAX_re,main='PACF:DAX returns',col='blue')
pacf(SMI_re,main='PACF:SMI returns ',col='blue')
pacf(CAC_re,main='PACF:CAC returns',col='blue')
pacf(FTSE_re,main='PACF:FTSE returns ',col='blue')
par(mfrow=c(2,2))

#iii.Statistical :tests

          #i.Hypotheses for adf test
#ho:time series is non-stationary
#h1:time series is stationary

       #ii.decision:if p<0.05 we reject null hypothesis
adf.test(SMI_re,k=10)                   #stationary
adf.test(DAX_re,k=10)                   #stationary
adf.test(SMI_re,k=10)                   #stationary
adf.test(DAX_re,k=10)                    #stationary

#==================LAG SELECTION===========================#

#1.Optimal lag selection
optimal <-VARselect(df_re, type="const")
optimal$selection                      #lag=1

#==================VAR MODEL===============================#

#i.VAR model
model <-VAR(df_re, p = 1, type = c("const"),
    season = NULL, exogen = NULL, lag.max = NULL,
    ic = c("AIC"))
#ii.VAR model results
summary(model)

#==================CHECKS============================#

#1.Autocorellation

#ho:no serial autocorrelation
#h1:serial autocorelation
autocorr <- serial.test(model,lags.pt = 16, 
                      type = c("PT.asymptotic"))
autocorr

#2.Heteroskedasticity
#ho:presence of heteroskedasticity
#h1:heteroskedasticity
hetesk <- arch.test(model,lags.multi=5, 
                    multivariate.only=TRUE)
hetesk

#3.Normality
#ho:residuals do not follow normal distribution
#h1:residual follow normal distribution

normality <- normality.test(model,
                            multivariate.only=TRUE)
normality

#4.Stability of model
#i.Purpose:if there are structural breaks in residuals
var_stab <-stability(model$DAX, type = c("OLS-CUSUM"))
plot(var_stab)

#==================GRANGER CAUSALITY=================#

#i.If DAX granger causes SMI,CAC,FTSE
DAX_granger <- causality(model,cause='DAX')
DAX_granger

#ii.If SMI granger causes DAX,CAC,FTSE
#verdict:p-value <0.05 reject null
DAX_granger <- causality(model,cause='SMI')
DAX_granger

#iii.If CAC granger causes SMI,FTSE,DAX
#verdict:p-value <0.05 reject null
DAX_granger <- causality(model,cause='CAC')
DAX_granger

#iv.If FTSE granger causes SMI,CAC,DAX
#verdict:p-value <0.05 reject null
DAX_granger <- causality(model,cause='FTSE')
DAX_granger

#verdict:p-value <0.05 reject null
#Conclude:There is instantaneous causality

#==================IRF===============================#

#i.IRF:Impact of a schock to DAX on other variables
DAX_SMI <- irf(model,impulse='DAX',response='SMI',n.ahead=10)
DAX_CAC <- irf(model,impulse='DAX',response='CAC',n.ahead=10)
DAX_FTSE <- irf(model,impulse='DAX',response='FTSE',n.ahead=10)

#ii.Plots of irf
plot(DAX_SMI)
plot(DAX_CAC)
plot(DAX_FTSE)

#================VARIANCE DECOMPOSITION==============#

#i.Variance decomposition
var_fevd <- fevd(model,n.ahead=10)
plot(var_fevd)

#=================FORECASTING========================#

#i.forecast for each variable
forecast <- predict(model,n.ahead=20,ci=0.95)

#ii.fancharts for each
fanchart(forecast,names='DAX')
fanchart(forecast,names='SMI')
fanchart(forecast,names='CAC')
fanchart(forecast,names='FTSE')

#================END=================================#


