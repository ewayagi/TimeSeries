#==================CLEAR THE MEMORY============================#

rm(list=ls()) 
gc() 
cat("\f") 
ls() 

#==================INTRODUCTION================================#

#Author:kkitonga and ewayagi
#Date:01/08/2022
#content:ARIMA

#==================PACKAGES NEEDED=============================#

install.packages("forecast")     #do this step only IF NOT installed
install.packages("tseries") #do this step only IF NOT installed

#====================LOAD LIBRARIES============================#

library(forecast)
library(tseries)
library(changepoint)
library(ggplot2)
library(ggfortify)


#=======================DATA===================================#

data(AirPassengers)
print(AirPassengers)

#===================DESCRIPTIVES===============================#

df_summary <-  function(df) {
                              head_df <-head(df)
                              tail_df <- tail(df)
                              str_df <-str(df)
                              desc_list<- list(head_df,tail_df,
                                               str_df)
                              return(desc_list)
                               }

df_summary(AirPassengers)

#=====================STATIONARITY CHECKS======================#
  
              #=====Visualization===#
#i.Air Passenger plot
autoplot(AirPassengers,xlab='time',ylab='Number of air Passengers',
         main="Plot of Air Passengers data",
         colour="blue")

#ii.Autocorrellation plot
Acf(AirPassengers)


#iii.Plots indicating points of changes in varaince and mean
autoplot(cpt.mean(AirPassengers),xlab='time',
         ylab="Air Passengers",
         main="Plot indicating point in changes in ts mean")

autoplot(cpt.var(AirPassengers),xlab='time',
         ylab="Air Passengers",
         main="Plot indicating point in changes in ts Variance")

            #======Statistical=====#
#1.ADF
adf.test(AirPassengers,k=12) #k=12 because it is monthly data

#===================BUILDING MODEL=============================#

#checking best stipulation of model and building
AP_arima <- auto.arima(AirPassengers,trace=TRUE)
summary(AP_arima)

#============= STATIONARITY: FITTED MODEL======================#

#1.check residuals of fitted model
checkresiduals(AP_arima)

#2.Acf plot:residuals of fitted model
acf(ts(AP_arima$residuals))

#====================PREDICTION================================#

#i.predict value of next period based on our model 
#h=1*4=one year (four quarters)
predictAP <- forecast(AP_arima,level=c(95),h=1*4)

#ii.plot data and predicted point
autoplot(predictAP,xlab='time',
         ylab="Air Passengers",
         main="Arima forecast plot")

#iii.Value of predict Air Passenger value in the next period(h=1)
summary(predictAP)

#======================BOX TEST================================#

#i.checking autocorellation
Box.test(AP_arima$residuals,lag=5,type='Ljung-Box')
Box.test(AP_arima$residuals,lag=10,type='Ljung-Box')


#======================ENDING SESSION==========================#

dev.off()
rm(list=ls())
gc() 
cat("\f")

