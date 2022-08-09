# -*- coding: utf-8 -*-
"""
Created on Mon Aug  1 05:42:00 2022

@author: Kkitonga and ewayagi
content:ARIMA

"""
#==========================INSTALL PACKAGES===================================#
 
#Install pandarima packages to get autoarima function
#!pip install pmdarima :remove # and run this code  if YOU DO NOT have pmdarima



#==========================LOAD LIBRARIES=====================================#
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
import pandas as pd
from pmdarima.arima import auto_arima

#=============================LOAD DATA=======================================#

#Air Passengers data
AirPassengers = sm.datasets.get_rdataset('AirPassengers',package='datasets')
AirPassengers = AirPassengers.data

#============================VISUALIZATION===================================#

#1.Plot 
plt.plot(AirPassengers.value)
plt.xlabel("Observations across time")
plt.ylabel('Number of air Passengers')
plt.title("Plot of Air Passengers data")

#2.aCF
sm.graphics.tsa.plot_acf(AirPassengers.value)


#3.statistical :ADF

def adf(dataset):
    adf_test1 = sm.tsa.stattools.adfuller(dataset)
    print('adf  test statistic:',adf_test1[0])
    print('pvalue:',adf_test1[1])
    print('usedlag:',adf_test1[2])
    print('''The number of observations used for the ADF regression 
     and calculation of the critical values. :''',adf_test1[3])
    print('Critical values for the test statistic at the 1 %, 5 %, and 10 % levels:')
    for key,val in adf_test1[4].items():
        print(key,":",val)
        

adf(AirPassengers.value)         

                   #data not stationary
 
#=============================BUILDING A MODEL================================#

#i.Best p,d,q stipulation
apModel = auto_arima(AirPassengers.value,trace=True,suppress_warnings=True)

#ii.fitting arima model
ap_Arima = sm.tsa.ARIMA(AirPassengers.value,order=(4,1,3))
ap_Arima =  ap_Arima.fit()

#iii.Arima output
print(ap_Arima.summary())
        
#iv.Acf plot:residuals of fitted model
sm.graphics.tsa.plot_acf(ap_Arima.resid)

#==========================PREDICTING =======================================#

#i.predict value of next 30 periods based on our model 
forecast = ap_Arima.predict(start=len(AirPassengers),end=len(AirPassengers)+30)

#ii.Value of predict Air Passenger value in the next period
print(forecast)

#==========================BOX TEST==========================================#

                 #autocorelation in time series
#i.perform Ljung-Box test on residuals with lag=5
box_test_lag5=sm.stats.acorr_ljungbox(ap_Arima.resid, lags=[5], return_df=True)
print(box_test_lag5)      #residuals independently distributed

#ii.perform Ljung-Box test on residuals with lag=10
box_test_lag10=sm.stats.acorr_ljungbox(ap_Arima.resid, lags=[10], return_df=True)
print(box_test_lag10)   #residuals independently distributed
#=============================END===========================================#
