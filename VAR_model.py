# -*- coding: utf-8 -*-
"""
Created on Thu Jul 21 13:45:34 2022
@author: kkitonga and ewayagi
Content:VAR Model
"""
#===========================libraries==================================#
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.api import VAR
from statsmodels.tsa.vector_ar.vecm import coint_johansen
from statsmodels.tsa.stattools import adfuller, grangercausalitytests


#====================exploring the datasets in statmodels=====================#

from statsmodels import datasets
print(dir(datasets))

#========loading macrodata from sm package and store in vardata===============#
vardata = datasets.macrodata.load_pandas().data

#==============================Exploring the data=============================#

#use these compands if you want to view all rows and all columns
#pd.set_option('display.max_columns', None)
#pd.set_option('display.max_rows' , None)

#view data
print(vardata)
#checking for more information about the data
vardata.info()
#describing the vardata
print(vardata.describe())
#checking for missing values in the data
print(vardata.isnull())
#view the first 5 rows
print(vardata.head())
#view the last 5 rows
print(vardata.tail())
#check the class ot type of the dataset
print(type(vardata))
#view the names of the variables 
print(vardata.columns.values)
#check the number of rows and columns 
print(vardata.shape)
#check data types
print(vardata.dtypes)
#selecting columns 
selection = vardata[["realgdp" , "unemp" , "infl"]]
print(selection)
#obtain means of variables
print(vardata.mean())

#================================plots========================================#
#1. simple plot of all variables 
vardata.plot()
#2. Scatter plots

vardata.plot(x = "realgdp", y ="unemp", kind = "scatter", color = "green")
plt.title("Scatterplot of real-gdp versus unemployment")

vardata.plot(x = "realgdp", y ="infl", kind = "scatter", color = "blue")
plt.title("Scatterplot of real-gdp versus inflation")

vardata.plot(x = "unemp", y ="infl", kind = "scatter", color = "red")
plt.title("Scatterplot of unemployment versus inflation")

#====================Testing for stationarity=================================#
# ADF method 
adfgdp1 = adfuller(vardata['realgdp'])
print(adfgdp1[0])
print(adfgdp1[1])

adfunemp1 = adfuller(vardata['unemp'])
print(adfunemp1[0])
print(adfunemp1[1])

adfinfl1 = adfuller(vardata['infl'])
print(adfinfl1[0])
print(adfinfl1[1])

#===============ADF test with differenced the non-stationary variables========#
adfdgdp2 = adfuller(vardata['realgdp'].diff()[1:])
print(adfdgdp2[0])
print(adfdgdp2[1])

adfdunemp2 = adfuller(vardata['unemp'].diff()[1:])
print(adfdunemp2[0])
print(adfdunemp2[1])

adfinfl2 = adfuller(vardata['infl'].diff()[1:])
print(adfinfl2[0])
print(adfinfl2[1])

#==================Granger Causality test=====================================#
granger1 = grangercausalitytests(vardata[["realgdp", "unemp"]], 5)

granger2 = grangercausalitytests(vardata[["realgdp", "infl"]], 5)

granger3 = grangercausalitytests(vardata[["infl", "unemp"]], 5)

granger4 = grangercausalitytests(vardata[["unemp", "infl"]], 5)

#========================Cointegration test===================================#

def cointegration_test(vardata, alpha=0.05): 
    out = coint_johansen(vardata, -1,5)
    d = {'0.90':0, '0.95':1, '0.99':2}
    traces = out.lr1
    cvts = out.cvt[:, d[str(1-alpha)]]
    def adjust(val, length= 6): return str(val).ljust(length)

    print('Name   ::  Test Stat > C(95%)    =>   Signif  \n', '--'*20)
    for col, trace, cvt in zip(vardata[["realgdp" , "unemp" , "infl"]], 
                               traces, cvts):
        print(adjust(col), ':: ', adjust(round(trace,2), 9), ">", 
              adjust(cvt, 8), ' =>  ' , trace > cvt)

cointegration_test(vardata)

#========================Splitting the train and test data====================#
variables = vardata[["realgdp" , "unemp" , "infl"]]
print(variables.shape)

test_obs = 12
train_data = variables[:-test_obs]
test_data = variables[-test_obs:]

print(train_data)
print(test_data)

#====================determine the number of lags to be used==================#
modellag = VAR(train_data)
order = modellag.select_order(maxlags = 10)
print(order.summary())

#lags = 2 

#===============================VAR model=====================================#
VAR_model = modellag.fit(2)
print(VAR_model.summary())

#===================================plotting the results======================#

VAR_model.plot()

#Autocorrelation plots 

VAR_model.plot_acorr()

#==============================forecasting====================================#
#ploting the forecast

VAR_model.plot_forecast(10)

#=======================================END===================================#