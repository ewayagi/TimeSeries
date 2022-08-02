# -*- coding: utf-8 -*-
"""
Spyder Editor
Authors:kkitonga & ewayagi
content:VAR model
Date:02/08/2022

"""

#===========================LOADING LIBRARIES=================================#

import numpy as np
import statsmodels.api as sm
import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.compat import lzip


#===========================LOADING DATASETS==================================#

USMacroG = sm.datasets.get_rdataset('USMacroG',package='AER'   )
USMacroG = USMacroG.data
data = USMacroG.dropna()

#descriptives
print(data.columns)                   #column names only
print(data.axes)                      #index and column name information
print(data.dtypes)                    #data structure
print(data.size)                      #dimension :rows* columns


#==========================STATIONARITY CHECK================================#

  
                   # 1.checking stationarity of level variables
adf_consump = sm.tsa.stattools.adfuller(data['consumption'])
print ('adf_test statistic for consumption :',adf_consump[0],';'
      'p-vlaue :',adf_consump[1])                              #not stationary

adf_gdp = sm.tsa.stattools.adfuller(data['gdp'])
print ('adf_test statistic for gdp :',adf_gdp[0],';' 
      'p-vlaue :',adf_gdp[1])                            #not stationary


                 #2.Differencing then checking stationarity

adf_consumpd = sm.tsa.stattools.adfuller(data['consumption'].diff()[1:])
print ('adf_test statistic for differenced consumption :',adf_consumpd[0],';'
      'p-vlaue :',adf_consumpd[1])                            #stationary

adf_gdpd = sm.tsa.stattools.adfuller(data['gdp'].diff()[1:])
print ('adf_test statistic for differenced gdp :',adf_gdpd[0],';'
      'p-vlaue :',adf_gdpd[1])                                 #stationary


#==========================GRANGER CAUSALITY==================================#

#Perform on level variables
print("\nresults below for consumption granger causing gdp")
granger_con_to_gdp = sm.tsa.stattools.grangercausalitytests(data[['consumption','gdp']], 5)

#Perform on level variables
print("\nResults below for GDP granger causing consumption")
granger_gdp_to_con = sm.tsa.stattools.grangercausalitytests(data[['gdp','consumption',]], 5)

#=====================OPTIMAL LAG============================================#
                 
                 #1.reating consumption gdp dataframe
consump_gdp =data[['gdp','consumption']]
#differencing the consumption gdp dataframe and then creating new object
diff_consump_gdp = consump_gdp.diff().dropna()

                 #2.Code for optimal lag
optimal_lag = sm.tsa.VAR(diff_consump_gdp) 
lag_order = optimal_lag.select_order(maxlags=20)
print(lag_order.summary())

#============================VAR model========================================#

                  #===========VARMAX syntax======#
#Option of using level variables as you enforce stationarity : VARMAX
print('\nresults below for VAR on level variables with enforced stationarity')
var_model = sm.tsa.VARMAX(consump_gdp, order=(3,0), enforce_stationarity=True)
fit_VAR = var_model.fit()
print(fit_VAR.summary())


                   #====== VAR syntax=======#
#using differenced variables :VAR syntax
print('\nresults below for VAR on differenced variables')
var_diff_model = sm.tsa.VAR(diff_consump_gdp)
var_diff_model_fit = var_diff_model.fit(3)
print(var_diff_model_fit.summary())

#plotting results
var_diff_model_fit.plot()

#Visual autocorellation  plots 
var_diff_model_fit.plot_acorr()

#Impulse response functions
irf = var_diff_model_fit.irf(10)
irf.plot(orth=False)


#========================= FORECASTING========================================#
#established lag though we already know 3 was used
lag_used = var_diff_model_fit.k_ar

#print forecast values
print('5 period forecast \nPredicted consumption , Predicted gdp ')
print(var_diff_model_fit.forecast(diff_consump_gdp.values[-lag_used:], 5))


#===========================END===============================================#
