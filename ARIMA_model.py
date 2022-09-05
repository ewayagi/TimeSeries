# -*- coding: utf-8 -*-
"""
Created on Fri Aug  5 10:01:47 2022

Content: ARIMA Model

@author: ewayagi and kkavengi
"""

#===================================libraries=================================#

import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt

#=========================loading the data from R package=====================#

arimadata = sm.datasets.get_rdataset("GoldSilver", "AER").data

arima_index = arimadata.reset_index()
print(arima_index)

arima_reset = arima_index.rename(columns = {"index" : "date"})
print(arima_reset)

#==================Resampling the data to monthly prices======================#

arima_reset['date'] = pd.to_datetime(arima_reset['date'])
arima_reset = arima_reset.set_index('date') 
monthly_prices = arima_reset.resample('M').mean()
print(monthly_prices)

#=================================plots=======================================#

arimagold = monthly_prices

arimagold['gold'].plot()

# this is a non-stationary series

#=========================arima parameters; p, d, q===========================#

from statsmodels.graphics.tsaplots import plot_acf

plot_acf(arimagold.gold)

#=======================determining the order of differencing, d==============#

plot_acf(arimagold.gold.diff()[1:])

plot_acf(arimagold.gold.diff()[2:])

# with first and second order differencing, there is no autocorrelation

#======================unit root or stationarity test=========================#

from statsmodels.tsa.stattools import adfuller

adftest1 = adfuller(arimagold['gold'])
print(adftest1[0])
print(adftest1[1])

adftest2 = adfuller(arimagold['gold'].diff()[1:])
print(adftest2[0])
print(adftest2[1])

adftest3 = adfuller(arimagold['gold'].diff()[2:])
print(adftest3[0])
print(adftest3[1])

# conclusion: choose d = 1 

#==============deterining the order of the autoregressive model, p============#

from statsmodels.graphics.tsaplots import plot_pacf

plot_pacf(arimagold.gold.diff()[1:])
plot_pacf(arimagold.gold.diff()[2:])

'''
conclusion: choose p = 1; 
let us use q = 2; from the ACF plots
'''

#====================================ARIMA model==============================#

from statsmodels.tsa.arima.model import ARIMA

arimamodel = ARIMA(arimagold.gold, order = (1, 1, 2))
arimamodel_fit = arimamodel.fit()
print(arimamodel_fit.summary())

#===================ploting the ARIMA model===================================#

#plotting the residuals and density functions

resid = pd.DataFrame(arimamodel_fit.resid)  
fig, ax = plt.subplots(1,2)  
resid.plot(title = "Residuals", ax = ax[0])  
resid.plot(kind = 'kde', title = 'Density', ax = ax[1])  
plt.show()


'''
the plots shows a fair distribution of residual errors around them mean with
a uniform variance 
'''

#============================predicted ARIMA model plot=======================#

from statsmodels.graphics.tsaplots import plot_predict

plot_predict(arimamodel_fit)

#==========predict the mean prices of gold for thr next 20 months=============#

predicted_values = arimamodel_fit.predict(start = len(arimagold),\
        end = len(arimagold) + 20, type = "levels").rename('predictions')
print(predicted_values)

#============================plot the predicted values========================#

predicted_values.plot()

#=========================================THE END=============================#