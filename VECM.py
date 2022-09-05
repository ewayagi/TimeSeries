# -*- coding: utf-8 -*-
"""
Created on Mon Aug 15 01:03:57 2022

Content: VECM

@author: ewayagi and kkavengi
"""

#===================================libraries=================================#

import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt

#=========================loading the data from R package=====================#

mydata = sm.datasets.get_rdataset("economics", "ggplot2").data

#view the names of the variables 
print(mydata.columns.values)

#==================Resampling the data to monthly prices======================#

mydata['date'] = pd.to_datetime(mydata['date'])
mydata = mydata.set_index('date') 
annualdata = mydata.resample('Y').mean()
print(annualdata)

#==========================subsetting the data================================#

mydata = annualdata[["pce" , "psavert"]]

#=================================plots=======================================#

plt.figure()

ax = plt.subplot(211)
ax.plot(mydata["pce"])

ax = plt.subplot(212)
ax.plot(mydata["psavert"])

#=============================lag selection===================================#

from statsmodels.tsa.vector_ar.vecm import select_order

lags = select_order(data = mydata, maxlags = 20, deterministic="ci")
print(lags.summary())
print(lags.aic, lags.bic, lags.fpe, lags.hqic)

# we select 12 lags

#========================cointegration test===================================#

from statsmodels.tsa.vector_ar.vecm import select_coint_rank

cointest = select_coint_rank(mydata, 0, 12, method="trace", signif= 0.05)
print(cointest.summary())

# we select 1 cointegration relations 

#===================================VECM model================================#

from statsmodels.tsa.vector_ar.vecm import VECM

vecm_model = VECM(mydata, k_ar_diff = 12, coint_rank = 1, deterministic = 'ci')
modelfit = vecm_model.fit()
print(modelfit.summary())

#=============================forecasting=====================================#

print(modelfit.predict(steps=10))

#=============================forecasted plots================================#

modelfit.plot_forecast(steps=10, plot_conf_int=False)

#================================END==========================================#