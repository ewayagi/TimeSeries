# -*- coding: utf-8 -*-
"""
Created on Tue Jul 12 22:48:03 2022

@author: kkitonga and ewayagi
"""

#===========================IMPORT LIBRARIES=================================#

import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.tsaplots import plot_pacf
import statsmodels.stats.diagnostic as dg
from statsmodels.compat import lzip
#=========================LOAD DATA===========================================#

#loading the longley dataset from the sm package
data = sm.datasets.longley.load_pandas()

#=========================DESCRIPTIVES========================================#

#find out exogeneous variables as defined within inbuilt dataset
print(data.exog)

#find out exogeneous variables as defined within inbuilt dataset
print(data.endog)

#find out variable names
print(data.names)

#view whole dataset
print(data.data)

#=================================REGRESSION==================================#

#specify depedent and indepedent variables:endog=y; exog=x
y, x = data.endog, data.exog

#model specification and fitting
reg = sm.OLS(y, x).fit()

#viewing regression output
print(reg.summary())

#=======================AUTOCORELLATION=======================================#


                      #1.Visual inspection plot
#Autocorrelation plot:data =data and endog =dependent variable
#Plot name:acfPlotPy
plot_acf(data.endog)
plt.show()

                      #2.Statistical tests

                #i.Breusch godfrey:goes beyond one lag

#creating a list to make test names
Bp_output = ['Lagrange multiplier test statistic', 'p-value','f-value',
             'f p-value']

#object to hold test results
test_result = np.round(sm.stats.diagnostic.acorr_breusch_godfrey
                       (reg, nlags=2, store=False),2)

#display output in form of a dictionary
print(dict(lzip(Bp_output, test_result)))

                  #ii.durbin watson:allows only one lag
#In regression  results(dw allows for only one lag)

#=================================END=========================================#







