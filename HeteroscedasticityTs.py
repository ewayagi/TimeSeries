# -*- coding: utf-8 -*-
"""
Created on Monday July  4 04:22:57 2022
@authors: kkitonga and ewayagi
Content: Regression (Static model in Time Series): Heteroscedasticity Tests 
Data Source: https://data.mendeley.com/datasets/v9679528f7/1
"""

#===========================Libraries=========================================#
from statsmodels.compat import lzip
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.stats.diagnostic as dg
import statsmodels.stats.api as sms

#===========================Loading Data======================================#

#Importing inflation csv
#Use the path directory for you computer 
inflation = pd.read_csv (r'C:\Users\ewaya\OneDrive\Desktop\Py, R and Stata\Py\data_inflation.csv')

#Converting to dataframe and selecting only column on argentina
infDf = pd.DataFrame(inflation,columns= ["ARG"])

#Renaming column arg to inflation
infDf=infDf.rename(columns={"ARG":"inf"})

#Seeing output
print(infDf)

#Importing unemployment csv
unemp= pd.read_csv(r'C:\Users\ewaya\OneDrive\Desktop\Py, R and Stata\Py\data_unemployment.csv')

#Convert unemp to dataframe
unempDf = pd.DataFrame(unemp,columns=["ARG"])

#Rename ARG column
unempDf=unempDf.rename(columns={"ARG":"unemployment"})

#View dataframe
print(unempDf)

#Creating merged data frame of unemployment and infaltion for argentina
tdDF= pd.concat([unempDf,infDf],axis="columns")

#Drop Nan rows
tdDF=tdDF.dropna()
print(tdDF)

#========================Data Visualization===================================#

#Scatter plot:ScatInfUnPy
tdDF.plot(x='unemployment',y='inf',kind='scatter',s=100,color='purple')
plt.title("Scatterplot of inflation versus unemployemnt for argentina")

#==============================Descritives====================================#

print(tdDF.head())                       #First five observations
print(tdDF.tail())                       #last five observations
print (tdDF.describe())                  #descriptive statistics


#=============================Running Regression==============================#

#Define independent and dependent variables
X=tdDF['unemployment']
Y=tdDF['inf']

#Add a constant
X=sm.add_constant(X)

#Define model
model = sm.OLS(Y, X)

#Fit model
model_result = model.fit()

                        #Regression Result
print(model_result.summary())

                     #Generating residuals and fitted values
#Fitted values    
tdDF['Fitted'] = model_result.predict()

#Residuals (manually) :observed infaltion -fitted values
tdDF['Residual'] =  tdDF['inf']-tdDF['Fitted']

#Residuals :Automatically
tdDF['resids2'] = model_result.resid

#Squared residuals
tdDF['Residualsq'] = tdDF['Residual'] * tdDF['Residual']

#=====================Heteroscedasticity Tests================================#

         #=======Visual Inspection:Residual plots======#

                #Plot 1. Residual plots (ResPltUnePy)
                    #Modify figure size
fig = plt.figure(figsize=(14, 8))
                #Creating regression plots
fig = sm.graphics.plot_regress_exog(model_result, 'unemployment', fig=fig)

             #Plot2. Residuals squared versus fitted values (ScatResFitPy)
tdDF.plot(x='Fitted',y='Residualsq',kind='scatter')
plt.title( "Scatter of squared residual values vs fitted")

              #Plot3. Residuals squared versus explanatory (ResSqUnePy)
tdDF.plot(x='unemployment',y='Residualsq',kind='scatter',color='purple',s=50)
plt.title( "Scatter of squared residual values vs unemployment")

#========================Statistical Tests====================================#
                            
                    #Breusch pagan
Bp_output = ['Breusch-pagan Lagrange multiplier test statistic', 'p-value',
         'f-value', 'f p-value']
test_result = np.round(sms.het_breuschpagan(model_result.resid, model_result.model.exog),2)
print(dict(lzip(Bp_output, test_result)))

                    #White test
WhiteTestLabs=['White test Lagrange multiplier test statistic', 'p-value',
         'f-value', 'f p-value']
white_test = np.round(sms.het_white(model_result.resid,  model_result.model.exog),2)
print(dict(lzip(WhiteTestLabs, white_test)))

#==================================END=======================================#


