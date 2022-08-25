#==================CLEAR THE MEMORY============================#

rm(list=ls()) 
gc() 
cat("\f") 
ls() 

#==================INTRODUCTION================================#

#Author:kkitonga
#Date:15/08/2022
#content:Var Testing

#==================INSTALL PACKAGES============================#

#install.packages("urca")          #do this only IF NOT installed
#install.packages("AER")           #do this only IF NOT installed
#install.packages("vars")          #do this only IF NOT installed
#install.packages("DataExplorer")  #do this only IF NOT installed
#install.packages("tsDyn")         #do this only IF NOT installed

#==================LOAD LIBRARIES==============================#

library(AER)
library(urca)
library(DataExplorer)
library(vars)
library(tsDyn)
library(tseries)
library(lmtest)

#==================LOAD DATA: 'AER'============================#

#i.load data
data(USMacroG)
#ii.about data
?USMacroG
#iii.print data
print(USMacroG)
#iv.duplicating data
USmacroG1 <- na.omit(data.frame(USMacroG))

tail(USMacroG,n=12)
#====================Subsetting data============================#

#creating a data frame of cpi and dpi
dpi <- ts(USmacroG1$dpi,start=c(1950,1),end=c(1997,4),frequency=4)
consump <- ts(USmacroG1$consump,start=c(1950,1),end=c(1997,4),frequency=4)
df <- cbind(dpi,consump)

#===================VISUALIZATION===============================#

#1.Line Plots:Consumption and disposable personal income
plot(consump,
     ylab="consumption/dpi",
     col="blue",
     lty=1,
     main="Time series plots: consumption and dpi")
lines(dpi,col='dark red',lty=1)
legend(1950,5000,legend=c('consumption','dpi'),
       col=c('blue','dark red'),lty=c(1,1))


#2.Acf plots
acf(consump)
acf(dpi)

#3.Histograms
#i.disposable personal income
hist(dpi,
     col='dark magenta',
     freq=FALSE,
     main="Histogram of dpi")
#ii.consumption
hist(consump,
     col='dark magenta',
     freq=FALSE,
     main="Histogram of consumption")


#============STATIONARITY TESTING:LEVEL VARIABLES===============#

#1.adf test for consumption
adf.test(consump,k=12)

#2.adf test for dpi
adf.test(dpi,k=12)

#============DIFFERENCING:CHECKING STATIONARITY=================#

#1.Differencing consumption then checking stationarity
adf.test(diff(consump,lag=1))

#2.Differencing dpi then checking stationarity
adf.test(diff(dpi,lag=1))

#non-stationary

#=========PLOTTING:LEVEL VERSUS DIFFERENCED DATA===========#

#i.Taking first differences
consump_diff <-diff(consump,1)
dpi_diff <-diff(dpi,1)

#ii.Creating dataframe of differenced data
consump_dpi_diff <-cbind(consump_diff,dpi_diff)

#iii.Viewing data frame
View(consump_dpi_diff)

#IV.Subplots:consumption versus differenced

par(mfrow=c(1,2))
plot(consump,
          ylab="Consumption",
          col="blue",
          lty=1,
          main="Trends in consumption")
plot(consump_diff,
     ylab="Differenced consumption",
     col="blue",
     lty=1,
     main="Trends:Differenced consumption")
par(mfrow=c(1,1))

#V.Subplots:dpi versus differenced dpi
par(mfrow=c(1,2))
plot(dpi,
     ylab="Disposable personal income",
     col="blue",
     lty=1,
     main="Trends in DPI")
plot(dpi_diff,
     ylab="Differenced disposable personal income",
     col="blue",
     lty=1,
     main="Trends:Differenced DPI")
par(mfrow=c(1,1))
  
#===============GRANGER CAUSALITY=========================#

#i.consumption causing dpi
grangertest(consump,dpi,order=4)
#ii.dpi causing consumption
grangertest(dpi,consump,order=4)

#============TRAINING VS TESTING DATASET===================#

#I.Create training dataset
#consump_dpi_diff
con_diff_tr <- diff(USmacroG1$consumption,lag=1)
dpi_diff_tr <- diff(USmacroG1$dpi,lag=1)
train_df <- cbind(con_diff_tr,dpi_diff_tr)


#ii.Create test dataset
con_diff <- diff(USmacroG1$consumption,lag=1)
dpi_diff <- diff(USmacroG1$dpi,lag=1)

#iii.training dataset
test_df <- data.frame(cbind(con_diff[191:202],dpi_diff[192:201]))

#================LAG OPTIMALITY============================#

#i.Information on VAR select syntax
??VARselect
#ii.Establishing optimal lag
optimal_lag <-VARselect(consump_dpi_diff, lag.max = 10)
#iii.Getting information on selection criteria
optimal_lag$selection                      #optimal:3

#==================VAR MODEL===============================#
#i.Information on var model
??VAR
#ii.runing var model
var_model <- VAR(consump_dpi_diff, p = 3, type = c("const", "trend", "both", "none"),
                 season = NULL, exogen = NULL, lag.max = NULL,
                 ic = c("AIC", "HQ", "SC", "FPE"))
#iii.Output of var model
summary(var_model)

#iv.generating forecasts
forecast <-predict(var_model,n.ahead = 12, ci = 0.95, dumvar = NULL)

#=====================DIAGNOSTICS==========================#

                    #i.serial autocorellation

autocorr <- serial.test(var_model,lags.pt = 16, 
                         type = c("PT.asymptotic"))
#autocorrelation result                                            
autocorr

                    #ii.Heteroskedasticity
hetesk <- arch.test(var_model,lags.multi=5, multivariate.only=TRUE)
hetesk

              #iii.Normality :Residual distribution
normality <- normality.test(var_model,multivariate.only=TRUE)
normality

var_stab <-stability(var_model, type = c("OLS-CUSUM"))
plot(var_stab)

#================Impulse response functions================#

#i.Consumption shock
 

con_dpi_irf <- irf(var_model,impulse='consump_diff',response='dpi_diff',n.ahead=10)
plot(con_dpi_irf,main="consumption shock impacts")

#ii.Disposable Personal Income shock

dpi_con_irf <- irf(var_model,impulse='dpi_diff',response='consump_diff',n.ahead=10,boot=TRUE)
plot(dpi_con_irf,main= "Disposable personal income shock impacts")

#==============Variance decomposition=====================#

#I.About fevd
??fevd

#ii.Variance decomposition
var_fevd <- fevd(var_model,n.ahead=10)
plot(var_fevd)


#=================FORECASTING==============================#

#i.About forecast synatx
??forecast

#ii.Plotting forecast
forecast <- predict(var_model,n.ahead=12,ci=0.95)
plot(forecast)

#iii.Plotting a fanchart
fanchart(forecast,names='dpi_diff')
fanchart(forecast,names='consump_diff')


#======================END OF SESSION======================#
dev.off()
rm(list=ls())
gc() 
cat("\f")
