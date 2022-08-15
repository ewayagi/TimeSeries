#==================CLEAR THE MEMORY============================#

rm(list=ls()) 
gc() 
cat("\f") 
ls() 

#==================INTRODUCTION================================#

#Author:kkitonga and ewayagi
#Date:10/08/2022
#content:VECM
#Special thanks:Justin Eloriaga channel
#https://www.youtube.com/watch?v=ZF6Ew9L7N_8

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

#==================LOAD DATA: 'AER'============================#

#i.load data
data(USMacroG)
#ii.about data
?USMacroG
#iii.print data
print(USMacroG)
#iv.duplicating data
USmacroG1 <- na.omit(data.frame(USMacroG))

#==================DESCRIPTIVES:'BASE R & DataExplorer=========#

#I.Base R
head(USMacroG)
tail(USMacroG)
str(USMacroG)
View(USMacroG)
summary(USMacroG)

#II.DataExplorer package
create_report(USMacroG)

#==================VISUALIZATION===============================#

#1.cpi
plot(cpi,
     main="Trends in cpi",
     col="blue")
#2.gdp
plot(gdp,
     main="Trends in gdp",
     col="purple")
#3.m1
plot(m1,
     main="Trends in m1",
     col="blue")

#==================DATA SUBSETTING=============================#

#Sub-setting : creating df of cpi,gdp and m1
cpi <- print(ts(USmacroG1$cpi,start=c(1950,1),frequency=4))
gdp <- print(ts(USmacroG1$gdp,start=c(1950,1),frequency=4))
m1 <- print(ts(USmacroG1$m1,start=c(1950,1),frequency=4))
data <- cbind(cpi,gdp,m1)

#==================OPTIMAL LAG SELECTION :'vars'===============#
 
#i.VARselect syntax details
??VARselect

#ii.Optimal lag
optimal_lag <- VARselect(data,lag.max=10,type=c('const'))
print(optimal_lag)
optimal_lag$selection                         #we go with 5 

#==================COINTEGRATION: 'urca'=======================#

#i.ca.jo syntax details
??ca.jo

#ii.Cointegration test
johanson_test <- ca.jo(data,type="trace",ecdet='const',K=4)
summary(johanson_test)        #at least 2 cointegrating rlshnpS

#==================VECM:'tyDyn'================================#

#i.VECM synatx details
??VECM

#II.VECM syntax 
#4=level tested for Cointegration
#2:no of Cointegrating relationships

vecm <- VECM(data,4,r=2,estim=c("ML"))
summary(vecm)

#==================VECM TO VAR:'vars'==========================#

#i.vec2var syntax details
??vec2var

#ii.transformation:#Vecm to Var
vecm_var <- vec2var(johanson_test,r=2)

#==================DIAGNOSTICS:'vars'==========================#

                    #I.autocorellation

#i.serial.test syntax details
??serial.test
#ii.autocorrelation syntax
autocorr <- serial.test(vecm_var,lags.pt = 16, type = c("PT.asymptotic"))
autocorr             

                  #ii.Heteroskedasticity
#i.heteroskedasticity syntax details
??arch.test
#ii.heteroskedasticity syntax
hkesdast <- arch.test(vecm_var, lags.single = 16, lags.multi = 5, multivariate.only = TRUE)
hkesdast            

                  #iii.normality test
#i.normality syntax details
??normality.test
#ii.Normality test
normality <- normality.test(vecm_var, multivariate.only = TRUE)
normality                                

#=================IMPULSE RESPONSE FUNCTIONS:'vars'============#

#I.irf syntax details
??irf

#II.impulse function: impulse(gdp),response(cpi)
vecm_irf <-irf(vecm_var, impulse = 'gdp', response = 'cpi', n.ahead = 10)
plot(vecm_irf)

#II.impulse function: impulse(gdp),response(m1)
vecm_irf_m1 <-irf(vecm_var, impulse = 'gdp', response = 'm1', n.ahead = 10)
plot(vecm_irf_m1)

#==================VARIANCE DECOMPOSITION======================#

#i.fevd syntax details
??fevd

#ii.variance decomposition
var_decomp <- fevd(vecm_var,n.ahead=10)
plot(var_decomp)    #maximize plot window to expand plot margins

#==================ENDING R SESSION============================#
dev.off()
rm(list=ls())
gc() 
cat("\f")

