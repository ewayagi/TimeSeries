
#author:kkitonga and ewayagi
#04/07/2022
#Content:Regression(Static model in time series):Heteroskedasticity
#Data source:https://data.mendeley.com/datasets/v9679528f7/1
#==========================INSTALL PACKAGES=========================#

#Do this step if you DO NOT have the packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("olsrr")
install.packages("lmtest")

#=======================LOAD LIBRARIES==============================#

library(dplyr)
library(ggplot2)
library(olsrr)
library(lmtest)

#=======================DATA========================================#

datainf <- read.csv("C:/Users/Karengi/Desktop/PythonStat/PyData/datainf.csv")
dataunemp <- read.csv("C:/Users/Karengi/Desktop/PythonStat/PyData/dataunemp.csv")


#1.extracting unemployment column for Argentina
UnempDf <-dataunemp %>%
          select(column="ARG") %>%
          rename(Unemployment=column)
  
#2.extracting inflation column for Argentina
InfDf <-datainf %>% 
        select(column="ARG") %>%
        rename(Inflation=column)

#3.Merging Dataframes: InfDf and UnempDf to create TsDf
TsDf <-bind_cols(UnempDf,InfDf)
TsDf <- na.omit(TsDf)  

#=======================DATA VISUALIZATION==========================#

#Scatter plot:ScatInfUnR
TsDf %>%
        ggplot(aes(Unemployment,Inflation))+
        geom_point(color="purple")+
        labs(title="Scatter plot of inflation versus unemployemnt for argentina")

#=======================DESCRIPTIVES================================#

head(TsDf)
tail(TsDf)
summary(TsDf)

#=======================RUNNING REGRESSION==========================#

#1.Define independent and dependent variables
  #independent:Unemployment
  #Dependent;Inflation

#2.Define and fit the model
RegInf <- lm(Inflation~Unemployment,TsDf)

#3.Regression results
summary(RegInf)

               #generating residuals and fitted values

#4.fitted values
Fitted <- fitted(RegInf)
#5.residuals
res<- residuals(RegInf)
#6.squared residuals
ressquare<-(res^2)

#7.Creating a data frame of residual and fitted values
#to help in ggplot when referring to data frame

fit_res_df <- data.frame(Fitted = fitted(RegInf),
                         res= residuals(RegInf),
                         ressquare =(res^2),
                         unemployment=TsDf$Unemployment
                         )

#======================Heteroskedasticity tests=====================#

         #==============Visual inspection=================#

#Plot 2.residuals squared versus fitted values (ScatResFitR)
plot(fitted(RegInf),
     y=ressquare,
     main="heteroskedasticity",
     xlab="fitted",
     ylab="squared residuals")
  
                        #or
fit_res_df %>%
              ggplot(aes(x=Fitted,y=ressquare)) +
                    geom_point(color="purple")+
                    labs(x="fitted values", y="squared residuals", 
                    title= "Scatter of squared values vs fitted values")

#Plot3.residuals squared versus explanatory (ResSqUneR)
plot(x=TsDf$Unemployment,
     y=ressquare,
     main="Scatter of squared values vs unemployment",
     xlab="unemployment",
     ylab="squared residuals"
     )
                           #OR

fit_res_df%>%
            ggplot(aes(x=unemployment,y=ressquare)) +
                  geom_point(color="purple")+
                 labs(x="unemployment", y="squared residuals", 
                title= "Scatter of squared values vs unemployment")


#==========================STATISTICAL TESTS========================#

                    #1.Breush pagan
bptest(RegInf)                                    #lm test package
ols_test_breusch_pagan(RegInf)                    #olsrr package

                    #2.F-test
#fitted values of model
ols_test_f(RegInf)

#use independent variables in the model
ols_test_f(RegInf, rhs = TRUE)

                    #3.Score test
#fitted values of the model
ols_test_score(RegInf)

#independent variables
ols_test_score(RegInf, rhs = TRUE)

#============================END====================================#
