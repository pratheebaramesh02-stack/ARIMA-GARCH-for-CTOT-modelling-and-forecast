
#TIME SERIES ANALYSIS OF INDIA'S COMMODITY TERMS OF TRADE


#In a nutshell, we use R to understand, analyse and forecast our data
#We do this by checking for stationarity, seasonality, volatility, 
#identifying which model would be a good fit, forecast and 
#interpret various diagnostics and statistics for meaningful conclusions

###############################################################################

#we start by clearing variables in workspace
rm(list = ls())

#installing packages
install.packages("ggplot2")
install.packages("zoo")
install.packages("readxl")
install.packages("tseries")
install.packages("fpp2")
install.packages('aTSA')
install.packages("FinTS")
install.packages("rugarch")

#importing packages
library(ggplot2)
library(zoo)
library(readxl)
library(tseries)
library(fpp2)
library(aTSA)
library(FinTS)
library(rugarch)

################################################################################

#importing the excel sheet
amfe <- read_excel("C:/Users/91994/Desktop/amfe_project/amfe_data.xlsx")
View(amfe)

#making the period in monthly format
amfe$Period <- as.yearmon(amfe$Period, format = "%b %Y")

#making the period in the date format
amfe$Period <- as.Date(amfe$Period)

#renaming the data column
colnames(amfe)[2] ="ToT"

#removing extra 2 months
amfe <- amfe[-c(517,518),]

#Splitting the data into in-sample (41 years) and out-sample (2 years)
df1 <- amfe[amfe$Period < as.Date("2021-01-01"), ]
df2 <- amfe[amfe$Period >= as.Date("2021-01-01"), ]

#Converting the data to time series object
Y <- ts(df1[,2],start = c(1980,1),frequency = 12)

################################################################################

#Time plot before making the data stationary
autoplot(Y) +
  ggtitle("Time plot before stationary adjustments") +
  ylab("ToT")
#Hints of non-stationarity from the visual

#Performing ADF test
adf.test(Y)
#p values > 0.05, failing to reject H0 ie data is non-stationary

#Differencing once
DY <- diff(Y)

#Time plot after differencing
autoplot(DY) +
  ggtitle("Time plot after stationary adjustments") +
  ylab("ToT")

adf.test(DY)
#p value = 0.01 (<0.05), rejecting H0 ie data has become stationary

kpss.test(DY)
#p value = 0.1 (>0.05), failing to reject H0 ie data has become stationary

################################################################################

#Plotting ACF of data
ggAcf(DY)
#Lags are significant at 1, 8, 24

#Plotting PACF of data
ggPacf(DY)
#Lags are significant at 1, 2, 8, 10

#Plotting seasonal sub series plot
ggsubseriesplot(DY) + 
  ggtitle("Subseries plot")
#The plot shows that there isn't much visible seasonality across the months - indicating we may not need a seasonal model                                                                                                                                          

################################################################################

#Fitting an ARIMA to the data

#combinations tried - (1,1,1),(2,1,1),(8,1,1),(10,1,1),(1,1,8),(2,1,8),(8,1,8),(10,1,8)

#we have tried the above combinations and we chose the model based on:
#information criterion tests(AIC and BIC - BIC being given more weight when there were conflicting AIC and BIC among models)
#results from model error measures and Ljung-Box test
#RMSE and MAPE of the forecasts

#The ideal combination has turned out to be the ARIMA(1,1,1)

#Fitting the ARIMA to the data
arima_model <- arima(Y, order = c(1, 1, 1))

#Diagnostic checks for the model - error measures and residuals
print(summary(arima_model))
checkresiduals(arima_model)
#The estimated AR coefficient for the model is 0.2429
#The estimated MA coefficient for the model is 0.1722

#Error measures (rounded off to 4 decimals):
#ME = -0.0016; RMSE = 0.2758; MAE = 0.1849; MPE = -0.00169; MAPE = 0.1782; MASE = 0.9626, ACF1 = 0.0040

#Results from Ljung-Box test (for upto 22 lags): 
#p value = 0.2961(>0.05) - failing to reject the null hypothesis that residuals are not auto correlated

#Information criterion tests for the model
BIC(arima_model)
AIC(arima_model)
#BIC = 148.0729; AIC = 135.4835

#Forecasting using the fitted ARIMA (For 24 months i.e. 2 years)
focst <- forecast::forecast(arima_model,h=24)
print(summary(focst))

#Calculating RMSE
point_forecasts_2 <- focst$mean
differences <- point_forecasts_2 - df2$ToT
rmse <- sqrt(mean(differences^2))
print(rmse)
#RMSE = 2.473274

#Calculating MAPE
mape <- mean(abs((df2$ToT - point_forecasts_2) / df2$ToT)) * 100
print(mape)
#MAPE = 2.199158

################################################################################

#Having fitted and forecasted with the ARIMA model, we notice fluctuations in the ARIMA residuals which hints at volatility clustering

#ARCH test to find out if there is heteroskedasticity in the data
residual <- residuals(arima_model)
ArchTest(residual)
#p value < 2.2e-16 - rejecting the HO of no ARCH effects

#rechecking with additional diagnostic checks (PQ test and LM test)
arch_test_result <- arch.test(arima_model)
#p values for both tests for different orders are significant (rejecting the H0 of no heteroskedasticity)

#We confirm that the data indeed has volatility clustering. We thus find an apt GARCH model to fit an ARIMA-GARCH to our data

################################################################################

#Specifying and Fitting ARIMA GARCH
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(1,1)))
#The GARCH(1,1) has been chosen as a given as empirical testing and evidence shows that a GARCH(1,1) always suffices

fit.garch <- ugarchfit(spec = garch_spec, data = DY)
#Note - we specify an ARMA instead of ARIMA and feed in the first differenced data to satisfy ARIMA(1,1,1)
#The Rugarch package doesn't explicitly allow us to add the d component of ARIMA (it is by default=0)

print(fit.garch)
#The estimated mean parameter (mu) = 0.001937
#The estimated AR coefficient =  -0.082144
#The estimated MA coefficient = 0.468001
#The estimated white noise term (omega) = 0.001027
#The estimated ARCH coefficient = 0.293976
#The estimated GARCH coefficient = 0.705024

#IC tests: AIC = -0.38368, BIC =  -0.33240, SIC =  -0.38397, HQIC =  -0.36354

#Weighted Ljung Box test results show no serial autocorrelation
#Weighted ARCH LM test results show no significant ARCH effects
#Sign bias test shows that there is a significant overall sign bias in residuals
#Adjusted Pearson test results show that model residuals fit the normal distribution well

#Forecasting the next 24 values (2 years) using this model
arg_frcst <- ugarchforecast(fit.garch, data = NULL, n.ahead = 24, n.roll = 0)
mean_forecasts <- arg_frcst@forecast$seriesFor
print(mean_forecasts)

#The forecasts are on the differenced data, we sum it to get the forecasts on the original data
forecast_original <- cumsum(c(103.02842, mean_forecasts))[-1]

#Converting it to a time series object
ag_foct <- ts(forecast_original,start = c(2021,1),frequency = 12)

#Calculating the RMSE
difference <- ag_foct - df2$ToT
rmse_2 <- sqrt(mean(difference^2))
print(rmse_2)
#RMSE of ARIMA-GARCH = 2.6441785 (>RMSE of ARIMA 2.47)

#Calculating the MAPE
mape_2 <- mean(abs((df2$ToT - ag_foct) / df2$ToT)) * 100
print(mape_2)
#MAPE of ARIMA-GARCH = 2.379089 (>MAPE of ARIMA 2.2)

################################################################################

#Visualizing actuals vs forecasts

#converting test data to time series object
actual_ts <- ts(df2[,2],start = c(2021,1),frequency = 12)

#Plotting the actuals, predictions of ARIMA and ARIMA GARCH
combined_ts <- ts.union(actual_ts, point_forecasts_2,ag_foct)
colnames(combined_ts) <- c("Actuals", "ARIMA", "ARIMA-GARCH")

#Plotting the two for comparison
autoplot(combined_ts,size=1)+
  ggtitle("Actual vs Forecasts") +
  ylab("values")+
  ylim(90, 105)+
  scale_colour_manual(values = c("Actuals" = "black", "ARIMA" = "red", "ARIMA-GARCH" = "blue"))
#We see that the forecasts of both models are very close to each other and reasonably close to the forecasts.

################################################################################

