# ARIMA-GARCH-for-CTOT-Modelling-and-Forecast

In this project, India's historical Commodity Terms of Trade (CTOT) data has been analysed through univariate time series modelling. The ARIMA-GARCH hybrid model aids in this analysis by combining linear ARIMA model with nonlinear GARCH model to capture both the
conditional mean and conditional heteroskedasticity of CTOT estimates.

Non-Stationarity and Trends were noticed in the data. To ensure stationarity, differencing was applied, and stationarity was confirmed via the Augmented Dickey-Fuller (ADF) and KPSS tests. An ARIMA was fit, and fluctuations were observed in ARIMA residuals indicating presence of volatility clustering. Hence an ARCH test was performed and the presence of heteroskedasticity in the residuals of the dataset was confirmed which led us to apply the ARIMA-GARCH model. 

Both ARIMA and ARIMA-GARCH were then used to forecast data for 24 months. Both gave very close forecast estimates.
