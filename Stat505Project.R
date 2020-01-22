setwd("C:/Users/steph/OneDrive/Documents/Stats505/")

#IMPORTING LIBRARIES

library(aTSA)
library(stats)
library(lmtest)

#IMPORTING DATA

df=scan(paste("jj.txt", sep=""))
logdf=log(df)

df.ts=ts(data=df,frequency=4,start=1960, end = 1980)
logdf.ts=ts(data=logdf,frequency=4,start=1960, end = 1980)

earnings<-logdf.ts

#PLOTTING TIMESERIES, ACF, and PACF

par(mfrow=c(2,1))

plot.ts(df.ts, 
        main = "Timeseries of Observed Earnings",
        xlab = "Time",
        ylab = "Earnings per Share" )

plot.ts(logdf.ts, 
        main = "Timeseries of Observed Log Earnings",
        xlab = "Time",
        ylab = "Earnings per Share")

Autocorrelation <- acf(df.ts, lag.max = 12, plot = FALSE)
plot(Autocorrelation, main = "ACF of Earnings")
PartialAutocorrelation <- pacf(df.ts, lag.max = 12, plot = FALSE)
plot(PartialAutocorrelation, main = "PACF of Earnings")

Autocorrelation1 <- acf(logdf.ts, lag.max = 12, plot = FALSE)
plot(Autocorrelation1, main = "ACF of Log Earnings")
PartialAutocorrelation1 <- pacf(logdf.ts, lag.max = 12, plot = FALSE)
plot(PartialAutocorrelation1, main = "PACF of Log Earnings")

#DIFFERENCING TIMESERIES (1-B4)
NoSeaso.earnings = diff(df.ts, lag=1)
NoSeaso.earnings4 = diff(df.ts, lag=4)

plot.ts(NoSeaso.earnings, 
        main = "The Differenced series (1-B)" ,
        xlab = "Time",
        ylab= "Earnings per Share")
plot.ts(NoSeaso.earnings4, 
        main = "The Differenced series (1-B)^4" ,
        xlab = "Time",
        ylab= "Earnings per Share")

NoLogSeaso.earnings = diff(logdf.ts, lag=4)
NoTrend.Seaso.earnings = diff(NoLogSeaso.earnings, lag=1)
plot.ts(NoTrend.Seaso.earnings, 
        main = "The Difference Series (1-B)(1-B^4)X_t" ,
        x_lab = "Time",
        y_lab = "Earnings per Share")

#DIFFERENCING TIMESERIES (1-B)(1-B4)Xt
NoLogSeaso.earnings = diff(logdf.ts, lag=4)
NoTrend.Seaso.earnings = diff(NoLogSeaso.earnings, lag=1)

plot.ts(NoTrend.Seaso.earnings, 
        main = "The Difference Series (1-B)(1-B^4)X_t" ,
        x_lab = "Time",
        y_lab = "Earnings per Share")
par(mfrow=c(1,2))
Autocorrelation2 <- acf(NoTrend.Seaso.earnings, lag.max = NULL, xlab="Lag", plot = FALSE)
plot(Autocorrelation2, main = "ACF of Differenced Series")
PartialAutocorrelation2 <- pacf(NoTrend.Seaso.earnings, lag.max = NULL, xlab="Lag", plot = FALSE)
plot(PartialAutocorrelation2, main = "PACF of Differenced Series")

#CREATING MODELS
fit0=arima(earnings, order=c(0,1,0), seasonal = list(order=c(0,1,0), period=4))
fit1=arima(earnings, order = c(1,1,0), seasonal = list(order= c(0,1,0), period=4))
fit2=arima(earnings, order = c(1,1,1), seasonal = list(order=c(0,1,0), period=4))
fit3=arima(earnings, order = c(1,1,1), seasonal = list(order=c(1,1,0), period=4))
fit4=arima(earnings, order = c(1,1,1), seasonal = list(order=c(1,1,1), period=4))
fit5=arima(earnings, order = c(1,1,0), seasonal = list(order=c(1,1,1), period=4))
fit6=arima(earnings, order = c(1,1,0), seasonal = list(order=c(0,1,1), period=4))
fit7=arima(earnings, order = c(1,1,1), seasonal = list(order=c(0,1,0), period=4))
fit8=arima(earnings, order = c(0,1,1), seasonal = list(order=c(1,1,1), period=4))
fit9=arima(earnings, order = c(0,1,1), seasonal = list(order=c(0,1,0), period=4))
fit10=arima(earnings, order = c(0,1,1), seasonal = list(order=c(1,1,0), period=4))
fit11=arima(earnings, order = c(0,1,1), seasonal = list(order=c(0,1,1), period=4))
fit12=arima(earnings, order = c(0,1,0), seasonal = list(order=c(1,1,1), period=4))
fit13=arima(earnings, order = c(0,1,0), seasonal = list(order=c(1,1,0), period=4))
fit14=arima(earnings, order = c(0,1,0), seasonal = list(order=c(0,1,1), period=4))
fit15=arima(earnings, order = c(1,1,0), seasonal = list(order=c(1,1,0), period=4))

coeftest(fit0)
coeftest(fit1)
coeftest(fit2)
coeftest(fit3)
coeftest(fit4)
coeftest(fit5)
coeftest(fit6)
coeftest(fit7)
coeftest(fit8)
coeftest(fit9)
coeftest(fit10)
coeftest(fit11)
coeftest(fit12)
coeftest(fit13)
coeftest(fit14)
coeftest(fit15)

#MODEL SELECTION - CHECKING RESIDUALS
tsdiag(fit1)
tsdiag(fit5)
tsdiag(fit6)
tsdiag(fit8)
tsdiag(fit9)
tsdiag(fit10)
tsdiag(fit11)
tsdiag(fit13)
tsdiag(fit14)
tsdiag(fit15)

#MODEL SELECTION - CHECKING AIC 
AIC(fit1)
AIC(fit5)
AIC(fit6)
AIC(fit8)
AIC(fit10)
AIC(fit11)
AIC(fit15)


