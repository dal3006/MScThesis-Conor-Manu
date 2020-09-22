### R Script Main results EMS data ###

# clear workspace
rm(list=ls())

# loading packages
library(matrixStats)
library(xtable)
library(forecast)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(tikzDevice)
library(xts)
library(lmtest)
library(tseries) 
library(fBasics)
library(urca)
library(gets)
library(dynlm)
library(stats)
library(graphics)
library(aTSA)
library(googledrive)
library(ggpubr)

# set seed for reproducibility
set.seed(123)

##### plotting the 1 year data size for the hourly traffic data (July 2018-June 2019)

# finds all CSV files on your google drive
drive_find(type = "csv", n_max = 30)  

# downloading a file from Gdrive to your working directory (only has to be run once)
drive_download("emergency_dispatches_bronx_hourly.csv", overwrite = TRUE)

# loading the data into R
ems_hr_1yr <- read.csv("/Users/Manu/emergency_dispatches_bronx_hourly.csv", header = TRUE)

# creating a ts object of the entire ems data
ems_hr_1yr_ts <- ts(ems_hr_1yr$X0, start = c(1,1), frequency = 24)

length(ems_hr_1yr_ts) # 8760

# Time Series:
# Start = c(1, 1) 
# End = c(365, 24) 
# Frequency = 24 

tail(ems_hr_1yr) # last observation: 2019-12-31 23:00

### preprocessing ###

# remove the last 216 observations i.e. the last 9 days (24hrs*9)
ems_hr_1yr <- ems_hr_1yr[1:8544,]

tail(ems_hr_1yr) # last observation: 2019-12-22 23:00

head(ems_hr_1yr) # first observation 2019-01-01 00:00:00

# creating a new ts object from the preprocessed data
ems_hr_1yr_ts <- ts(ems_hr_1yr$X0, start = c(1,1), frequency = 24)

length(ems_hr_1yr_ts) # 8400

# Time Series:
# Start = c(1, 1)
# End = c(350, 24)
# Frequency = 24

# xts object of the entire ems data

## Create date index
time.index <- as.POSIXct(ems_hr_1yr$FIRST_ASSIGNMENT_DATETIME, formula = "%Y-%m-%d %H:%M:%S")

# creating an xts object for the entire 1 year of data
ems_hr_1yr_xts <- xts(ems_hr_1yr$X0, order.by = time.index)
names(ems_hr_1yr_xts) <- "values" 

# xts object showing the last month incl entire test set (720 observations)

last_month <- ems_hr_1yr[7825:8544,]

head(last_month) # first observation 2019-11-23 00:00

tail(last_month) # last observation 2019-12-22 23:00

## Create date index
time.index2 <- as.POSIXct(last_month$FIRST_ASSIGNMENT_DATETIME, formula = "%Y-%m-%d %H:%M:%S")

# creating an xts object for the last month of data
last_month_xts <- xts(last_month$X0, order.by = time.index2)
names(last_month_xts) <- "values" 


### time plots 1 year and last month incl train set and test set ###

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/ems_1yr_timeplot.tex",width=6.5,height=3.5)

par(mfrow=c(1,1))
plot(ems_hr_1yr_xts, main = "Time Plot Entire Year", lwd=1) # xts plot whole data

#dev.off()


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/ems_lastmonth_timeplot.tex",width=6.5,height=3.5)

par(mfrow=c(1,1))
plot(last_month_xts, main = "Time Plot Last Month", col = "blue")   # plot of the last month 
addEventLines(events= xts(("Train   Test"),  as.Date("2019-12-16 00:00:00", format='%Y-%m-%d %H:%M:%S')), col="black", lty = "solid", srt=0, pos=1, offset=1.2, cex=1.2)

#dev.off()


### preprocessing ###

# remove the last 360 observations i.e. the last 15 days (24hrs*15)
ems_hr_1yr <- ems_hr_1yr[1:8400,]

tail(ems_hr_1yr) # last observation: 2019-12-16 23:00

head(ems_hr_1yr) # first observation 2019-01-01 00:00:00

# creating a new ts object from the preprocessed data
ems_hr_1yr_ts <- ts(ems_hr_1yr$X0, start = c(1,1), frequency = 24)

length(ems_hr_1yr_ts) # 8400

#### train test split ####

train <- window(ems_hr_1yr_ts, end = c(349,24))
test <- window(ems_hr_1yr_ts, start = c(350,1), end = c(350,24)) # test set of 24 days

length(train) # 8376
length(test) # 24

#### end train test split ####

#### summary statistics for the entire training set and the entire test set

sumstats_train <- basicStats(ts(train))

# summary stats for the entire test set (incl. hold out days for robustness)
ems_hr_1yr <- ems_hr_1yr[1:8544,]

ems_hr_1yr_ts <- ts(ems_hr_1yr$X0, start = c(1,1), frequency = 24)

test_complete <- window(ems_hr_1yr_ts, start = c(350,1), end = c(356,24)) # test set incl. hold out weekdays

length(test_complete) # 168

sumstats_test <- basicStats((ts(test_complete)))

# create a Dataframe
sum_stats <- cbind(sumstats_train, sumstats_test)

# rename columns
names(sum_stats) <- c("Training Set", "Test Set")

xtable(sum_stats)


#### Statistical models ####

# automated ETS model
ets_fc <- forecast:: forecast(ets(train), h=24)

summary(ets_fc) # A,N,A model


accuracy(ets_fc, test) # RMSE 6.1852 MAE 5.1225 MAPE 12.7363

### bootstrapped PIs for the ETS model

### using the bootrap option of the forecast.ets function to create prediction intervals

ets_bs_fc <- forecast:: forecast(ets(train), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level=0.95)

## Prediction interval accuracy
lower <- ets_bs_fc$lower
upper <- ets_bs_fc$upper

picp <- mean(test >= lower & test <= upper)# PICP _ 1.0 
mpiw <- mean(upper - lower)# MPIW _ 41.5736 

#### CWC (ets bootstrapped)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

# range() returns a vector with the min and max of the given arguments
# i.e. it gives the range of the target variable
range(test) # c(16,59) -> the range of y is 43

picp <- 1.0

mpiw <- 41.5735 

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_ets <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_ets # 0.9668

### end bootstrapped PIs for the ETS model



##### automated ARIMA model #### (to be updated)

auto_arima <- auto.arima(train) # fitted auto.arima model: SARIMA (4,0,0) (2,1,0)_24 

summary(auto_arima)

arima_fc <- forecast:: forecast(auto.arima(train), h=24)

accuracy(arima_fc, test) # RMSE 8.1699 MAE 7.2306 MAPE 20.1902

### bootstrapped PIs for the auto.arima model

### using the bootrap option of the forecast.ets function to create prediction intervals

autoarima_bs_fc <- forecast(auto.arima(train), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level=0.95)

## Prediction interval accuracy
lower <- autoarima_bs_fc$lower
upper <- autoarima_bs_fc$upper

picp <- mean(test >= lower & test <= upper)# PICP _ 0.9583 
mpiw <- mean(upper - lower)# MPIW _ 34.3559

#### CWC (ets bootstrapped)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

# range() returns a vector with the min and max of the given arguments
# i.e. it gives the range of the target variable
range(test) # c(16,59) -> the range of y is 43

picp <- 0.9583

mpiw <- 34.3559

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_autoarima <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_autoarima # 0.7990

# use autolayer(simfc) to plot the bootstrapped PIs

### end bootstrapped PIs for the autoarima model



### Tests for autocorrelation - Auto.Arima residuals ###

## test for autocorrelation -> H_0: no serial correlation vs. H_1: serial correlation
# According to Hyndman the number of lags for a seasonal time series can
# be chosen by the rule of thumb h = min(2m, T/5); here: 2m = 48, T/5 = 504/5 = 100.8

length(arima_fc$residuals) # 8376


Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 24)
# pvalue is < 0.0000

Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 48)
# pvalue is < 0.0000

Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 72)
# p-value is < 0.0000

Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 1)
# p-value = 0.9657
Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 2)
#p-value = 0.9904
Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 3)
#p-value = 0.9782

# autocorrelation at seasonal lags

### end autocorrelation tests ###

# normality tests

jarque.bera.test(arima_fc$residuals) # p.val < 0.0000

skewness(arima_fc$residuals) # 0.0875
kurtosis(arima_fc$residuals) # excess K: 0.1945

jarque.bera.test(manual_arima1_fc$residuals) # p.val < 0.0000

skewness(manual_arima1_fc$residuals) # 0.1011
kurtosis(manual_arima1_fc$residuals) # excess K: 0.1905

# ARCH tests (object: estimated arima model)

#autoarima
arch.test(object = arima(train,order=c(4,0,0), seasonal= c(2,1,0)), output=TRUE) 

## all p-alues are 0 (up to lag 24) -> evidence for ARCH effects

#manualarima  
arch.test(object = arima(train,order=c(4,0,0), seasonal= c(2,1,1)), output=TRUE) 


# stationarity tests (ADF TEST)

adf_train <- adf.test(x=train, nlag=97)


# as of lag 21 the series becomes nonstationary

adf_sdiff <- adf.test(x=diff(train,24), nlag = 97)

# seasonally differenced data is stationary!




#### Manual SARIMA modeling procedure ####

# check margins
par("mar")

# reset margins to default
par(mar=c(5.1,4.1, 4.1, 2.1))

# acf pacf checks

#train <- ts(train)

#par(mfrow=c(1,1))
#acf(train)

acf_train <- acf(ts(train), lag.max = 80)
pacf_train <- pacf(ts(train), lag.max = 80)

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/train_acf_pacf.tex",width=6.5,height=6.5)

par(mfrow=c(1,1))
plot(acf_train, ylim = c(-1,1), main = "ACF(training set)")

plot(pacf_train, ylim =c(-1,1), main = "PACF(training set)")

#dev.off()

# check residuals of the automatic model
acf(ts(arima_fc$residuals), lag.max=80, ylim = c(-0.5,0.5)) # spikes at slags 1,2,3
pacf(ts(arima_fc$residuals), lag.max=80, ylim = c(-0.5,0.5)) # spikes at slags 1,2,3

# -> add sMA terms and sAR terms (1st manual model)

# manual SARIMA model 1 # 1 more sMA term
manual_arima1 <- arima(train, order= c(4,0,0), seasonal = list(order=c(2,1,1)) ) # arima (4,0,0) (2,1,1)_24 model

manual_arima1_fc <- forecast:: forecast(manual_arima1, h=24)

accuracy(ts(manual_arima1_fc$mean), ts(test)) # RMSE 5.8663, MAE 4.8481, MAPE 12.5739


### using the bootrap option of the forecast.ets function to create prediction intervals

manualarima_bs_fc <- forecast(arima(train, order= c(4,0,0), seasonal = list(order=c(2,1,1))), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level=0.95)

## Prediction interval accuracy
lower <- manualarima_bs_fc$lower
upper <- manualarima_bs_fc$upper

picp <- mean(test >= lower & test <= upper)# PICP _ 1.0 
mpiw <- mean(upper - lower)# MPIW _ 28.5951

#### CWC (manual arima bootstrapped)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

# range() returns a vector with the min and max of the given arguments
# i.e. it gives the range of the target variable
range(test) # c(16,59) -> the range of y is 43

picp <- 1.0 

mpiw <- 28.5951

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_manarima <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_manarima # 0.6650


# check residuals of manual model 1 
acf(ts(manual_arima1_fc$residuals), lag.max=80, ylim = c(-0.5,0.5)) # no autocorrelation
pacf(ts(manual_arima1_fc$residuals), lag.max=80, ylim = c(-0.5,0.5)) # no autocorrelation

### autocorrelation tests manual arima model 1

Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 24)
Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 48)
Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 72)

Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 1)
Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 2)
Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 3)

## all p-values < 0.0000 ->  still autocorrelation

checkresiduals(manual_arima1_fc$residuals)

#### manual model 2 # 2 more sMA terms 

manual_arima2 <- arima(train, order= c(4,0,0), seasonal = list(order=c(2,1,2)) ) # arima (4,0,0) (2,1,2)_24 model

manual_arima2_fc <- forecast:: forecast(manual_arima2, h=24)

accuracy(manual_arima2_fc, test) # RMSE 5.9832, MAE 5.0943, MAPE 13.7923 

# -> choose manual_arima1 model as the manual candidate model 

### Estimation results for the manual SARIMA model 1

### extracting the IC values
# k contains the BIC penalty
bic <- AIC(manual_arima1, k = log(length(manual_arima1))) # 55,726.51

# custom AICc function
aicc = function(model){
  n = model$nobs
  p = length(model$coef)
  aicc = model$aic + 2*p*(p+1)/(n-p-1)
  return(aicc)
}

# compute the AICc
aicc(manual_arima1) # 55,721.41

# inference for estimated coefficients (x= object, vcov= covariance matrix of the est. coefficients)
coeftests <- coeftest(x=manual_arima1)

# values
coeftests <- coeftests[,]

# coefficients
xtable(coeftests)


# check residuals of manual model 2 
acf(ts(manual_arima2_fc$residuals), lag.max=80, ylim = c(-0.5,0.5))
pacf(ts(manual_arima2_fc$residuals), lag.max=80, ylim = c(-0.5,0.5))

### autocorrelation tests manual arima model 2

Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 24)
Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 48)
Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 72)

## all p-values < 0.0000 -> still autocorrelation


Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 1)
Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 2)
Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 3)

# normality test manual arima model 1

jarque.bera.test(manual_arima1_fc$residuals)

skewness(manual_arima1_fc$residuals)
kurtosis(manual_arima1_fc$residuals)




#### ACF plot of weekly level autocorrelation SARIMA vs BATS


acf_sarima <- acf(ts(manual_arima2_fc$residuals), lag.max=200)

acf_bats <- acf(ts(bats_fc$residuals), lag.max = 200)

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/weekly_comp_acf.tex",width=6.5,height=6.5)


par(mfrow=c(2,1))
plot(acf_sarima, ylim = c(-0.4,0.4), main = "ACF(SARIMA)")

plot(acf_bats, ylim =c(-0.4,0.4), main = "ACF(BATS)")

#dev.off()


#### TBATS model ####

# seasonal periods: 24 hours (day), 168=24*7 (week)
# tbats function with parallel processing
tbats_fc <- forecast :: forecast(tbats(train, seasonal.periods= c(24,168), use.parallel = TRUE, num.cores = NULL), h=24)

summary(tbats_fc) # TBATS(1, {4,4}, -, {<24,6>,<168,6>})

# extracting model parameter estimations
tbats_fc$model



accuracy(tbats_fc, test) # RMSE 5.1236 MAE 4.2746, MAPE 12.5474 

# decomposition by TBATS model
tbats_decomp <- tbats(train, seasonal.periods= c(24,168), use.parallel = TRUE, num.cores = NULL)

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/tbatsdecomposition_ems.tex",width=6.5,height=5.5)

plot(tbats_decomp)

#dev.off()



### bootstrapped PIs for the TBATS model

### using the bootrap option of the forecast.ets function to create prediction intervals

tbats_bs_fc <- forecast(tbats(train, seasonal.periods = c(24,168)), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level=0.95)

## Prediction interval accuracy
lower <- tbats_bs_fc$lower
upper <- tbats_bs_fc$upper

picp <- mean(test >= lower & test <= upper)# PICP _ 1.0 
mpiw <- mean(upper - lower)# MPIW _ 26.5626 

#### CWC (ets bootstrapped)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

# range() returns a vector with the min and max of the given arguments
# i.e. it gives the range of the target variable
range(test) # c(16,59) -> the range of y is 43

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_tbatsbs <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_tbatsbs # 0.6177


#### BATS model ####


# seasonal periods: 3 hours, 6 hours, 24 hours (day), 168=24*7 (week), 168*3=504 (month)
# fitting the bats model + forecasting
bats_fc <- forecast :: forecast(bats(train, seasonal.periods= c(24,168), use.parallel = TRUE, num.cores = NULL), h=24)

summary(bats_fc) # BATS(1, {0,0}, 0.833, {24,168})


accuracy(bats_fc, test) # RMSE 5.1989 MAE 3.7985 MAPE 9.9314


# decomposition by BATS model
bats_decomp <- bats(train, seasonal.periods= c(24,168), use.parallel = TRUE, num.cores = NULL)


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/batsdecomposition_ems.tex",width=6.5,height=5.5)

plot(bats_decomp)

#dev.off()


### using the bootrap option of the forecast.ets function to create prediction intervals

bats_bs_fc <- forecast(bats(train, seasonal.periods = c(24,168)), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level=0.95)

## Prediction interval accuracy
lower <- bats_bs_fc$lower
upper <- bats_bs_fc$upper

picp <- mean(test >= lower & test <= upper)# PICP _ 0.9583 
mpiw <- mean(upper - lower)# MPIW _ 25.2107 

#### CWC (bats bootstrapped)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

# range() returns a vector with the min and max of the given arguments
# i.e. it gives the range of the target variable
range(test) # c(16,59) -> the range of y is 43

picp <- 0.9583

mpiw <- 25.2107

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_batsbs <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_batsbs # 0.5863

### end bootstrapped PIs for the BATS model



##### Robustness check BATS wrt training size ### 


# 21/05/2019 to 21/06/2019
train_1month <- window(traffic_hr_1yr_ts, start = c(325,1), end= c(356,24))

test_1month <- window(traffic_hr_1yr_ts, start = c(357,1), end = c(357,24))

length(train_1month)

# 6months 21/1/2019 to 21/06/2019 test 22/06/2019  

train_6months <- window(traffic_hr_1yr_ts, start = c(205,1), end= c(356,24))

test_6months <- window(traffic_hr_1yr_ts, start = c(357,1), end = c(357,24))  

length(train_6months)

# Bats model 1 month

bats_fc_1mo <- forecast(bats(train_1month, seasonal.period=c(24,168)),h=24)

accuracy(bats_fc_1mo, test_1month) # RMSE 475.67 MAE 397.81 MAPE 4.4002 

# Bats model 6 months

bats_fc_6mo <- forecast(bats(train_6months, seasonal.period=c(24,168)),h=24)

accuracy(bats_fc_6mo, test_6months) # RMSE 731.09 MAE 631.46 MAPE 6.3299


#### bagged ETS model #### (doesn't support parallel processing)

# 24 step ahead forecasts from a bagged ETS model with 100 learners
baggedets_fc <- forecast:: forecast(baggedETS(train), h=24)


# test errors of the baggedETS model (arithmetic mean)
accuracy(baggedets_fc$mean, test) # RMSE 6.8980 MAE 5.7206 MAPE 13.4927


# test errors of the baggedETS model (median)
accuracy(baggedets_fc$median, test) # RMSE 6.8838 MAE 5.7131 MAPE 13.4602

#### to fix: bootstrapped series plot ####

# illustration plot with 10 bootstrapped series # 

## create bootstrapped series of weekly deseasonalized data
bootseries <- ts(as.data.frame(bld.mbb.bootstrap(train, num=10)), start = c(1,1), frequency = 168)

## plot bootstrapped series
autoplot(ts(train), xlab = "Time") +
  autolayer(bootseries, colour=TRUE) +
  autolayer(ts(train), colour=FALSE) +
  ylab("Bootstrapped Series") + guides(colour="none")

#### end to fix plot with bootstrapped series ####

# plotting the forecasts

autoplot(test, xlab = "Time", ylab = "Traffic flows") + 
  autolayer(baggedets_fc, series = "BaggedETS", PI = TRUE) +
  autolayer(test, series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

### bootstrapped PIs for the baggedETS model

# simulating future sample paths by bootstrapping the training data
nsim <- 100L
sim <- bld.mbb.bootstrap(train, nsim)

# For each of these series, fit an ETS model to each bootstrapped series, then refit to
# training data and simulate from refit.
h <- 24L
future <- matrix(0, nrow=nsim, ncol=h)
for(i in seq(nsim)) {
  model <- ets(sim[[i]])
  refit <- ets(train, model = model, use.initial.values = TRUE)
  future[i,] <- simulate(refit, nsim=h)
}


### 2nd step FFP2 code

# we take the means and quantiles of these simulated sample paths to form point forecasts and prediction intervals

start <- tsp(train)[2]+1/24
simfc_ets <- structure(list(
  mean = ts(colMeans(future), start=start, frequency=24),
  lower = ts(apply(future, 2, quantile, prob=0.025),
             start=start, frequency=24),
  upper = ts(apply(future, 2, quantile, prob=0.975),
             start=start, frequency=24),
  level=95),
  class="forecast")

### end PIs baggedETS model

# creating a forecast object with the simulated PIs

baggedets_fc$mean <- ts(baggedets_fc$median)
baggedets_fc$upper <- ts.union(ts(simfc_ets$upper), ts(simfc_ets$upper))
baggedets_fc$lower <- ts.union(ts(simfc_ets$lower), ts(simfc_ets$lower))
baggedets_fc$x <-  ts(c(1:24), start = c(350,1), end = c(350,24))

## Prediction interval accuracy
lower <- simfc_ets$lower
upper <- simfc_ets$upper

picp <- mean(test >= lower & test <= upper)# PICP _ 0.9583
mpiw <- mean(upper - lower)# MPIW _ 31.5055

#### CWC (baggedETS)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

picp = 0.9583

mpiw = 31.5055

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_baggedets <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_baggedets # 0.7327

### SNAIVE model

snaive_fc <- forecast(snaive(train), h=24)

# accuracy
accuracy(snaive_fc, test) # RMSE 11.0189 MAE 9.4167 MAPE 24.1957

# PI accuracy
lower <- snaive_fc$lower[,2]
upper <- snaive_fc$upper[,2]

picp <- mean(test >= lower & test <= upper)# PICP _ 0.9583

mpiw <- mean(upper - lower) # MPIW 38.8648

#### CWC (snaive)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_snaive <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_snaive # 0.9038


autoplot(test, xlab = "Time", ylab = "Traffic flows") + 
  autolayer(snaive_fc, series = "SNAIVE", PI = TRUE) +
  autolayer(test, series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


##### Group plot of all traditional and hybrid models ####

# download LGBM predictions and PI bounds

drive_download("LGBM_ems_df.csv", overwrite = TRUE)

lgbm_ems_df <- read.csv("/Users/Manu/LGBM_ems_df.csv", header = TRUE)



# creating a forecast object with the lgbm data

lgbm_fc <- forecast:: forecast(ets(train), h=24)

# replace PI bounds and point forecasts
lgbm_fc$mean <- ts(lgbm_ems_df$LGBM.predictions)

# hard code the 95% PIs
lgbm_fc$upper <- ts.union(ts(lgbm_fc$upper[,2]), ts(lgbm_fc$upper[,2]))
lgbm_fc$lower <- ts.union(ts(lgbm_fc$lower[,2]), ts(lgbm_fc$lower[,2]))

# replace by PI bound values
lgbm_fc$upper <- ts.union(ts(lgbm_ems_df$PI.upper.bound), ts(lgbm_ems_df$PI.upper.bound))
lgbm_fc$lower <- ts.union(ts(lgbm_ems_df$PI.lower.bound), ts(lgbm_ems_df$PI.lower.bound))

lgbm_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

## test error point forecasts LGBM

accuracy(lgbm_fc$mean, ts(test)) # RMSE 6.4795 MAE 5.4597 MAPE 14.8491

# PI accuracy
lower <- lgbm_fc$lower[,2]
upper <- lgbm_fc$upper[,2]

picp <- mean(ts(test) >= lower & ts(test) <= upper)# PICP _ 1.0

mpiw <- mean(upper - lower) # MPIW 26.6975

#### CWC (LGBM)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_lgbm <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_lgbm # 0.6209



# test plotting the LGBM predictions
autoplot(ts(test), xlab= "Time", ylab = "Traffic flows") +
  autolayer(lgbm_fc, series = "LGBM", color="blue", PI = TRUE) +
  autolayer(ts(test), series = "Test", show.legend =FALSE) +
  #guides(colour = guide_legend(title = "")) + 
  theme_bw()

# preprocessing the other forecast objects for plotting

tbats_fc$mean <- ts(tbats_fc$mean)
tbats_fc$upper <- ts.union(ts(tbats_fc$upper[,2]), ts(tbats_fc$upper[,2]))
tbats_fc$lower <- ts.union(ts(tbats_fc$lower[,2]), ts(tbats_fc$lower[,2]))
tbats_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

manual_arima1_fc$mean <- ts(manual_arima1_fc$mean)
manual_arima1_fc$upper <- ts.union(ts(manual_arima1_fc$upper[,2]), ts(manual_arima1_fc$upper[,2]))
manual_arima1_fc$lower <- ts.union(ts(manual_arima1_fc$lower[,2]), ts(manual_arima1_fc$lower[,2]))
manual_arima1_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

# hard coding the median of the baggedETS model into the fcobject for plotting
baggedets_fc$mean <- ts(baggedets_fc$median)
baggedets_fc$upper <- ts.union(ts(simfc_ets$upper), ts(simfc_ets$upper))
baggedets_fc$lower <- ts.union(ts(simfc_ets$lower), ts(simfc_ets$lower))
baggedets_fc$x <- ts(c(1:24),start = c(350,1), end = c(350,24))

bats_fc$mean <- ts(bats_fc$mean)
bats_fc$upper <- ts.union(ts(bats_fc$upper[,2]), ts(bats_fc$upper[,2]))
bats_fc$lower <- ts.union(ts(bats_fc$lower[,2]), ts(bats_fc$lower[,2]))
bats_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

lgbm_fc$mean <- ts(lgbm_ems_df$LGBM.predictions)
lgbm_fc$upper <- ts.union(ts(lgbm_ems_df$PI.upper.bound), ts(lgbm_ems_df$PI.upper.bound))
lgbm_fc$lower <- ts.union(ts(lgbm_ems_df$PI.lower.bound), ts(lgbm_ems_df$PI.lower.bound))
lgbm_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/plotgrid_traditional_ems.tex",width=5.5,height=6.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(tbats_fc, series = "TBATS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(manual_arima1_fc, series = "SARIMA", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p3 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(lgbm_fc, series = "LGBM", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p4 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(baggedets_fc, series = "BaggedETS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2,label.x= 0.33, labels=c("TBATS", "SARIMA", "LGBM", "BaggedETS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "none")


#dev.off()

### Comparison grid plot traditional and ensemble (bootstrapped PIs) ###

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/plotgrid_traditional_bs_ems.tex",width=5.5,height=6.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(bats_bs_fc, series = "BATS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(manualarima_bs_fc, series = "SARIMA", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p3 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(lgbm_fc, series = "LGBM", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p4 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(baggedets_fc, series = "BaggedETS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2,label.x= 0.33, labels=c("BATS", "SARIMA", "LGBM", "BaggedETS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "none")


#dev.off()

# preprocessing the fc models for plotting

manual_arima1_fc$mean <- ts(manual_arima1_fc$mean)
manual_arima1_fc$upper <- ts.union(ts(manual_arima1_fc$upper[,2]), ts(manual_arima1_fc$upper[,2]))
manual_arima1_fc$lower <- ts.union(ts(manual_arima1_fc$lower[,2]), ts(manual_arima1_fc$lower[,2]))
manual_arima1_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

arima_fc$mean <- ts(arima_fc$mean)
arima_fc$upper <- ts.union(ts(arima_fc$upper[,2]), ts(arima_fc$upper[,2]))
arima_fc$lower <- ts.union(ts(arima_fc$lower[,2]), ts(arima_fc$lower[,2]))
arima_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))


# grid plot with legend SARIMA auto vs manual


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/arima_comparison_ems.tex",width=6.5,height=4.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(arima_fc, series = "Auto SARIMA", PI = TRUE)  +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(manual_arima1_fc, series = "Manual SARIMA", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


ggarrange(p1, p2, ncol=2, nrow=1,labels=c("Auto SARIMA", "Manual SARIMA"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")

#dev.off()

### Manual vs Auto arima (bootstrapped PIs)

# preprocessing the fc models for plotting


manualarima_bs_fc$mean <- ts(manualarima_bs_fc$mean)
manualarima_bs_fc$upper <- ts.union(ts(manualarima_bs_fc$upper), ts(manualarima_bs_fc$upper))
manualarima_bs_fc$lower <- ts.union(ts(manualarima_bs_fc$lower), ts(manualarima_bs_fc$lower))
manualarima_bs_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

autoarima_bs_fc$mean <- ts(autoarima_bs_fc$mean)
autoarima_bs_fc$upper <- ts.union(ts(autoarima_bs_fc$upper), ts(autoarima_bs_fc$upper))
autoarima_bs_fc$lower <- ts.union(ts(autoarima_bs_fc$lower), ts(autoarima_bs_fc$lower))
autoarima_bs_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))


# grid plot with legend SARIMA auto vs manual


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/arima_comparison_bs_ems.tex",width=6.5,height=4.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(autoarima_bs_fc, series = "SARIMA ", PI = TRUE)  +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(manualarima_bs_fc, series = "SARIMA ", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


#legend1 <- get_legend(p1)
#legend2 <- get_legend(p2)

ggarrange(p1, p2, ncol=2, nrow=1,label.x=0.2, labels=c("Auto SARIMA", "Manual SARIMA"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")

#dev.off()

#### grid plot with legend bats vs tbats


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/bats_comparison_ems.tex",width=6.5,height=4.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(bats_fc, series = "BATS", PI = TRUE)  +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(tbats_fc, series = "TBATS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


#legend1 <- get_legend(p1)
#legend2 <- get_legend(p2)

ggarrange(p1, p2, ncol=2, nrow=1,labels=c("BATS", "TBATS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")

#dev.off()

### Tbats vs Bats plot (bootstrapped PIs)

# preprocessing for plotting
bats_bs_fc$mean <- ts(bats_bs_fc$mean)
bats_bs_fc$upper <- ts.union(ts(bats_bs_fc$upper), ts(bats_bs_fc$upper))
bats_bs_fc$lower <- ts.union(ts(bats_bs_fc$lower), ts(bats_bs_fc$lower))
bats_bs_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

tbats_bs_fc$mean <- ts(tbats_bs_fc$mean)
tbats_bs_fc$upper <- ts.union(ts(tbats_bs_fc$upper), ts(tbats_bs_fc$upper))
tbats_bs_fc$lower <- ts.union(ts(tbats_bs_fc$lower), ts(tbats_bs_fc$lower))
tbats_bs_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/bats_comparison_bs_ems.tex",width=6.5,height=4.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(bats_bs_fc, series = "BATS", PI = TRUE)  +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(tbats_bs_fc, series = "TBATS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


#legend1 <- get_legend(p1)
#legend2 <- get_legend(p2)

ggarrange(p1, p2, ncol=2, label.x = 0.40, nrow=1,labels=c("BATS", "TBATS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")

#dev.off()


###  ACF/PACF of the automatic & manual sarima model residuals ### updated

# Automatic SARIMA residuals
auto_sarima_res <- ts(arima_fc$residuals)


acf_auto_res <- acf(auto_sarima_res, lag.max = 100)
pacf_auto_res <- pacf(auto_sarima_res, lag.max = 100)

# Manual SARIMA residuals
man_sarima_res <- ts(manual_arima1_fc$residuals)

acf_man_res <- acf(man_sarima_res, lag.max = 100)
pacf_man_res <- pacf(man_sarima_res, lag.max = 100)



#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/all_res_acf_pacf_ems.tex",width=6.5,height=6.5)

par(mfrow=c(2,2))
par(oma=c(0,0,1,0))

# Automatic & manual SARIMA residuals
plot(acf_auto_res, ylim = c(-0.5,0.5))
plot(pacf_auto_res, ylim =c(-0.5,0.5))

title("Automatic SARIMA residuals", line = -2, outer=T )

plot(acf_man_res, ylim = c(-0.5,0.5))
plot(pacf_man_res, ylim =c(-0.5,0.5))

title("Manual SARIMA residuals", line=-25, outer =T)

#dev.off()




## BATS model for different length of horizon
train <- window(ts_ori, start = c(1,1),end = c(356,24))
test <- window(ts_ori, start = c(357,1), end = c(360,24))

bats.fit <- bats(train, seasonal.periods  = c(24,168))

## h = 48
bats.fc <- forecast(bats.fit, h = 48)
accuracy(ts(bats.fc$mean), ts(test))
## h = 72
bats.fc <- forecast(bats.fit, h = 72)
accuracy(ts(bats.fc$mean), ts(test))
## h = 96
bats.fc <- forecast(bats.fit, h = 96)
accuracy(ts(bats.fc$mean), ts(test))


#### Forecast grid plots for the GRU and LSTM predictions ####

# get working directory
getwd()

# finds all CSV files on your google drive
drive_find(type = "csv", n_max = 30)  

# downloading the LSTM and GRU predictions from Gdrive to your working directory (only has to be run once)
drive_download("LSTM_ems_preds_trial.csv", overwrite = TRUE) # done
drive_download("LSTM_ems_preds_deep.csv", overwrite = TRUE) # done
drive_download("GRU_ems_preds.csv", overwrite = TRUE)
drive_download("GRU_ems_preds_deep.csv", overwrite = TRUE)

# downloading the PIs 
drive_download("LSTM_ems_pis.csv", overwrite = TRUE)
drive_download("GRU_ems_pis.csv", overwrite = TRUE)

# reading the predictions into R
lstm_preds <- read.csv("/Users/Manu/LSTM_ems_preds_trial.csv", header = TRUE)
lstm_preds_deep <- read.csv("/Users/Manu/LSTM_ems_preds_deep.csv", header = TRUE)
gru_preds <- read.csv("/Users/Manu/GRU_ems_preds.csv", header = TRUE)
gru_preds_deep <- read.csv("/Users/Manu/GRU_ems_preds_deep.csv", header = TRUE)
lstm_pis <- read.csv("/Users/Manu/LSTM_ems_pis.csv", header = TRUE)
gru_pis <- read.csv("/Users/Manu/GRU_ems_pis.csv", header = TRUE)

# test errors of the point forecasts

### grid used to get these values:

#batch_size = [168]
#epochs = [750, 1000]
#neurons = [1500, 2000]
#dropout = [0.0]
#learning_rate = [0.01, 0.001, 0.0001]
#optimizer = ['Adam', 'Adadelta']

accuracy(ts(lstm_preds), ts(test)) # RMSE 4.9303

accuracy(ts(lstm_preds_deep), ts(test)) # RMSE 5.3765 MAE 4.3909 MAPE 12.0963


accuracy(ts(gru_preds), ts(test)) # RMSE 5.5284

accuracy(ts(gru_preds_deep), ts(test)) # RMSE 4.9751


## Prediction interval accuracy (DLSTM)
lower <- lstm_pis$lower_bound_2hl
upper <- lstm_pis$upper_bound_2hl

picp <- mean(test >= lower & test <= upper)# PICP _ 1.0 
mpiw <- mean(upper - lower)# MPIW _ 31.3929

#### CWC (DLSTM)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_dlstm <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_dlstm # 0.7301

## Prediction interval accuracy (DGRU)

lower <- gru_pis$lower_bound_2hl
upper <- gru_pis$upper_bound_2hl

picp <- mean(test >= lower & test <= upper) # PICP _ 1.0 
mpiw <- mean(upper - lower) # MPIW 30.7371

# print picp and mpiw
picp  # PICP _ 1.0

mpiw # MPIW _ 30.7371

#### CWC (DGRU)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_dgru <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_dgru # 0.7148


# preprocessing the other forecast objects for plotting

lstm_fc <- forecast:: forecast(ets(train), h=24)

lstm_fc$mean <- ts(lstm_preds)
lstm_fc$upper <- ts.union(ts(lstm_pis$upper_bound_1hl), ts(lstm_pis$upper_bound_1h))
lstm_fc$lower <- ts.union(ts(lstm_pis$lower_bound_1h), ts(lstm_pis$lower_bound_1h))
lstm_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

lstm_deep_fc <- forecast:: forecast(ets(train), h=24)

lstm_deep_fc$mean <- ts(lstm_preds_deep)
lstm_deep_fc$upper <- ts.union(ts(lstm_pis$upper_bound_2hl), ts(lstm_pis$upper_bound_2hl))
lstm_deep_fc$lower <- ts.union(ts(lstm_pis$lower_bound_2hl), ts(lstm_pis$lower_bound_2hl))
lstm_deep_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

# hard coding the median of the baggedETS model into the fcobject for plotting

gru_fc <- forecast:: forecast(ets(train), h=24)

gru_fc$mean <- ts(gru_preds)
gru_fc$upper <- ts.union(ts(gru_pis$upper_bound_1hl), ts(gru_pis$upper_bound_1h))
gru_fc$lower <- ts.union(ts(gru_pis$lower_bound_1h), ts(gru_pis$lower_bound_1h))
gru_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

gru_deep_fc <- forecast:: forecast(ets(train), h=24)

gru_deep_fc$mean <- ts(gru_preds_deep)
gru_deep_fc$upper <- ts.union(ts(gru_pis$upper_bound_2hl), ts(gru_pis$upper_bound_2hl))
gru_deep_fc$lower <- ts.union(ts(gru_pis$lower_bound_2hl), ts(gru_pis$lower_bound_2hl))
gru_deep_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/plotgrid_networks_ems.tex",width=5.5,height=6.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches", ylim=c(0,80)) + 
  autolayer(lstm_fc, series = "LSTM", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches",  ylim=c(0,80)) + 
  autolayer(gru_fc, series = "GRU", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p3 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches",  ylim=c(0,80)) + 
  autolayer(lstm_deep_fc, series = "DLSTM", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p4 <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches",  ylim=c(0,80)) + 
  autolayer(gru_deep_fc, series = "DGRU", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2,label.x= 0.33, labels=c("LSTM", "GRU", "DLSTM", "DGRU"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")


#dev.off()




#### CWC (FFNN)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

picp = 1.0

mpiw = 33.4885

nmpiw <- mpiw/43

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_ffnn <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_ffnn # 0.7788


# plotting the baggedETS forecasts with the simulated PIs against the bootstrapped ETS forecasts

## plot of the ETS model (hard coding the 95% PI)
ets_bs_fc$mean <- ts(ets_bs_fc$mean)
ets_bs_fc$upper <- ts.union(ts(ets_bs_fc$upper), ts(ets_bs_fc$upper))
ets_bs_fc$lower <- ts.union(ts(ets_bs_fc$lower), ts(ets_bs_fc$lower))
ets_bs_fc$x <- ts(c(1:24), start = c(350,1), end = c(350,24))

# 1x2 plot ETS model vs Bagged ETS model

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/baggedets_ets.tex",width=6.5,height=4.5)

baggedets_pi <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(baggedets_fc, series = "BaggedETS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ets_pi <- autoplot(ts(test), xlab = "Time", ylab = "EMS Dispatches") + 
  autolayer(ets_bs_fc, series = "ETS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ggarrange(ets_pi, baggedets_pi, ncol=2, nrow=1,label.x= 0.20, labels=c("ETS", "BaggedETS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "none")

#dev.off()

