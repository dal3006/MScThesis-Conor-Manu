### R script - Main Results Traffic Forecasting ###

# clear workspace
rm(list=ls())

# load packages
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
#library(aTSA)
library(googledrive)
library(ggpubr)

# setting the seed for reproducibility
set.seed(123)

##### plotting the 1 year data size for the hourly traffic data (July 2018-June 2019)

# finds all CSV files on your google drive
drive_find(type = "csv", n_max = 30)  

# downloading a file from Gdrive to your working directory (only has to be run once)
drive_download("taxi_series_hourly.csv", overwrite = TRUE)

traffic_hr_1yr <- read.csv("/Users/Manu/taxi_series_hourly.csv", header = TRUE)

traffic_hr_1yr_ts <- ts(traffic_hr_1yr$X0, start = c(1,1), frequency = 24)

length(traffic_hr_1yr_ts)

tail(traffic_hr_1yr) # last observation: 2019-06-30 23:00

length(traffic_hr_1yr)

# remove the last 48 observations
traffic_hr_1yr <- traffic_hr_1yr[1:8712,]

head(traffic_hr_1yr) # first observation 2018-07-01 00:00:00

tail(traffic_hr_1yr) # last observation  2019-06-28 23:00:00

length(traffic_hr_1yr_ts) # 8760

# remove the last two days (48 observations aka 2 days)
traffic_hr_1yr_ts <- ts(traffic_hr_1yr$X0, start = c(1,1), end=c(363,24), frequency = 24)

length(traffic_hr_1yr_ts) # 8712

# Time Series:
# Start = c(1, 1)
# End = c(363, 24)
# Frequency = 24

# subsetting the last month of data (24hrs *30 = 720 observations)

traffic_lastmonth_ts <- ts(traffic_hr_1yr$X0, start = c(334,1), end=c(363,24), frequency = 24)

length(traffic_lastmonth_ts) # 720

## Create date index
time.index <- as.POSIXct(traffic_hr_1yr$tpep_pickup_datetime, formula = "%Y-%m-%d %H:%M:%S")

# creating an xts object for the entire 1 year of data
traffic_hr_1yr_xts <- xts(traffic_hr_1yr$X0, order.by = time.index)
names(traffic_hr_1yr_xts) <- "values" 

# xts object showing the last month incl test set

last_month <- traffic_hr_1yr[7992:8712,]

tail(last_month) # last observation 2019-06-28 23:00



### train test split ###

train <- window(traffic_hr_1yr_ts, end = c(356,24))
test <- window(traffic_hr_1yr_ts, start = c(357,1), end = c(357,24)) # test set of 24 days

length(train) # 8544
length(test) # 24

#### summary statistics for the entire training set and the entire test set

sumstats_train <- basicStats(ts(train))

test_complete <- window(traffic_hr_1yr_ts, start = c(357,1), end = c(363,24)) # test set incl. hold out weekdays

length(test_complete)

sumstats_test <- basicStats((ts(test_complete)))

# create a Dataframe with sumstats for training and test set
sum_stats <- cbind(sumstats_train, sumstats_test)

# rename columns
names(sum_stats) <- c("Training Set", "Test Set")

xtable(sum_stats)

## Create date index
time.index2 <- as.POSIXct(last_month$tpep_pickup_datetime, formula = "%Y-%m-%d %H:%M:%S")

# creating an xts object for the entire 1 year of data
last_month_xts <- xts(last_month$X0, order.by = time.index2)
names(last_month_xts) <- "values" 



#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/traffic_1yr_timeplot.tex",width=6.5,height=3.5)

par(mfrow=c(1,1))
plot(traffic_hr_1yr_xts, main = "Time Plot Entire Year", lwd=1) # xts plot whole data

#dev.off()

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/traffic_lastmonth_timeplot.tex",width=6.5,height=3.5)

par(mfrow=c(1,1))
plot(last_month_xts, main = "Time Plot Last Month", col = "blue", ylim=c(0,20000))   # plot of the last month 
addEventLines(events= xts(("Train   Test"),  as.Date("2019-06-23 00:00:00", format='%Y-%m-%d %H:%M:%S')), col="black", lty = "solid", srt=0, pos=1, offset=1.2, cex=1.2)

#dev.off()





## automated ETS model ##
ets_fc <- forecast:: forecast(ets(train), h=24)

summary(ets_fc) # A,Ad,A model


accuracy(ets_fc, test) # RMSE 3115.68 MAE 2930.14 MAPE 47.4696

### bootstrapped PIs for the ETS model

### using the bootrap option of the forecast.ets function to create prediction intervals

ets_bs_fc <- forecast(ets(train), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level=0.95)


### end bootstrapped PIs for the ETS model


## plot of the ETS model (hard coding the 95% PI)
ets_fc$mean <- ts(ets_fc$mean)
ets_fc$upper <- ts.union(ts(ets_fc$upper[,2]), ts(ets_fc$upper[,2]))
ets_fc$lower <- ts.union(ts(ets_fc$lower[,2]), ts(ets_fc$lower[,2]))
test <- ts(test)

# plotting the 1 ahead ahead forecasts

autoplot(test, xlab = "Time", ylab = "Traffic flows") + 
  autolayer(ets_fc, series = "ETS", PI = TRUE) +
  autolayer(test, series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

## Prediction interval accuracy
lower <- ets_fc$lower[,2]
upper <- ets_fc$upper[,2]

mean(test >= lower & test <= upper)# PICP _ 0.9167 
mean(upper - lower)# MPIW _ 15765.59  

## Prediction interval accuracy boostrapped PIs (ETS)

lower <- ets_bs_fc$lower
upper <- ets_bs_fc$upper

mean(test >= lower & test <= upper)# PICP _ 0.9167 
mean(upper - lower)# MPIW _ 15457.04

#### CWC (ETS)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

range(test)

picp <- 0.9167 

mpiw <- 15457.04

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_ets <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_ets # 8.6455


##### automated ARIMA model #### (to be updated)

auto_arima <- auto.arima(train) # arima (5,0,5) (2,1,0)_24 model

summary(auto_arima)

arima_fc <- forecast:: forecast(auto.arima(train), h=24)

accuracy(arima_fc, test) # RMSE 2807.86 MAE 2127.95 MAPE 35.5932


### bootstrapped PIs for the auto.arima model


autoarima_bs_fc <- forecast(auto.arima(train), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level=95)


## Prediction interval accuracy boostrapped PIs (autoarima)

lower <- autoarima_bs_fc$lower
upper <- autoarima_bs_fc$upper

mean(test >= lower & test <= upper)# PICP _ 0.9167
mean(upper - lower)# MPIW _ 9717.26

#### CWC (autoarima)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

range(test)

picp <- 0.9167 

mpiw <- 9717.26

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_autoarima <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_autoarima # 5.4351



### Tests for autocorrelation - Auto.Arima residuals ###

## test for autocorrelation -> H_0: no serial correlation vs. H_1: serial correlation
# According to Hyndman the number of lags for a seasonal time series can
# be chosen by the rule of thumb h = min(2m, T/5); here: 2m = 48, T/5 = 504/5 = 100.8

length(arima_fc$residuals) # 8544



Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 24)
# pvalue is < 0.0000

Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 48)
# pvalue is < 0.0000

Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 72)
# p-value is < 0.0000

Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 1)
# p-value = 0.8781
Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 2)
#p-value = 0.9133
Box.test(arima_fc$residuals,type = "Ljung-Box", lag = 3)
#p-value = 0.8915

# autocorrelation at seasonal lags

### end autocorrelation tests ###

# normality tests

jarque.bera.test(arima_fc$residuals)

skewness(arima_fc$residuals)
kurtosis(arima_fc$residuals)

# ARCH tests (object: estimated arima model)

#autoarima
arch.test(object = arima(train,order=c(5,0,5), seasonal= c(2,1,0)), output=TRUE) 

#manualarima
arch.test(object = arima(train,order=c(5,0,5), seasonal= c(4,1,1)), output=TRUE) 


# stationarity tests (ADF TEST)

adf_train <- adf.test(x=train, nlag=97)


# as of lag 19-20 it gets nonstationary

adf_sdiff <- adf.test(x=diff(train,24), nlag = 97)

# seasonally differenced data is stationary



#### Manual ARIMA modeling procedure #### 

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
acf(ts(arima_fc$residuals), lag.max=80, ylim = c(-1,1)) # spikes at slags 1,2,3
pacf(ts(arima_fc$residuals), lag.max=80, ylim = c(-1,1)) # spikes at slags 1,2,3

# -> add sMA terms and sAR terms (1st manual model)

# manual model 1 # 1 more sMA term + 1 more sAR term
manual_arima1 <- arima(train, order= c(5,0,5), seasonal = list(order=c(3,1,1)) ) # arima (5,0,4) (2,1,0)_24 model

manual_arima1_fc <- forecast:: forecast(arima(train, order= c(5,0,5), seasonal = list(order=c(3,1,1))), h=24)

accuracy(manual_arima1_fc, test) # RMSE 2044.14, MAE 1630.27, MAPE 26.6855 



# check residuals of manual model 1 
acf(ts(manual_arima1_fc$residuals), lag.max=80, ylim = c(-1,1))
pacf(ts(manual_arima1_fc$residuals), lag.max=80, ylim = c(-1,1))

### autocorrelation tests manual arima model 1

Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 24)
Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 48)
Box.test(manual_arima1_fc$residuals,type = "Ljung-Box", lag = 72)

## all p-values < 0.0000 ->  still autocorrelation

# manual SARIMA model 2 # 2 more sAR terms and 1 more sMA term

# model fit
manual_arima2_fit <- arima(train, order= c(5,0,5), seasonal = list(order=c(4,1,1)))


### extracting the IC values
# k contains the BIC penalty
bic <- AIC(manual_arima2_fit, k = log(length(manual_arima2_fit))) # 139,396.40

# custom AICc function
aicc = function(model){
  n = model$nobs
  p = length(model$coef)
  aicc = model$aic + 2*p*(p+1)/(n-p-1)
  return(aicc)
}

# compute the AICc
aicc(manual_arima2_fit) # 139,386.3

# compute the BIC
BIC(manual_arima2_fit)

# inference for estimated coefficients (x= object, vcov= covariance matrix of the est. coefficients)
coeftests <- coeftest(x=manual_arima2_fit)

# values
coeftests <- coeftests[,]

# coefficients
xtable(coeftests)


# forecast using the manual SARIMA model 2
manual_arima2_fc <- forecast:: forecast(arima(train, order= c(5,0,5), seasonal = list(order=c(4,1,1))), h=24)

# test errors
accuracy(manual_arima2_fc, test) # RMSE 1702.52, MAE 1310.50, MAPE 22.4235


### using the bootrap option of the forecast.ets function to create prediction intervals

man_arima_bs_fc <- forecast(arima(train, order= c(5,0,5), seasonal = list(order=c(4,1,1))), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level=95)

# PI accuracy manual arima model 2 (boostrapped PIs)
lower <- man_arima_bs_fc$lower
upper <- man_arima_bs_fc$upper

mean(ts(test) >= lower & ts(test) <= upper)# PICP _ 0.9167

mean(upper - lower) # 7432.07

#### CWC (manual arima)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

range(test)

picp <- 0.9167

mpiw <- 7432.07

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_manarima <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_manarima # 4.1569



# check residuals of manual model 2 
acf(ts(manual_arima2_fc$residuals), lag.max=80, ylim = c(-1,1))
pacf(ts(manual_arima2_fc$residuals), lag.max=80, ylim = c(-1,1))

### autocorrelation tests manual arima model 2

Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 24)
Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 48)
Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 72)

## all p-values < 0.0000 -> still autocorrelation


Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 1)
Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 2)
Box.test(manual_arima2_fc$residuals,type = "Ljung-Box", lag = 3)

# normality test

jarque.bera.test(manual_arima2_fc$residuals)

skewness(manual_arima2_fc$residuals)
kurtosis(manual_arima2_fc$residuals)




#### ACF plot of weekly level autocorrelation manual SARIMA vs BATS


acf_sarima <- acf(ts(manual_arima2_fc$residuals), lag.max=200)

acf_bats <- acf(ts(bats_fc$residuals), lag.max = 200)

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/weekly_comp_acf.tex",width=6.5,height=6.5)


par(mfrow=c(2,1))
plot(acf_sarima, ylim = c(-0.4,0.4), main = "ACF(SARIMA)")

plot(acf_bats, ylim =c(-0.4,0.4), main = "ACF(BATS)")

#dev.off()



#### TBATS model ####

# seasonal periods: 24 hours (day), 168=24*7 (week)
# tbats function comes with parallel processing option
tbats_fc <- forecast :: forecast(tbats(train, seasonal.periods= c(24,168), use.parallel = TRUE, num.cores = NULL), h=24)

summary(tbats_fc) # TBATS(1, {5,1}, -, {<24,8>,<168,5>})

accuracy(tbats_fc, test) # RMSE 2125.12 MAE 1954.20, MAPE 27.2012 

# decomposition by TBATS model (traffic)
tbats_decomp <- tbats(train, seasonal.periods= c(24,168), use.parallel = TRUE, num.cores = NULL)

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/tbatsdecomposition_traffic.tex",width=6.5,height=5.5)

plot(tbats_decomp)

#dev.off()



# built in bootstrap function for PIs

tbats_bs_fc <- forecast(tbats(train, seasonal.periods = c(24,168)), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level = 95)

## Prediction interval accuracy (bootstrapped PIs TBATS)
lower <- tbats_bs_fc$lower
upper <- tbats_bs_fc$upper

mean(test >= lower & test <= upper)# PICP _ 1.0
mean(upper - lower)# MPIW _ 7981.65

#### CWC (TBATS bootstrapped)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

range(test)

picp <- 1.0

mpiw <- 7981.65

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_tbatsbs <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_tbatsbs # 0.7102



#### BATS model ####



# seasonal periods: 24 hours (day), 168=24*7 (week)
# bats model fitting + forecasting
bats_fc <- forecast :: forecast(bats(train, seasonal.periods= c(24,168), use.parallel = TRUE, num.cores = NULL), h=24)

summary(bats_fc) # BATS(0.035, {4,1}, 0.934, {24,168})

# test errors
accuracy(bats_fc, test) # RMSE 851.98 MAE 729.58 MAPE 7.2395


# decomposition by BATS model
bats_decomp <- bats(train, seasonal.periods= c(24,168), use.parallel = TRUE, num.cores = NULL)


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/batsdecomposition_traffic.tex",width=6.5,height=5.5)

plot(bats_model)

#dev.off()


# built in bootstrap function for PIs

bats_bs_fc <- forecast(bats(train, seasonal.periods = c(24,168)), bootstrap=TRUE, h=24, PI=TRUE, npaths=100, level = 95)

bats_bs_fc$upper <- ts.union(ts(bats_bs_fc$upper), ts(bats_bs_fc$upper))
bats_bs_fc$lower <- ts.union(ts(bats_bs_fc$lower), ts(bats_bs_fc$lower))
bats_bs_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

# PI accuracy bootstrapped PIs (BATS model)
lower <- bats_bs_fc$lower
upper <- bats_bs_fc$upper

picp <- mean(ts(test) >= lower & ts(test) <= upper)# PICP _ 1.0

mpiw <- mean(upper - lower) # MPIW 5370.03


#### CWC (BATS bootstrapped)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

range(test)

picp <- 1.0

mpiw <- 5370.03

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_bats <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_bats # 0.4778

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
accuracy(baggedets_fc$mean, test) 


# test errors of the baggedETS model (median)
accuracy(baggedets_fc$median, test) 

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

## Prediction interval accuracy
lower <- simfc_ets$lower
upper <- simfc_ets$upper

picp <- mean(test >= lower & test <= upper)# PICP _ 0.875
mpiw <- mean(upper - lower)# MPIW _ 32769.51

#### CWC (baggedETS)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

picp = 0.875

mpiw = 32769.51

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_baggedets <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_baggedets # 126.9055

#### to fix: bootstrapped series plot ####

# illustration plot with 10 bootstrapped series # 

## create bootstrapped series of weekly deseasonalized data
bootseries <- ts(as.data.frame(bld.mbb.bootstrap(train, num=10)), start = c(1,1), frequency = 24)

## plot bootstrapped series
autoplot(ts(train), xlab = "Time") +
  autolayer(bootseries, colour=TRUE) +
  autolayer(ts(train), colour=FALSE) +
  ylab("Bootstrapped Series") + guides(colour="none")



## plot bootstrapped series

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/bootstrapped_series.tex",width=6.5,height=4.5)

autoplot(ts(train), xlab = "Time") +
  autolayer(ts(bootseries), colour=TRUE) +
  autolayer(ts(train), colour=FALSE) +
  ylab("Bootstrapped Series") + guides(colour="none") +
  theme_bw()

#dev.off()

#### end to fix plot with bootstrapped series ####

# inserting the simulated PIs into the FC object
# the upper and lower objects in the baggedETS model are simply min and max values
# of the base models

baggedets_fc$mean <- ts(baggedets_fc$median)
baggedets_fc$upper <- ts.union(ts(simfc_ets$upper), ts(simfc_ets$upper))
baggedets_fc$lower <- ts.union(ts(simfc_ets$lower), ts(simfc_ets$lower))
baggedets_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))


# plotting the baggedETS forecasts with the simulated PIs

tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/baggedets_pis.tex",width=6.5,height=4.5)

baggedets <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(0, 20000)) + 
  autolayer(baggedets_fc, series = "BaggedETS", PI = FALSE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

baggedetspi <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows") + 
  autolayer(baggedets_fc, series = "BaggedETS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ggarrange(baggedets, baggedetspi, ncol=2, nrow=1,label.x= 0.15, labels=c("Point forecasts", "Prediction intervals"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "none")

dev.off()


## SNAIVE model

snaive_fc <- forecast(snaive(train), h=24)


# PI accuracy
lower <- snaive_fc$lower[,2]
upper <- snaive_fc$upper[,2]

picp <- mean(test >= lower & test <= upper)# PICP _ 0.9167

mpiw <- mean(upper - lower) # MPIW 10345

#### CWC (snaive)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

picp = 0.9167

mpiw = 10345

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_snaive <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_snaive # 5.7862


# accuracy
accuracy(snaive_fc, test) # RMSE 1355.50 MAE 1040.67 MAPE 11.9214

autoplot(test, xlab = "Time", ylab = "Traffic flows") + 
  autolayer(snaive_fc, series = "SNAIVE", PI = TRUE) +
  autolayer(test, series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


##### Group plot of all traditional and hybrid models ####

# download LGBM predictions and PI bounds

drive_download("LGBM_preds.csv", overwrite = TRUE)

lgbm_preds <- read.csv("/Users/Manu/LGBM_preds.csv", header = TRUE)

lgbm_preds


# creating a forecast object with the lstm data

lgbm_fc <- forecast(ets(train), h=24)

# replace PI bounds and point forecasts
lgbm_fc$mean <- ts(lgbm_preds$LGBM.predictions)

# hard code the 95% PIs
lgbm_fc$upper <- ts.union(ts(lgbm_fc$upper[,2]), ts(lgbm_fc$upper[,2]))
lgbm_fc$lower <- ts.union(ts(lgbm_fc$lower[,2]), ts(lgbm_fc$lower[,2]))

lgbm_fc$upper <- ts.union(ts(lgbm_preds$PI.upper.bound), ts(lgbm_preds$PI.upper.bound))
lgbm_fc$lower <- ts.union(ts(lgbm_preds$PI.lower.bound), ts(lgbm_preds$PI.lower.bound))

lgbm_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

# PI accuracy
lower <- lgbm_fc$lower[,2]
upper <- lgbm_fc$upper[,2]

picp <- mean(ts(test) >= lower & ts(test) <= upper)# PICP _ 1.0

mpiw <- mean(upper - lower) # MPIW 5128.74

#### CWC (LGBM)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

picp <- 1.0

mpiw <- 5128.74

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_lgbm <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_lgbm # 0.4564



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
tbats_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

manual_arima2_fc$mean <- ts(manual_arima2_fc$mean)
manual_arima2_fc$upper <- ts.union(ts(manual_arima2_fc$upper[,2]), ts(manual_arima2_fc$upper[,2]))
manual_arima2_fc$lower <- ts.union(ts(manual_arima2_fc$lower[,2]), ts(manual_arima2_fc$lower[,2]))
manual_arima2_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

# hard coding the median of the baggedETS model into the fcobject for plotting
baggedets_fc$mean <- ts(baggedets_fc$median)
baggedets_fc$upper <- ts.union(ts(simfc_ets$upper), ts(simfc_ets$upper))
baggedets_fc$lower <- ts.union(ts(simfc_ets$lower), ts(simfc_ets$lower))
baggedets_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

bats_fc$mean <- ts(bats_fc$mean)
bats_fc$upper <- ts.union(ts(bats_fc$upper[,2]), ts(bats_fc$upper[,2]))
bats_fc$lower <- ts.union(ts(bats_fc$lower[,2]), ts(bats_fc$lower[,2]))
bats_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/plotgrid_traditional.tex",width=5.5,height=6.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(bats_fc, series = "BATS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(manual_arima2_fc, series = "SARIMA", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p3 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(lgbm_fc, series = "LGBM", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p4 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,44000)) + 
  autolayer(baggedets_fc, series = "BaggedETS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2,label.x= 0.33, labels=c("BATS", "SARIMA", "LGBM", "BaggedETS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "none")


#dev.off()



#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/plotgrid_traditional_bs.tex",width=5.5,height=6.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(bats_bs_fc, series = "BATS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(man_arima_bs_fc, series = "SARIMA", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p3 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(lgbm_fc, series = "LGBM", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p4 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(baggedets_fc, series = "BaggedETS", PI = FALSE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2,label.x= 0.33, labels=c("BATS", "SARIMA", "LGBM", "BaggedETS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")


#dev.off()

### end plot grid traditional models with bootstrapped PIs ###



# preprocessing the fc models for plotting


manual_arima2_fc$mean <- ts(manual_arima2_fc$mean)
manual_arima2_fc$upper <- ts.union(ts(manual_arima2_fc$upper[,2]), ts(manual_arima2_fc$upper[,2]))
manual_arima2_fc$lower <- ts.union(ts(manual_arima2_fc$lower[,2]), ts(manual_arima2_fc$lower[,2]))
manual_arima2_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

arima_fc$mean <- ts(arima_fc$mean)
arima_fc$upper <- ts.union(ts(arima_fc$upper[,2]), ts(arima_fc$upper[,2]))
arima_fc$lower <- ts.union(ts(arima_fc$lower[,2]), ts(arima_fc$lower[,2]))
arima_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

# preprocessing the bootrapped fc models for plotting

man_arima_bs_fc$mean <- ts(man_arima_bs_fc$mean)
man_arima_bs_fc$upper <- ts.union(ts(man_arima_bs_fc$upper), ts(man_arima_bs_fc$upper))
man_arima_bs_fc$lower <- ts.union(ts(man_arima_bs_fc$lower), ts(man_arima_bs_fc$lower))
man_arima_bs_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

autoarima_bs_fc$mean <- ts(autoarima_bs_fc$mean)
autoarima_bs_fc$upper <- ts.union(ts(autoarima_bs_fc$upper), ts(autoarima_bs_fc$upper))
autoarima_bs_fc$lower <- ts.union(ts(autoarima_bs_fc$lower), ts(autoarima_bs_fc$lower))
autoarima_bs_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

# grid plot with legend SARIMA auto vs manual 


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/arima_comparison.tex",width=6.5,height=4.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-5000,22000)) + 
  autolayer(arima_fc, series = "Auto SARIMA", PI = TRUE)  +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-5000,22000)) + 
  autolayer(manual_arima2_fc, series = "Manual SARIMA", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


#legend1 <- get_legend(p1)
#legend2 <- get_legend(p2)

ggarrange(p1, p2, ncol=2, nrow=1,labels=c("Auto SARIMA", "Manual SARIMA"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")

#dev.off()


# grid plot with legend SARIMA auto vs manual (BOOTSTRAPPED PIs)


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/arima_comparison_bs.tex",width=6.5,height=4.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-6000,22000)) + 
  autolayer(autoarima_bs_fc, series = "Auto SARIMA", PI = TRUE)  +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-6000,22000)) + 
  autolayer(man_arima_bs_fc, series = "Manual SARIMA", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()



ggarrange(p1, p2, ncol=2, nrow=1, label.x=0.2, labels=c("Auto SARIMA", "Manual SARIMA"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")

#dev.off()

#### grid plot with legend bats vs tbats


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/bats_comparison.tex",width=6.5,height=4.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-5000,22000)) + 
  autolayer(bats_fc, series = "BATS", PI = TRUE)  +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-5000,22000)) + 
  autolayer(tbats_fc, series = "TBATS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


#legend1 <- get_legend(p1)
#legend2 <- get_legend(p2)

ggarrange(p1, p2, ncol=2, nrow=1,labels=c("BATS", "TBATS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")

#dev.off()

#### grid plot with legend bats vs tbats (Boostrapped PIs) #####

# preprocessing for plotting
bats_bs_fc$mean <- ts(bats_bs_fc$mean)
bats_bs_fc$upper <- ts.union(ts(bats_bs_fc$upper[,2]), ts(bats_bs_fc$upper[,2]))
bats_bs_fc$lower <- ts.union(ts(bats_bs_fc$lower[,2]), ts(bats_bs_fc$lower[,2]))
bats_bs_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

tbats_bs_fc$mean <- ts(tbats_bs_fc$mean)
tbats_bs_fc$upper <- ts.union(ts(tbats_bs_fc$upper), ts(tbats_bs_fc$upper))
tbats_bs_fc$lower <- ts.union(ts(tbats_bs_fc$lower), ts(tbats_bs_fc$lower))
tbats_bs_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

#plot

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/bats_comparison_bs.tex",width=6.5,height=4.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-5000,22000)) + 
  autolayer(bats_bs_fc, series = "BATS", PI = TRUE)  +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-5000,22000)) + 
  autolayer(tbats_bs_fc, series = "TBATS", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()


#legend1 <- get_legend(p1)
#legend2 <- get_legend(p2)

ggarrange(p1, p2, ncol=2, label.x = 0.40, nrow=1,labels=c("BATS", "TBATS"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "hv")

#dev.off()

###  ACF/PACF of the automatic & manual sarima model residuals ###

# Automatic SARIMA residuals
auto_sarima_res <- ts(arima_fc$residuals)


acf_auto_res <- acf(auto_sarima_res, lag.max = 100)
pacf_auto_res <- pacf(auto_sarima_res, lag.max = 100)

# Manual SARIMA residuals
man_sarima_res <- ts(manual_arima2_fc$residuals)

acf_man_res <- acf(man_sarima_res, lag.max = 100)
pacf_man_res <- pacf(man_sarima_res, lag.max = 100)



#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/all_res_acf_pacf.tex",width=6.5,height=6.5)

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
drive_download("LSTM_preds.csv", overwrite = TRUE) # done
drive_download("LSTM_preds_deep.csv", overwrite = TRUE) # done
drive_download("GRU_preds.csv", overwrite = TRUE)
drive_download("GRU_preds_deep.csv", overwrite = TRUE)

# downloading the PIs 
drive_download("LSTM_pis.csv", overwrite = TRUE)
drive_download("GRU_pis.csv", overwrite = TRUE)

# reading the predictions into R
lstm_preds <- read.csv("/Users/Manu/LSTM_preds.csv", header = TRUE)
lstm_preds_deep <- read.csv("/Users/Manu/LSTM_preds_deep.csv", header = TRUE)
gru_preds <- read.csv("/Users/Manu/GRU_preds.csv", header = TRUE)
gru_preds_deep <- read.csv("/Users/Manu/GRU_preds_deep.csv", header = TRUE)
lstm_pis <- read.csv("/Users/Manu/LSTM_pis.csv", header = TRUE)
gru_pis <- read.csv("/Users/Manu/GRU_pis.csv", header = TRUE)

# test errors of the point forecasts

accuracy(ts(lstm_preds), ts(test)) # RMSE 671.49

accuracy(ts(lstm_preds_deep), ts(test)) # RMSE 528.05


accuracy(ts(gru_preds), ts(test)) # RMSE 605.23

accuracy(ts(gru_preds_deep), ts(test)) # RMSE 565.67


## Prediction interval accuracy (DLSTM)
lower <- lstm_pis$lower_bound_2hl
upper <- lstm_pis$upper_bound_2hl

picp <- mean(test >= lower & test <= upper)# PICP _ 1.0 
mpiw <- mean(upper - lower)# MPIW _ 3733.15

#### CWC (DLSTM)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

picp <- 1.0

mpiw <- 3733.15

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_dlstm <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_dlstm # 0.3322

## Prediction interval accuracy (DGRU)

lower <- gru_pis$lower_bound_2hl
upper <- gru_pis$upper_bound_2hl

picp <- mean(test >= lower & test <= upper) # PICP _ 1.0 
mpiw <- mean(upper - lower) 

# print picp and mpiw
picp  # PICP _ 1.0

mpiw # MPIW _ 5133.53

#### CWC (DGRU)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

# range of the target variable = 11238

picp <- 1.0

mpiw <- 5133.53


nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_dgru <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_dgru # 0.4568


# preprocessing the other forecast objects for plotting

lstm_fc <- forecast(ets(train), h=24)

lstm_fc$mean <- ts(lstm_preds)
lstm_fc$upper <- ts.union(ts(lstm_pis$upper_bound_1hl), ts(lstm_pis$upper_bound_1h))
lstm_fc$lower <- ts.union(ts(lstm_pis$lower_bound_1h), ts(lstm_pis$lower_bound_1h))
lstm_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

lstm_deep_fc <- forecast(ets(train), h=24)

lstm_deep_fc$mean <- ts(lstm_preds_deep)
lstm_deep_fc$upper <- ts.union(ts(lstm_pis$upper_bound_2hl), ts(lstm_pis$upper_bound_2hl))
lstm_deep_fc$lower <- ts.union(ts(lstm_pis$lower_bound_2hl), ts(lstm_pis$lower_bound_2hl))
lstm_deep_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

# hard coding the median of the baggedETS model into the fcobject for plotting

gru_fc <- forecast(ets(train), h=24)

gru_fc$mean <- ts(gru_preds)
gru_fc$upper <- ts.union(ts(gru_pis$upper_bound_1hl), ts(gru_pis$upper_bound_1h))
gru_fc$lower <- ts.union(ts(gru_pis$lower_bound_1h), ts(gru_pis$lower_bound_1h))
gru_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

gru_deep_fc <- forecast(ets(train), h=24)

gru_deep_fc$mean <- ts(gru_preds_deep)
gru_deep_fc$upper <- ts.union(ts(gru_pis$upper_bound_2hl), ts(gru_pis$upper_bound_2hl))
gru_deep_fc$lower <- ts.union(ts(gru_pis$lower_bound_2hl), ts(gru_pis$lower_bound_2hl))
gru_deep_fc$x <- ts(c(1:24), start=c(357,1), end=c(357,24))

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/plotgrid_networks.tex",width=5.5,height=6.5)


p1 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(lstm_fc, series = "LSTM", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p2 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(gru_fc, series = "GRU", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p3 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(lstm_deep_fc, series = "DLSTM", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

p4 <- autoplot(ts(test), xlab = "Time", ylab = "Traffic flows", ylim=c(-3000,20000)) + 
  autolayer(gru_deep_fc, series = "DGRU", PI = TRUE) +
  autolayer(ts(test), series = "Test") + 
  guides(colour = guide_legend(title = "")) +
  theme_bw()

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2,label.x= 0.33, labels=c("LSTM", "GRU", "DLSTM", "DGRU"), font.label = list(size = 11, color = "black", fontface="bold"), common.legend=FALSE, legend="bottom", align = "none")


#dev.off()




#### CWC (FFNN)
# formula: CWC = NMPIW(1 + \gamma(PICP) *exp(-\xi(PICP-\alpha))
# where alpha = 0.95 and xi = 50

picp = 0.9583

mpiw = 7449.29

range(test) # c(1790, 13028)

# range of the target variable:
# 13028-1790 = 11238

nmpiw <- mpiw/11238

# creating the binary variable based on the PICP using an ifelse function 
# with the syntax: ifelse(test, yes, no)
gamma_picp <- ifelse(picp >= 0.95, 0, 1)

gamma_picp

cwc_ffnn <- nmpiw*(1+gamma_picp * exp(-50*(picp-0.95)))

cwc_ffnn # 0.6629


