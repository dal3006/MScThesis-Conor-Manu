### Robustness checks - EMS data ###


# loading packages

library(ggplot2)
library(forecast)
library(tikzDevice)
library(ggpubr)
library(tikzDevice)
library(ggpubr)
library(gridExtra)

# leading the data into R
#df_ori <- read.csv("/Users/Conorcavanaugh/Dropbox/CBS MSc Thesis Research Folder/DATA & Code/Model Specific Notebooks/emergency_dispatches_bronx_hourly.csv", header = TRUE)
df_ori <- read.csv("/Users/Manu/Dropbox/CBS MSc Thesis Research Folder/DATA & Code/Model Specific Notebooks/emergency_dispatches_bronx_hourly.csv", header = TRUE)


ts_ori <- ts(df_ori$X0, start = c(1,1), frequency = 24) 

## different lenghts of train and test are used
## train ending on 349,24 is the one used for regular results and is sunday the 15
## test ending on 350,24 is the one used for regular results and is monday the 16
## test set goes from monday the 16 to sunday the 22
train <- window(ts_ori, end = c(349,24))
test <- window(ts_ori, start = c(350,1), end = c(350,24)) # test set of 24 days

#-----------------------------
## ROBUSTNESS CHECK OF DAYS ##
#-----------------------------
train <- window(ts_ori, end = c(349,24))
test <- window(ts_ori, start = c(350,1), end = c(350,24)) # test set of 24 days


## MONDAY 16
bats.fit <- bats(train, seasonal.periods  = c(24,168))
bats.fc <- forecast(bats.fit, h = 24)
accuracy(bats.fc, test) ## 5.1989

tbats.fit <- tbats(train, seasonal.periods = c(24,168))
tbats.fc <- forecast(tbats.fit, h = 24)
accuracy(tbats.fc, test) ## 5.123621

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 11.018923

## TUESDAY 17
train <- window(ts_ori, end = c(350,24))
test <- window(ts_ori, start = c(351,1), end = c(351,24)) # test set of 24 days

bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)
accuracy(bats.fc, test) ## 6.349298

tbats.refit <- tbats(train, model = tbats.fit)
tbats.fc <- forecast(tbats.refit, h = 24)
accuracy(tbats.fc, test) ## 6.10596

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 7.026735

## WEDNESDAY 18
train <- window(ts_ori, end = c(351,24))
test <- window(ts_ori, start = c(352,1), end = c(352,24)) # test set of 24 days

bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)
accuracy(bats.fc, test) ## 5.037459

tbats.refit <- tbats(train, model = tbats.fit)
tbats.fc <- forecast(tbats.refit, h = 24)
accuracy(tbats.fc, test) ## 5.504294

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 7.807582

## THURSDAY 19
train <- window(ts_ori, end = c(352,24))
test <- window(ts_ori, start = c(353,1), end = c(353,24)) # test set of 24 days

bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)
accuracy(bats.fc, test) ## 4.783501

tbats.refit <- tbats(train, model = tbats.fit)
tbats.fc <- forecast(tbats.refit, h = 24)
accuracy(tbats.fc, test) ## 4.821010

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 5.930149

## FRIDAY 20
train <- window(ts_ori, end = c(353,24))
test <- window(ts_ori, start = c(354,1), end = c(354,24)) # test set of 24 days

bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)
accuracy(bats.fc, test) ## 4.915709

tbats.refit <- tbats(train, model = tbats.fit)
tbats.fc <- forecast(tbats.refit, h = 24)
accuracy(tbats.fc, test) ## 5.366160

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 8.005207

## SATURDAY 21
train <- window(ts_ori, end = c(354,24))
test <- window(ts_ori, start = c(355,1), end = c(355,24)) # test set of 24 days

bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)
accuracy(bats.fc, test) ## 5.100017

tbats.refit <- tbats(train, model = tbats.fit)
tbats.fc <- forecast(tbats.refit, h = 24)
accuracy(tbats.fc, test) ## 5.491460 

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 9.888967

## SUNDAY 22
train <- window(ts_ori, end = c(355,24))
test <- window(ts_ori, start = c(356,1), end = c(356,24)) # test set of 24 days

bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)
accuracy(bats.fc, test) ## 3.450904

tbats.refit <- tbats(train, model = tbats.fit)
tbats.fc <- forecast(tbats.refit, h = 24)
accuracy(tbats.fc, test) ## 5.730848

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 6.254998

#-----------------------------
## ROBUSTNESS CHECK OF HORIZON
#-----------------------------
train <- window(ts_ori, end = c(349,24))

## h = 24
test <- window(ts_ori, start = c(350,1), end = c(350,24)) # test set of 24 days

bats.fit <- bats(train, seasonal.periods  = c(24,168))
bats.fc <- forecast(bats.fit, h = 24)
accuracy(bats.fc, test) ## 5.1989

tbats.fit <- tbats(train, seasonal.periods = c(24,168))
tbats.fc <- forecast(tbats.fit, h = 24)
accuracy(tbats.fc, test) ## 5.123621

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 11.018923

## h = 48
test <- window(ts_ori, start = c(350,1), end = c(351,24)) # test set of 48 days

bats.fc <- forecast(bats.fit, h = 48)
accuracy(bats.fc, test) ## 5.828852

tbats.fc <- forecast(tbats.fit, h = 48)
accuracy(tbats.fc, test) ## 5.732191

snaive.fc <- snaive(train, h = 48)
accuracy(snaive.fc, test) ## 10.757749

## h = 72
test <- window(ts_ori, start = c(350,1), end = c(352,24)) # test set of 72 days

bats.fc <- forecast(bats.fit, h = 72)
accuracy(bats.fc, test) ## 5.492270

tbats.fc <- forecast(tbats.fit, h = 72)
accuracy(tbats.fc, test) ## 5.616982

snaive.fc <- snaive(train, h = 72)
accuracy(snaive.fc, test) ## 11.054788

## h = 96
test <- window(ts_ori, start = c(350,1), end = c(353,24)) # test set of 96 days

bats.fc <- forecast(bats.fit, h = 96)
accuracy(bats.fc, test) ## 5.316675

tbats.fc <- forecast(tbats.fit, h = 96)
accuracy(tbats.fc, test) ## 5.431594

snaive.fc <- snaive(train, h = 96)
accuracy(snaive.fc, test) ## 10.840222


#--------------------------------
## ROBUSTNESS CHECK SIZE OF TRAIN
#--------------------------------
test <- window(ts_ori, start = c(350,1), end = c(350,24)) # test set of 24 days

## 1 year
train <- window(ts_ori, end = c(349,24)) ## use all data

bats.fit <- bats(train, seasonal.periods  = c(24,168))
bats.fc <- forecast(bats.fit, h = 24)
accuracy(bats.fc, test) ## 5.1989

tbats.fit <- tbats(train, seasonal.periods = c(24,168))
tbats.fc <- forecast(tbats.fit, h = 24)
accuracy(tbats.fc, test) ## 5.123621

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 11.018923

## 6 months
train <- window(ts_ori, start = c(166,1), end = c(349,24)) ## start at june 15th

bats.fit <- bats(train, seasonal.periods  = c(24,168))
bats.fc <- forecast(bats.fit, h = 24)
accuracy(bats.fc, test) ## 4.853350

tbats.fit <- tbats(train, seasonal.periods = c(24,168))
tbats.fc <- forecast(tbats.fit, h = 24)
accuracy(tbats.fc, test) ## 5.335147

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 11.018923

## 1 month
train <- window(ts_ori, start = c(319,1), end = c(349,24)) ## start at november 15th

bats.fit <- bats(train, seasonal.periods  = c(24,168))
bats.fc <- forecast(bats.fit, h = 24)
accuracy(bats.fc, test) ## 5.076015

tbats.fit <- tbats(train, seasonal.periods = c(24,168))
tbats.fc <- forecast(tbats.fit, h = 24)
accuracy(tbats.fc, test) ## 5.375196

snaive.fc <- snaive(train, h = 24)
accuracy(snaive.fc, test) ## 11.018923

#-----------------------------
## PLOTTING ROBUSTNESS RESULTS
#-----------------------------

## PLOTTING RMSE VS LENGTH OF HORIZION
x.axis <- c(1,2,3,4)

rmse.snaive <- c(11.0189, 10.7577, 11.0548, 10.8402) # updated
rmse.TBATS <- c(5.1236, 5.73, 5.61, 5.43) # updated
rmse.DLSTM <- c(4.93, 5.57, 5.73, 5.65) # updated
rmse.LGBM <- c(6.47, 6.17, 6.07, 5.98) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.TBATS, rmse.DLSTM, rmse.LGBM)


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/rmse_fchorizon.tex",width=6.0,height=5.0)

ggplot(df,) + 
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("24", "48", "72", "96"), limits = c(1,4)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.TBATS, color = "TBATS")) +
  geom_point(aes(x = x.axis, y = rmse.TBATS)) +
  geom_line(aes(x = x.axis, y = rmse.DLSTM, color = "DLSTM")) +
  geom_point(aes(x = x.axis, y = rmse.DLSTM)) +
  geom_line(aes(x = x.axis, y = rmse.LGBM, color = "LGBM")) +
  geom_point(aes(x = x.axis, y = rmse.LGBM)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5, fill=NA)) +
  labs(title = "RMSE vs forecast horizon", y = "RMSE\n", x = "\nForecast horizon(h)") +
  theme(plot.title= element_text(color = "black", size = 12, face = "bold", hjust = 0.5)) +
  guides(color=guide_legend(title="Model", title.hjust = 0.5)) 

#dev.off()


## PLOTTING RMSE VS DAY

x.axis <- c(1,2,3,4,5,6,7)

rmse.snaive <- c(11.01, 7.02, 7.80, 5.93, 8.00, 9.88, 6.25) # updated
rmse.TBATS <- c(5.12, 6.10, 5.50, 4.82, 5.36, 5.49, 5.73) # updated
rmse.DLSTM <- c(4.93, 5.88, 5.39, 5.01, 5.44, 6.49, 5.49) # updated
rmse.LGBM <- c(6.47,5.70,6.28, 5.43, 5.64, 6.66, 4.48) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.TBATS, rmse.DLSTM, rmse.LGBM)

## Plot

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/traffic_dayofweek.tex",width=6.0,height=5.0)

ggplot(df) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"), limits = c(1,7)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.TBATS, color = "TBATS")) +
  geom_point(aes(x = x.axis, y = rmse.TBATS)) +
  geom_line(aes(x = x.axis, y = rmse.DLSTM, color = "DLSTM")) +
  geom_point(aes(x = x.axis, y = rmse.DLSTM)) +
  geom_line(aes(x = x.axis, y = rmse.LGBM, color = "LGBM")) +
  geom_point(aes(x = x.axis, y = rmse.LGBM)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5, fill=NA)) +
  labs(title ="RMSE vs day of the week", y = "RMSE\n", x= "\nDay of the Week") +
  theme(plot.title= element_text(color = "black", size = 12, face = "bold", hjust = 0.5)) +
  guides(color=guide_legend(title="Model", title.hjust = 0.5)) 

#dev.off()


## PLOTTING RMSE VS SIZE OF TRAINING DATA

x.axis <- c(1,2,3)

rmse.snaive <- c(11.01, 11.01, 11.01) # updated
rmse.TBATS <- c(5.37, 5.33, 5.12) # updated
rmse.DLSTM <- c(7.22, 5.78, 4.93) # updated
rmse.LGBM <- c(9.04, 7.14,6.47) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.TBATS, rmse.DLSTM, rmse.LGBM)

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/traffic_trainingsize.tex",width=6.0,height=5.0)


ggplot(df) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1 Month", "6 Months", "1 Year"), limits = c(1,3)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.TBATS, color = "TBATS")) +
  geom_point(aes(x = x.axis, y = rmse.TBATS)) +
  geom_line(aes(x = x.axis, y = rmse.DLSTM, color = "DLSTM")) +
  geom_point(aes(x = x.axis, y = rmse.DLSTM)) +
  geom_line(aes(x = x.axis, y = rmse.LGBM, color = "LGBM")) +
  geom_point(aes(x = x.axis, y = rmse.LGBM)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5, fill=NA)) +
  labs(title ="RMSE vs amount of training data", y = "RMSE\n", x= "\nSize of the training set") +
  theme(plot.title= element_text(color = "black", size = 12, face = "bold", hjust = 0.5)) +
  guides(color=guide_legend(title="Model", title.hjust = 0.5)) 

#dev.off()




## -------------------------------
## GRID PLOT of all 3 checks
## -------------------------------

## Panel 1: PLOTTING RMSE VS LENGTH OF HORIZION

## PLOTTING RMSE VS LENGTH OF HORIZION
x.axis <- c(1,2,3,4)

rmse.snaive <- c(11.0189, 10.7577, 11.0548, 10.8402) # updated
rmse.TBATS <- c(5.1236, 5.73, 5.61, 5.43) # updated
rmse.DLSTM <- c(4.93, 5.57, 5.73, 5.65) # updated
rmse.LGBM <- c(6.47, 6.17, 6.07, 5.98) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.TBATS, rmse.DLSTM, rmse.LGBM)

lenhorizon <- ggplot(df,) + 
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("24", "48", "72", "96"), limits = c(1,4)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.TBATS, color = "TBATS")) +
  geom_point(aes(x = x.axis, y = rmse.TBATS)) +
  geom_line(aes(x = x.axis, y = rmse.DLSTM, color = "DLSTM")) +
  geom_point(aes(x = x.axis, y = rmse.DLSTM)) +
  geom_line(aes(x = x.axis, y = rmse.LGBM, color = "LGBM")) +
  geom_point(aes(x = x.axis, y = rmse.LGBM)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5, fill=NA)) +
  labs(title = "RMSE vs forecast horizon", y = "RMSE\n", x = "\nForecast horizon(h)") +
  theme(plot.title= element_text(color = "black", size = 12, face = "bold", hjust = 0.5)) +
  guides(color=guide_legend(title="Model", title.hjust = 0.5)) 


## Panel 2: PLOTTING RMSE VS DAY

x.axis <- c(1,2,3,4,5,6,7)

rmse.snaive <- c(11.01, 7.02, 7.80, 5.93, 8.00, 9.88, 6.25) # updated
rmse.TBATS <- c(5.12, 6.10, 5.50, 4.82, 5.36, 5.49, 5.73) # updated
rmse.DLSTM <- c(4.93, 5.88, 5.39, 5.01, 5.44, 6.49, 5.49) # updated
rmse.LGBM <- c(6.47,5.70,6.28, 5.43, 5.64, 6.66, 4.48) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.TBATS, rmse.DLSTM, rmse.LGBM)

weekday <- ggplot(df) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"), limits = c(1,7)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.TBATS, color = "TBATS")) +
  geom_point(aes(x = x.axis, y = rmse.TBATS)) +
  geom_line(aes(x = x.axis, y = rmse.DLSTM, color = "DLSTM")) +
  geom_point(aes(x = x.axis, y = rmse.DLSTM)) +
  geom_line(aes(x = x.axis, y = rmse.LGBM, color = "LGBM")) +
  geom_point(aes(x = x.axis, y = rmse.LGBM)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5, fill=NA)) +
  labs(title ="RMSE vs day of the week", y = "RMSE\n", x= "\nDay of the Week") +
  theme(plot.title= element_text(color = "black", size = 12, face = "bold", hjust = 0.5)) +
  guides(color=guide_legend(title="Model", title.hjust = 0.5)) 


## Panel 3: PLOTTING RMSE VS SIZE OF TRAINING DATA

x.axis <- c(1,2,3)

rmse.snaive <- c(11.01, 11.01, 11.01) # updated
rmse.TBATS <- c(5.37, 5.33, 5.12) # updated
rmse.DLSTM <- c(7.22, 5.78, 4.93) # updated
rmse.LGBM <- c(9.04, 7.14,6.47) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.TBATS, rmse.DLSTM, rmse.LGBM)

trainsize <- ggplot(df) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1 Month", "6 Months", "1 Year"), limits = c(1,3)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.TBATS, color = "TBATS")) +
  geom_point(aes(x = x.axis, y = rmse.TBATS)) +
  geom_line(aes(x = x.axis, y = rmse.DLSTM, color = "DLSTM")) +
  geom_point(aes(x = x.axis, y = rmse.DLSTM)) +
  geom_line(aes(x = x.axis, y = rmse.LGBM, color = "LGBM")) +
  geom_point(aes(x = x.axis, y = rmse.LGBM)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.5, fill=NA)) +
  labs(title ="RMSE vs amount of training data", y = "RMSE\n", x= "\nSize of the training set") +
  theme(plot.title= element_text(color = "black", size = 12, face = "bold", hjust = 0.5)) +
  guides(color=guide_legend(title="Model", title.hjust = 0.5)) 


#tikz("/Users/Conorcavanaugh/Dropbox/MScThesis-Conor-Manu/Latex/emergency_robustness.tex",width=6.5,height=6.0)

# 2x2 grid 3 plots without centered 3rd plot in the bottom row
ggarrange(lenhorizon, weekday, trainsize, ncol=2, nrow=2,label.x= 0.33, font.label = list(size = 11, color = "black", fontface="bold"), common.legend=TRUE, legend="bottom", align = "none")

#dev.off()

