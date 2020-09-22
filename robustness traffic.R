### Robustness checks - traffic flow data ###


# loading packages

library(ggplot2)
library(forecast)
library(tikzDevice)
library(ggpubr)
library(gridExtra)

# loading the data into R (2 local paths)
#df_ori <- read.csv("/Users/Conorcavanaugh/Dropbox/CBS MSc Thesis Research Folder/DATA & Code/Conor Code/taxi_series_hourly.csv", header = TRUE)
df_ori <- read.csv("/Users/Manu/Dropbox/CBS MSc Thesis Research Folder/DATA & Code/Conor Code/taxi_series_hourly.csv", header = TRUE)

ts_ori <- ts(df_ori$X0, start = c(1,1), frequency = 24) 

## different lenghts of train and test are used
## train ending on 356,24 is the one used for regular results and is friday the 21th
## test ending on 357,24 is the one used for regular results and is saturday the 22th
## test set goes from saturday the 22th to friday the 28th
train <- window(ts_ori, start = c(1,1),end = c(362,24))
test <- window(ts_ori, start = c(363,1), end = c(363,24))

## SATURDAY 22
bats.fit <- bats(train, seasonal.periods  = c(24,168))
bats.fc <- forecast(bats.fit, h = 24)

accuracy(bats.fc, test) ## 851.9753

## SUNDAY 23
bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)

accuracy(bats.fc, test) ## 581.2675

## MONDAY 24
bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)

accuracy(bats.fc, test) ## 356.3960

## TUESDAY 25
bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)

accuracy(bats.fc, test) ## 446.9798

## WEDNESDAY 26
bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)

accuracy(bats.fc, test) ## 493.7384

## THURSDAY 27
bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)

accuracy(bats.fc, test) ## 622.9726

## FRIDAY 28
bats.refit <- bats(train, model = bats.fit)
bats.fc <- forecast(bats.refit, h = 24)

accuracy(bats.fc, test) ## 1040.1084


## PLOTTING RMSE VS LENGTH OF HORIZION
x.axis <- c(1,2,3,4)

rmse.snaive <- c(3072.02, 3488.89, 3072.86, 2800.98) # updated
rmse.BATS <- c(851, 746, 632, 604) # updated
rmse.DLSTM <- c(528, 730, 703, 779) # updated
rmse.LGBM <- c(808, 848, 852, 805) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.BATS, rmse.DLSTM, rmse.LGBM)
  

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/rmse_fchorizon.tex",width=6.0,height=5.0)

ggplot(df,) + 
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("24", "48", "72", "96"), limits = c(1,4)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.BATS, color = "BATS")) +
  geom_point(aes(x = x.axis, y = rmse.BATS)) +
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

rmse.snaive <- c(3072, 1550, 3559, 1288, 914, 615, 1526) # updated
rmse.BATS <- c(851, 581, 356, 446, 493, 622, 1040) # updated
rmse.DLSTM <- c(528,520,436,488,650,616,859) # updated
rmse.LGBM <- c(808,567,483,555,618,679,546) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.BATS, rmse.DLSTM, rmse.LGBM)

## Plot

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/traffic_dayofweek.tex",width=6.0,height=5.0)

ggplot(df) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), limits = c(1,7)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.BATS, color = "BATS")) +
  geom_point(aes(x = x.axis, y = rmse.BATS)) +
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

rmse.snaive <- c(3072, 3072, 3072) # updated
rmse.BATS <- c(476, 731, 852) # updated
rmse.DLSTM <- c(1470,892,528) # updated
rmse.LGBM <- c(880,813,808) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.BATS, rmse.DLSTM, rmse.LGBM)

#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/traffic_trainingsize.tex",width=6.0,height=5.0)


ggplot(df) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1 Month", "6 Months", "1 Year"), limits = c(1,3)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.BATS, color = "BATS")) +
  geom_point(aes(x = x.axis, y = rmse.BATS)) +
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


#### GRID plot (all three robustness checks) #### 

## Panel 1: PLOTTING RMSE VS LENGTH OF HORIZION

x.axis <- c(1,2,3,4)

rmse.snaive <- c(3072.02, 3488.89, 3072.86, 2800.98) # updated
rmse.BATS <- c(851, 746, 632, 604) # updated
rmse.DLSTM <- c(528, 730, 703, 779) # updated
rmse.LGBM <- c(808, 848, 852, 805) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.BATS, rmse.DLSTM, rmse.LGBM)


lenhorizon <- ggplot(df,) + 
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("24", "48", "72", "96"), limits = c(1,4)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.BATS, color = "BATS")) +
  geom_point(aes(x = x.axis, y = rmse.BATS)) +
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

rmse.snaive <- c(3072, 1550, 3559, 1288, 914, 615, 1526) # updated
rmse.BATS <- c(851, 581, 356, 446, 493, 622, 1040) # updated
rmse.DLSTM <- c(528,520,436,488,650,616,859) # updated
rmse.LGBM <- c(808,567,483,555,618,679,546) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.BATS, rmse.DLSTM, rmse.LGBM)

## Plot

weekday <- ggplot(df) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"), limits = c(1,7)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.BATS, color = "BATS")) +
  geom_point(aes(x = x.axis, y = rmse.BATS)) +
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

rmse.snaive <- c(3072, 3072, 3072) # updated
rmse.BATS <- c(476, 731, 852) # updated
rmse.DLSTM <- c(1470,892,528) # updated
rmse.LGBM <- c(880,813,808) # updated

df <- data.frame(x.axis, rmse.snaive, rmse.BATS, rmse.DLSTM, rmse.LGBM)

trainsize <- ggplot(df) + 
  scale_x_continuous(breaks = c(1,2,3), labels = c("1 Month", "6 Months", "1 Year"), limits = c(1,3)) + 
  geom_line(aes(x = x.axis, y = rmse.snaive, color = "Snaive")) +
  geom_point(aes(x = x.axis, y = rmse.snaive)) +
  geom_line(aes(x = x.axis, y = rmse.BATS, color = "BATS")) +
  geom_point(aes(x = x.axis, y = rmse.BATS)) +
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


#tikz("/Users/Manu/Dropbox/MScThesis-Conor-Manu/Latex/traffic_robustness.tex",width=6.5,height=6.0)

# 2x2 grid 3 plots without centered 3rd plot in the bottom row
ggarrange(lenhorizon, weekday, trainsize, ncol=2, nrow=2,label.x= 0.33, font.label = list(size = 11, color = "black", fontface="bold"), common.legend=TRUE, legend="bottom", align = "none")

#dev.off()




### Alternative grid laypout: 2x2 grid 3 plots with centered 3rd plot ###

# code to center lower row panel
(layout_matrix <- matrix(c(1, 1, 2, 2, 4, 3, 3, 4), nrow = 2, byrow = TRUE))
#      [,1] [,2] [,3] [,4]
# [1,]    1    1    2    2
# [2,]    4    3    3    4
grid.arrange(lenhorizon, weekday, trainsize, layout_matrix = layout_matrix)







