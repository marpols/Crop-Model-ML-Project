# Random Forest on field data provided by Matt Ramsay, Climate data from ECCC
# and soil data from field and CANSIS
# Created by Mariaelisa Polsinelli for AAFC, 2023

if (!require("randomForest")){
  install.packages("randomForest")
}
if (!require("caret")){
  install.packages("caret")
}
if (!require("dplyr")){
  install.packages("dplyr")
}
if (!require("Metrics")){
  install.packages("Metrics")
}
if (!require("ggplot2")){
  install.packages("ggplot2")
}
library("dplyr")
library("readxl")
library(randomForest)
library(caret)
library(Metrics)
library(ggplot2)
library (tidyr)


set.seed(1234)#set random seed

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path)) 

####
#ag_data <- data.frame(read.csv("C:"))
data <- data.frame(read.csv("C:/Users/PolsinelliM/OneDrive - AGR-AGR/MatRamsayData/mattRamsay_ML_ALL_DATA_ver3.csv"))
data <- data %>% dplyr::select("yield_moni", everything())
data$yield_tha <- data$yield_moni/892.2
data <- data %>% dplyr::select("yield_tha", everything())
yield_dist <- ggplot(data, aes(x=yield_tha)) + geom_histogram(color="black", fill="white") + labs(x="Yield (t/ha)", title = "Distribution of all Russet Yield Datapoints")
original_dat <- data

#all data points
all_data <- original_dat
all_data$PH1[which(original_dat$PH1 %in% c(0))] <- 6.0
for (x in which(original_dat$sh_PER_CLA %in% c(0)))
  all_data$sh_PER_CLA[x] <- original_dat$TCLAY_1[x]


all_data$sh_PER_CLA[which(original_dat$sh_PER_CLA %in% c(0))] <- original_dat$TCLAY_1
all_data$sh_PER_SAN[which(original_dat$sh_PER_SAN %in% c(0))] <- original_dat$TSAND_1
all_data$sh_PER_SIL[which(original_dat$sh_PER_SIL %in% c(0))] <- original_dat$TSILT_1

#select only datapoints that have %sand info (have soil samples)
ss_data <- subset(original_dat, original_dat$sh_PER_SAN != 0) %>% select(1,8:35,43:47,50:56,59,60,63:70,73:80,83:90,93:108)
yld_dist_snd <- ggplot(ss_data, aes(x=yield_tha)) + geom_histogram(color="black", fill="white") + labs(x="Yield (t/ha)", title = "Distribution of Russet Yield Datapoints with Field Soil Information")
ss_data$PH1[which(ss_data$PH1 %in% c(0))] <- 6.0

#growing season GDD Vs whole year GDD (Cansis data removed b/c fields with soil samples all CTW)
gdd_yr <- ss_data %>% select(1,2,4:14,20:43)
gdd_gs <- ss_data %>% select(1,3,4:9,15:43)



data <- select(ss_data,1,2,3,4,5,7,9,24,30,34,37,42,43)
# data <- select(ss_data,1,2,4,5,7,9,42,43)

#RANDOM FOREST -----------------------------------------------------------------

#train/test split
c <- length(data)
ind <- sample(2, nrow(data), replace = TRUE, prob=c(0.8,0.2))
train_x <- data[ind==1,2:c]
train_y <- data[ind==1,1]
test_x <- data[ind==2,2:c]
test_y <- data[ind==2,1]

tuneRF(train_x,train_y, stepFactor=1.5)

#Run Random Forest
rf <- randomForest(train_x,y=train_y, proximity=TRUE, ntree=500, mtry=6, plot=TRUE)

print(rf)

rmse_train <- rmse(train_y, rf[3]$predicted)
rmse_train
rrmse_train <- rmse_train/mean(train_y)
rrmse_train

#Plot train metrics
plot_data_train <- data.frame(train_y,rf[3],seq(length(train_y))) %>% rename(n=seq.length.train_y..)

PredObs_train <- ggplot(plot_data_train, aes(x=train_y, y = predicted)) + geom_point() + geom_smooth(method = lm) + labs(x_axis = "observed")
PredObs_train

#variable importance
varImpPlot(rf, sort=TRUE, n.var=min(30, nrow(rf$importance)),
           type=NULL, class=NULL, scale=TRUE)

prediction <- predict(rf,test_x,type="response",predict.all=TRUE)

           
#calculate error
rmse <- rmse(test_y, prediction[[1]])
rmse
rrmse <- rmse/mean(test_y)
rrmse
mse <- mse(test_y, prediction[[1]])
mse
r2 <- cor(test_y,prediction[[1]])^2
r2

#plot predictions vs. observed
plot_data <- data.frame(test_y,prediction[[1]],seq(length(test_y))) %>% rename(predicted = prediction..1.., n=seq.length.test_y..)

PredObs <- ggplot(plot_data, aes(x=test_y, y = predicted)) + geom_point() + geom_smooth(method = lm)
PredObs
