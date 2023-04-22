# Small Script to generate STICS .obs files from data provided by Matt Ramsay
# Created by Mariaelisa Polsinelli for AAFC, 2023

if (!require("dplyr")){
  install.packages("dplyr")
}
library("dplyr")
library("readxl")
library(lubridate)
library (tidyr)
library(ggplot2)

wd <- "C:/Users/PolsinelliM/OneDrive - AGR-AGR/LL2022/Atlantic_Potato-kristen/Atlantic_Potato-kristen/JavaSTICS-1.41-stics-9.2-MattRamsayData/FieldsWithSoilSampleData/observations"
setwd(wd)

obs_data <- data.frame(read.csv("C:/Users/PolsinelliM/OneDrive - AGR-AGR/MatRamsayData/agData_STICS_wSOIL2.csv")) %>% select(4,23,15,16,17,26,28,29)
obs_data$harvest_da <- as.numeric(sprintf('%02d',obs_data$harvest_date %% 100))                       

i <- 1
while (i <= length(obs_data$CFIDYr)){
  new_obs <- paste(obs_data$harvest_yr[i],obs_data$harvest_mo[i],obs_data$harvest_da[i],
                   obs_data$harvest_jul[i],obs_data$mean_yld_th[i], sep=";")
  new_obs <- paste("ian;mo;jo;jul;pdsfruitfrais;\n",new_obs,";",sep="")
  write.table(new_obs, file=paste(obs_data$CFIDYr[i],".obs",sep=""),row.names=FALSE,col.names=FALSE,quote=FALSE)
  i <- i+1
}                       