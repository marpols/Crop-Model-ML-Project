library("dplyr")
library("readxl")
library(ggplot2)
library (tidyr)

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path)) 

data <- data.frame(read.csv("C:/Users/PolsinelliM/OneDrive - AGR-AGR/MatRamsayData/cum30yr_climateData.csv"))

#get average 30 year GDD, precipitation, radiation
averages <- data[(data$Year < 2022 & data$Year > 1991),] %>% group_by(,Julian) %>% summarise_at(vars(GDDcum,precip_cum,rad_cum), list(mean))
GDD_avg <- ggplot(averages, 
                  aes(x=Julian, y=GDDcum)) + geom_line(color="dark green", 
                  linewidth=1.0)+ labs(x = "doy", y="Cummulative GDD (base 5)", 
                  title="30 Year Average Yearly Cummulative GDD (1991-2021)") + scale_x_continuous(breaks = seq(0,365, by=20)) + coord_cartesian(ylim = c(0,2200)) + scale_y_continuous(breaks = seq(0,2200,by=200)) + theme_bw()
ggsave("Figures/GDD30yrAvg.png", width=7, height=5)

precip_avg <- ggplot(averages, 
                    aes(x=Julian, y=precip_cum)) + geom_line(color="blue", 
                    linewidth=1.0)+ labs(x = "doy", y="Precipitation (mm)", 
                    title="30 Year Average Yearly Cummulative Precipitation (1991-2021)") + scale_x_continuous(breaks = seq(0,365, by=20)) + coord_cartesian(ylim = c(0,1400)) + scale_y_continuous(breaks = seq(0,1400,by=100)) + theme_bw()
ggsave("Figures/precip30yrAvg.png", width=7, height=5)

rad_avg <- ggplot(averages, aes(x=Julian, y=rad_cum)) + geom_line(color="red", 
          linewidth=1.0)+ labs(x = "doy", y="Radiation (MJ.m-2)", 
          title="30 Year Average Yearly Cummulative Radiation (1991-2021)") + scale_x_continuous(breaks = seq(0,365, by=20)) + coord_cartesian(ylim = c(0,5000)) + scale_y_continuous(breaks = seq(0,5000,by=500)) + theme_bw()
ggsave("Figures/rad30yrAvg.png", width=7, height=5)


years <- c(2015,2016,2017,2019,2020,2021) #STICS years
#GDD plots
for (y in years){
  GDD_plot <- ggplot(data[data$Year==y,], 
            aes(x=Julian, y=GDDcum)) + geom_line(color="dark green", 
            linewidth=1.0)+ labs(x = "doy", y="Cummulative GDD (base 5)", 
            title=paste(as.character(y),"Cummulative GDD")) + coord_cartesian(ylim = c(0,2200)) + scale_y_continuous(breaks = seq(0,2200,by=200)) + theme_bw()
  ggsave(paste("Figures/",as.character(y),"GDD.png",sep=""), width=7, height=5)
}

#precipitation plot
for (y in years){
  precip_plot <- ggplot(data[data$Year==y,], 
                aes(x=Julian, y=precip_cum)) + geom_line(color="blue", 
                linewidth=1.0)+ labs(x = "doy", y="Precipitation (mm)", 
                title=paste(as.character(y),"Cummulative Precipitation")) + scale_x_continuous(breaks = seq(0,365, by=20)) + coord_cartesian(ylim = c(0,1400)) + scale_y_continuous(breaks = seq(0,1400,by=100)) + theme_bw()
  ggsave(paste("Figures/",as.character(y),"precip.png",sep=""), width=7, height=5)
}

#rad plots
for (y in years){
  rad_plot <- ggplot(data[data$Year==y,], 
                aes(x=Julian, y=rad_cum)) + geom_line(color="red", 
                linewidth=1.0)+ labs(x = "doy", y="Radiation (MJ.m-2)", 
                title=paste(as.character(y),"Cummulative Radiation")) + scale_x_continuous(breaks = seq(0,365, by=20)) + coord_cartesian(ylim = c(0,5000)) + scale_y_continuous(breaks = seq(0,5000,by=500)) + theme_bw()
  ggsave(paste("Figures/",as.character(y),"rad.png",sep=""), width=7, height=5)
}
