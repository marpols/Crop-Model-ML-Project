# Organize climate, soil, and field data provided by Matt Ramsay
# for ML models
# Climate data from ECCC and soil data from field and CANSIS
# Created by Mariaelisa Polsinelli for AAFC, 2023


if (!require("dplyr")){
  install.packages("dplyr")
}
library("dplyr")
library("readxl")
library(lubridate)
library (tidyr)
library(ggplot2)



current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path)) 
#Functions----------------------------------------------------------------------
convert_jul <- function(year_col,month_col,day_col){
  #converts individual day, month, year column to day of year
  dates <- paste(year_col,'-',month_col,'-',day_col, sep='')
  return(yday(as.Date(dates)))
}

calcCumGDD <- function(df,cnames,start_date,end_date){
  #start and end dates as jul day
  #df include year,min_temp,max_temp,base
  start <- match()
  GDD <- max((max_temp+min_temp/2)-base,0)
}
#import and clean data----------------------------------------------------------

#CLIMATE DATA
#headers name, year, month, day, julian day, tmin, tmax, radiation, total_pp, rain, wind, vapour pressure, and co2
all_clim_yrs <- data.frame()
files <- list.files(path="C:/Users/PolsinelliM/OneDrive - AGR-AGR/MatRamsayData/Climate Files/SUMMERSIDE_RF", pattern="*.*", full.names=TRUE, recursive=FALSE)
for(f in files){
  x <- read.table(f)
  all_clim_yrs <- rbind(all_clim_yrs,x)
}
colnames(all_clim_yrs) <- c("Station","Year","Month","Day","Julian", "Min.Temp","Max.Temp","radiation","total_pp","Precip.", "wind.speed","vap","co2")
all_clim_yrs$Year <- as.numeric(all_clim_yrs$Year)
all_clim_yrs$Date <- as.Date(paste(all_clim_yrs$Year,all_clim_yrs$Month,all_clim_yrs$Day,sep="/"))
all_clim_yrs <- all_clim_yrs[order(all_clim_yrs$Date),]


#Calculate GDD
base <- 5
all_clim_yrs$GDD <- with(all_clim_yrs,((Min.Temp + Max.Temp)/2 - base))
all_clim_yrs$GDD[all_clim_yrs$GDD <0] <- 0

#calculate cumulative yearly GDD
i <- 1
j <- 1
for (y in unique(all_clim_yrs$Year)){
  while (all_clim_yrs$Year[j] == y){
    all_clim_yrs$GDDcum[j] <- sum(all_clim_yrs$GDD[i:j])
    #all_clim_yrs$precip_cum[j] <- sum(all_clim_yrs$Precip.[i:j])
    #all_clim_yrs$rad_cum[j] <- sum(all_clim_yrs$radiation[i:j])
    j <- j + 1
  }
  i <- j
}

#calculate cumulative growing season GDD, precip. and radiation
j <- 1
for (y in unique(all_clim_yrs$Year)){
  i <- which(all_clim_yrs$Year==y & all_clim_yrs$Month==5 & all_clim_yrs$Day == 1)
  while(all_clim_yrs$Year[j] == y){
    if (all_clim_yrs$Month[j] >= 5 & all_clim_yrs$Month[j] <= 11){
      all_clim_yrs$GDDcum_gs[j] <- sum(all_clim_yrs$GDD[i:j])
      all_clim_yrs$precip_gs[j] <- sum(all_clim_yrs$Precip.[i:j])
      all_clim_yrs$rad_gs[j] <- sum(all_clim_yrs$radiation[i:j])
      
    } else{
      all_clim_yrs$GDDcum_gs[j] <- 0
      all_clim_yrs$precip_gs[j] <- 0
      all_clim_yrs$rad_gs[j] <- 0
    }
    j <- j + 1
  }
}

#calculate monthly total precip. and radiation
monthly_sums <- group_by(all_clim_yrs,Year,Month) %>% summarise_at(vars(Precip.,radiation), list(sum))
colnames(monthly_sums) <- c("Year", "Month", "Precip.mo", "rad.mo")
all_clim_yrs <- merge(all_clim_yrs, monthly_sums, by = c('Year','Month'), all.y = F)



#FIELD DATA
mr_data <- data.frame(read_excel("C:/Users/PolsinelliM/OneDrive - AGR-AGR/MatRamsayData/agData_CANSIS.xlsx", guess_max = 100000))

mr_data2 <- mr_data[mr_data$varCode == "RBK",] %>% 
  subset(select=c('CFIDYr','harvest_da','harvest_mo','harvest_ye','plantDate','DEMslope1','Total_N_ap','Total_P_ap',
                  'Total_K_Ap','PH1','P2051','K201','sh_PER_SAN',
                  'sh_PER_SIL','sh_PER_CLA','sh_PER_OM','yield_moni','JulyRainE','AugRainE','SeptRainE',
                  'SOIL_CODE','SOIL_ID'))
# mr_data2 <- mr_data2[mr_data2$sh_PER_SAN != 0,]

#get individual day,month,year for plant and harvest dates and convert to day of year
mr_data2$harvest_day <- as.numeric(sprintf('%02d',mr_data2$harvest_da %% 100))
mr_data2 <- separate(mr_data2, plantDate, into=c("plant_mo","plant_da","plant_ye"),sep="/", remove=TRUE)
mr_data2$plant_jul <- convert_jul(mr_data2$plant_ye,mr_data2$plant_mo,mr_data2$plant_da)
mr_data2$harvest_jul <- convert_jul(mr_data2$harvest_ye,mr_data2$harvest_mo,mr_data2$harvest_day)
mr_data2$harvest_date <- as.Date(paste(mr_data2$harvest_ye,mr_data2$harvest_mo,mr_data2$harvest_day, sep='-'))

#set yield column as first
mr_data2 <- mr_data2 %>% dplyr::select("yield_moni", 
                                      everything())
# colnames(mr_data2) <- c("yield","CFIDyr","harvest_da", "harvest_mo", "harvest_yr",
#                         "plant_mo","plant_da","plant_yr", "DEMslope", "Total.N.ap",
#                         "Total.P.ap", "Total.K.ap", "PH", "P205","K20", "per_SAN",
#                         "per_SIL", "per_CLA", "per_OM", "plant_jul","harvest_jul")

#SOIL DATA
cansis <- read_excel("C:/Users/PolsinelliM/OneDrive - AGR-AGR/MatRamsayData/soilCode_horizons.xlsx") %>% select(2,7:9,17:22,25,30)
field_soils <- unique(mr_data2$SOIL_ID)

cansisf <- cansis[cansis$SOIL_ID %in% field_soils,]

SOIL_ID <- field_soils[!is.na(field_soils)]

#create table with columns for values @ each horizon
#1st horizon
UDEPTH_1 <- cansisf$UDEPTH[cansisf$LAYER_NO == 1]
LDEPTH_1 <- cansisf$LDEPTH[cansisf$LAYER_NO == 1]
TSAND_1 <- cansisf$TSAND[cansisf$LAYER_NO == 1]
TSILT_1 <- cansisf$TSILT[cansisf$LAYER_NO == 1]
TCLAY_1 <- cansisf$TCLAY[cansisf$LAYER_NO == 1]
ORGCARB_1 <- cansisf$ORGCARB[cansisf$LAYER_NO == 1]
PHCA_1 <- cansisf$PHCA[cansisf$LAYER_NO == 1]
PH2_1 <- cansisf$PH2[cansisf$LAYER_NO == 1]
KSAT_1 <- cansisf$KSAT[cansisf$LAYER_NO == 1]
BD_1 <- cansisf$BD[cansisf$LAYER_NO == 1]

#2nd horizon
UDEPTH_2 <- cansisf$UDEPTH[cansisf$LAYER_NO == 2]
LDEPTH_2 <- cansisf$LDEPTH[cansisf$LAYER_NO == 2]
TSAND_2 <- cansisf$TSAND[cansisf$LAYER_NO == 2]
TSILT_2 <- cansisf$TSILT[cansisf$LAYER_NO == 2]
TCLAY_2 <- cansisf$TCLAY[cansisf$LAYER_NO == 2]
ORGCARB_2 <- cansisf$ORGCARB[cansisf$LAYER_NO == 2]
PHCA_2 <- cansisf$PHCA[cansisf$LAYER_NO == 2]
PH2_2 <- cansisf$PH2[cansisf$LAYER_NO == 2]
KSAT_2 <- cansisf$KSAT[cansisf$LAYER_NO == 2]
BD_2 <- cansisf$BD[cansisf$LAYER_NO == 2]

#3rd horizon
UDEPTH_3 <- cansisf$UDEPTH[cansisf$LAYER_NO == 3]
LDEPTH_3 <- cansisf$LDEPTH[cansisf$LAYER_NO == 3]
TSAND_3 <- cansisf$TSAND[cansisf$LAYER_NO == 3]
TSILT_3 <- cansisf$TSILT[cansisf$LAYER_NO == 3]
TCLAY_3 <- cansisf$TCLAY[cansisf$LAYER_NO == 3]
ORGCARB_3 <- cansisf$ORGCARB[cansisf$LAYER_NO == 3]
PHCA_3 <- cansisf$PHCA[cansisf$LAYER_NO == 3]
PH2_3 <- cansisf$PH2[cansisf$LAYER_NO == 3]
KSAT_3 <- cansisf$KSAT[cansisf$LAYER_NO == 3]
BD_3 <- cansisf$BD[cansisf$LAYER_NO == 3]

#4th horizon
UDEPTH_4 <- cansisf$UDEPTH[cansisf$LAYER_NO == 4]
LDEPTH_4 <- cansisf$LDEPTH[cansisf$LAYER_NO == 4]
TSAND_4 <- cansisf$TSAND[cansisf$LAYER_NO == 4]
TSILT_4 <- cansisf$TSILT[cansisf$LAYER_NO == 4]
TCLAY_4 <- cansisf$TCLAY[cansisf$LAYER_NO == 4]
ORGCARB_4 <- cansisf$ORGCARB[cansisf$LAYER_NO == 4]
PHCA_4 <- cansisf$PHCA[cansisf$LAYER_NO == 4]
PH2_4 <- cansisf$PH2[cansisf$LAYER_NO == 4]
KSAT_4 <- cansisf$KSAT[cansisf$LAYER_NO == 4]
BD_4 <- cansisf$BD[cansisf$LAYER_NO == 4]

#if only one soil type
# TSAND_avg <- mean(TSAND_1,TSAND_2,TSAND_3,TSAND_4)
# TSILT_avg <- mean(TSILT_1,TSILT_2,TSILT_3,TSILT_4)
# TCLAY_avg <- mean(TCLAY_1,TCLAY_2,TCLAY_3,TCLAY_4)
# ORGCARB_avg <- mean(ORGCARB_1,ORGCARB_2,ORGCARB_3,ORGCARB_4)
# PHCA_avg <- mean(PHCA_1,PHCA_2,PHCA_3,PHCA_4)
# PH2_avg <- mean(PH2_1,PH2_2,PH2_3,PH2_3,PH2_4)
# KSAT_avg <- mean(KSAT_1,KSAT_2,KSAT_3,KSAT_4)

#if only one soil type
# horizon_cols <- data_frame(SOIL_ID,UDEPTH_1,LDEPTH_1,TSAND_1,TSILT_1,TCLAY_1,ORGCARB_1,PHCA_1,PH2_1,KSAT_1,BD_1,
#                            UDEPTH_2,LDEPTH_2,TSAND_2,TSILT_2,TCLAY_2,ORGCARB_2,PHCA_2,PH2_2,KSAT_2,BD_2,
#                            UDEPTH_3,LDEPTH_3,TSAND_3,TSILT_3,TCLAY_3,ORGCARB_3,PHCA_3,PH2_3,KSAT_3,BD_3,
#                            UDEPTH_4,LDEPTH_4,TSAND_4,TSILT_4,TCLAY_4,ORGCARB_4,PHCA_4,PH2_4,KSAT_4,BD_4,
#                            TSAND_avg,TSILT_avg,TCLAY_avg,ORGCARB_avg,PHCA_avg,PH2_avg,KSAT_avg,BD_avg)

#if more than one soil type
horizon_cols <- data_frame(SOIL_ID,UDEPTH_1,LDEPTH_1,TSAND_1,TSILT_1,TCLAY_1,ORGCARB_1,PHCA_1,PH2_1,KSAT_1,BD_1,
                           UDEPTH_2,LDEPTH_2,TSAND_2,TSILT_2,TCLAY_2,ORGCARB_2,PHCA_2,PH2_2,KSAT_2,BD_2,
                           UDEPTH_3,LDEPTH_3,TSAND_3,TSILT_3,TCLAY_3,ORGCARB_3,PHCA_3,PH2_3,KSAT_3,BD_3,
                           UDEPTH_4,LDEPTH_4,TSAND_4,TSILT_4,TCLAY_4,ORGCARB_4,PHCA_4,PH2_4,KSAT_4,BD_4,)

TSAND_avg <- horizon_cols %>% transmute(SOIL_ID, TSAND_avg =rowMeans(select(.,TSAND_1,TSAND_2,TSAND_3,TSAND_4)))
TSILT_avg <- horizon_cols %>% transmute(SOIL_ID, TSILT_avg =rowMeans(select(.,TSILT_1,TSILT_2,TSILT_3,TSILT_4)))
TCLAY_avg <- horizon_cols %>% transmute(SOIL_ID, TCLAY_avg =rowMeans(select(.,TCLAY_1,TCLAY_2,TCLAY_3,TCLAY_4)))
ORGCARB_avg <- horizon_cols %>% transmute(SOIL_ID, ORGCARB_avg =rowMeans(select(.,ORGCARB_1,ORGCARB_2,ORGCARB_3,ORGCARB_4)))
PHCA_avg <- horizon_cols %>% transmute(SOIL_ID, PHCA_avg =rowMeans(select(.,PHCA_1,PHCA_2,PHCA_3,PHCA_4)))
PH2_avg <- horizon_cols %>% transmute(SOIL_ID, PH2_avg =rowMeans(select(.,PH2_1,PH2_2,PH2_3,PH2_4)))
KSAT_avg <- horizon_cols %>% transmute(SOIL_ID, KSAT_avg =rowMeans(select(.,KSAT_1,KSAT_2,KSAT_3,KSAT_4)))
BD_avg <- horizon_cols %>% transmute(SOIL_ID, BD_avg =rowMeans(select(.,BD_1,BD_2,BD_3,BD_4)))

horizon_cols <- data_frame(SOIL_ID,UDEPTH_1,LDEPTH_1,TSAND_1,TSILT_1,TCLAY_1,ORGCARB_1,PHCA_1,PH2_1,KSAT_1,BD_1,
                           UDEPTH_2,LDEPTH_2,TSAND_2,TSILT_2,TCLAY_2,ORGCARB_2,PHCA_2,PH2_2,KSAT_2,BD_2,
                           UDEPTH_3,LDEPTH_3,TSAND_3,TSILT_3,TCLAY_3,ORGCARB_3,PHCA_3,PH2_3,KSAT_3,BD_3,
                           UDEPTH_4,LDEPTH_4,TSAND_4,TSILT_4,TCLAY_4,ORGCARB_4,PHCA_4,PH2_4,KSAT_4,BD_4,
                           TSAND_avg[2],TSILT_avg[2],TCLAY_avg[2],ORGCARB_avg[2],PHCA_avg[2],PH2_avg[2],KSAT_avg[2],BD_avg[2])

#COMBINE------------------------------------------------------------------------
all_clim_yrs$Day <- as.numeric(all_clim_yrs$Day)
h_dates <- unique(mr_data2$harvest_date)
h_years <- unique(mr_data2$harvest_ye)
Year <- h_years[!is.na(h_years)]
gs <- c(5,6,7,8,9) #growing season months May to Sept

#monthly precip and rad totals for growing season (excpet october)
monthlies <- monthly_sums[monthly_sums$Year %in% h_years & monthly_sums$Month %in% gs,]

#values from September until harvest date(GDD cumulative,precip,rad) (precip/rad on harvest date-1 - precip/rad on sept. 31st.)
aug31 <- all_clim_yrs[all_clim_yrs$Day==31 & all_clim_yrs$Month==8,] %>% select(1,18,19)
colnames(aug31) <- c("Year", "Aug31.precip.cum","Aug31.rad.cum")
hd <- all_clim_yrs[all_clim_yrs$Date %in% (h_dates-1),] %>% select(1,4,14,16,17,18,19) #cum precip, rad until harvest day
colnames(hd) <- c("Year","Day","Date","GDDcum","GDDcum_gs","HD.precip.cum_gs","HD.rad.cum_gs")
hd <- merge(hd, aug31, by="Year")
hd$Sept2harv.precip <- with(hd, hd$HD.precip.cum-hd$Aug31.precip.cum)
hd$Sept2harv.rad <- with(hd, hd$HD.rad.cum-hd$Aug31.rad.cum)

#arrange monthly growing season cumGDD, precip, and rad
May <- all_clim_yrs[all_clim_yrs$Year %in% Year & all_clim_yrs$Month==5 & 
                      all_clim_yrs$Day==31,]
June <- all_clim_yrs[all_clim_yrs$Year %in% Year & all_clim_yrs$Month==6 & 
                       all_clim_yrs$Day==30,]
July <- all_clim_yrs[all_clim_yrs$Year %in% Year & all_clim_yrs$Month==7 & 
                       all_clim_yrs$Day==31,]
Aug <- all_clim_yrs[all_clim_yrs$Year %in% Year & all_clim_yrs$Month==8 & 
                      all_clim_yrs$Day==31,]
Sept <- all_clim_yrs[all_clim_yrs$Year %in% Year & all_clim_yrs$Month==9 & 
                      all_clim_yrs$Day==30,]

May.GDDcum <- select(May,16)
June.GDDcum <- select(June,16)
July.GDDcum <- select(July,16)
Aug.GDDcum <- select(Aug,16)
Sept.GDDcum <- select(Sept,16)

May.GDDcum_gs <- select(May,17)
June.GDDcum_gs <- select(June,17)
July.GDDcum_gs <- select(July,17)
Aug.GDDcum_gs <- select(Aug,17)
Sept.GDDcum_gs <- select(Sept,17)

May.precip <- monthlies[monthlies$Month==5,3] 
June.precip <- monthlies[monthlies$Month==6,3]
July.precip <- monthlies[monthlies$Month==7,3]
Aug.precip <- monthlies[monthlies$Month==8,3]
Sept.precip <- monthlies[monthlies$Month==9,3]

May.rad <- monthlies[monthlies$Month==5,4] 
June.rad <- monthlies[monthlies$Month==6,4]
July.rad <- monthlies[monthlies$Month==7,4]
Aug.rad <- monthlies[monthlies$Month==8,4]
Sept.rad <- monthlies[monthlies$Month==9,4]

gs_by_month <- data.frame(Year,May.GDDcum,June.GDDcum,July.GDDcum,Aug.GDDcum,Sept.GDDcum,May.GDDcum_gs,June.GDDcum_gs,July.GDDcum_gs,
                          Aug.GDDcum_gs,Sept.GDDcum_gs,May.precip,
                          June.precip,July.precip,Aug.precip,Sept.precip,
                          May.rad,June.rad,July.rad,Aug.rad,
                          Sept.rad)
colnames(gs_by_month) <- c("Year","May.GDDcum","June.GDDcum","July.GDDcum",
                           "Aug.GDDcum","Sept.GDDcum","May.GDDcum_gs","June.GDDcum_gs","July.GDDcum_gs",
                           "Aug.GDDcum_gs","Sept.GDDcum_gs","May.precip",
                           "June.precip","July.precip","Aug.precip","Sept.precip",
                           "May.rad","June.rad","July.rad","Aug.rad",
                           "Sept.rad")


hd$Date <- hd$Date + 1
hy_climdata <- merge(hd,gs_by_month,by="Year")
ALL_DATA <- merge(hy_climdata, mr_data2, by.x="Date", by.y="harvest_date")
ALL_DATA <- merge(ALL_DATA, horizon_cols, by="SOIL_ID")
# ALL_DATA <- rename(ALL_DATA, "Yield" = "yield_moni")
ALL_DATA$yield_tha <- ALL_DATA$yield_moni/892.2

#write
write.csv(ALL_DATA, file = "mattRamsay_ML_ALL_DATA_ver3.csv")
#Data Plots---------------------------------------------------------------------
yield_dist <- ggplot(ALL_DATA, aes(x=yield_tha)) + geom_histogram(color="black", fill="white") + labs(x="Yield (t/ha)", title = "Distribution of all Russet Yield Datapoints")
