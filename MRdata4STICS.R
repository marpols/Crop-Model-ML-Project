if (!require("dplyr")){
  install.packages("dplyr")
}
library("dplyr")
library("readxl")
library(lubridate)

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path)) 

mr_data <- data.frame(read_excel("C:/Users/PolsinelliM/OneDrive - AGR-AGR/MatRamsayData/agData_CANSIS.xlsx", guess_max = 100000))

#data_names <- c("CFID","CFIDYr","harvest_ye","varCode","plantDate","yield_moni","yldpp5100","yldIdwP2")

#FUNCTION-----------------------------------------------------------------------
convert_jul <- function(year_col,month_col,day_col){
  #converts individual day, month, year column to day of year
  dates <- paste(year_col,'-',month_col,'-',day_col, sep='')
  return(yday(as.Date(dates)))
}
#-------------------------------------------------------------------------------

#summarise yield
data_summary <- mr_data[mr_data$varCode == "RBK",] %>% group_by(CFID,harvest_ye,CFIDYr) %>% summarise_at(vars(yield_moni), list(med_yld = median, mean_yld = mean, yld_SD = sd), na.rm=TRUE)

i <- 1
for (n in match(data_summary$CFIDYr,mr_data$CFIDYr)){
  data_summary$plantDate[i] <- mr_data$plantDate[n]
  data_summary$Total_N_app[i] <- mr_data$Total_N_ap[n]
  data_summary$Total_P_app[i] <- mr_data$Total_P_ap[n]
  data_summary$Total_K_app[i] <- mr_data$Total_K_Ap[n]
  data_summary$harvest_date[i] <- mr_data$harvest_da[n]
  data_summary$harvest_mo[i] <- mr_data$harvest_mo[n]
  data_summary$harvest_yr[i] <- mr_data$harvest_ye[n]
  data_summary$sh_per_N[i] <- mr_data$sh_PER_N[n]
  data_summary$BNA[i] <- mr_data$sh_BNA__mg[n]
  data_summary$CN_ratio <- mr_data$sh_C_NRATI[n]
  data_summary$SOIL_CODE[i] <- mr_data$SOIL_CODE[n]
  data_summary$SOIL_ID[i] <- mr_data$SOIL_ID[n]
  i <- i + 1
}

data_summary$med_yld_th <- with(data_summary, data_summary$med_yld/892.2) #yield in tonnes/ha (STICS units) converted from pounds/acre
data_summary$mean_yld_th <- with(data_summary, data_summary$mean_yld/892.2) 
data_summary$yld_SD_th <- with(data_summary, data_summary$yld_SD/892.2) 

#split N fert into. blend and urea and convert to kg/ha.
data_summary$fert_blend <- with(data_summary, 100*1.121)
data_summary$fert_urea <- with(data_summary, (data_summary$Total_N_app - 100) *1.121)

#get individual day,month,year for plant and harvest dates and convert to day of year
data_summary$harvest_da <- as.numeric(sprintf('%02d',data_summary$harvest_date %% 100))
data_summary <- separate(data_summary, plantDate, into=c("plant_mo","plant_da","plant_ye"),sep="/", remove=FALSE)
data_summary$plant_jul <- convert_jul(data_summary$plant_ye,data_summary$plant_mo,data_summary$plant_da)
data_summary$harvest_jul <- convert_jul(data_summary$harvest_ye,data_summary$harvest_mo,data_summary$harvest_da)


#get fields that have soil information
soildata_flds <- mr_data[mr_data$sh_PER_SAN != 0 & mr_data$varCode == "RBK",] %>% 
  group_by(CFID,harvest_ye, CFIDYr)%>% summarise_at(vars(sh_PER_SAN, sh_PER_SIL, sh_PER_CLA,PH1, sh_PER_OM), list(median), na.rm=TRUE)

#combine
all_data <- merge(data.frame(data_summary, row.names=NULL), data.frame(soildata_flds, row.names=NULL), 
                  by = c("CFID","harvest_ye","CFIDYr"), all = TRUE)[-1]

#convert BNA to mineral N

#get CANSIS soil data
cansis <- read_excel("C:/Users/PolsinelliM/OneDrive - AGR-AGR/MatRamsayData/soilCode_horizons.xlsx") %>% select(2,7:9,17:22,25,30)
field_soils <- unique(data_summary$SOIL_ID)

cansisf <- cansis[cansis$SOIL_ID %in% field_soils,]

SOIL_ID <- field_soils

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


horizon_cols <- data_frame(SOIL_ID,UDEPTH_1,LDEPTH_1,TSAND_1,TSILT_1,TCLAY_1,ORGCARB_1,PHCA_1,PH2_1,KSAT_1,BD_1,
                           UDEPTH_2,LDEPTH_2,TSAND_2,TSILT_2,TCLAY_2,ORGCARB_2,PHCA_2,PH2_2,KSAT_2,BD_2,
                           UDEPTH_3,LDEPTH_3,TSAND_3,TSILT_3,TCLAY_3,ORGCARB_3,PHCA_3,PH2_3,KSAT_3,BD_3,
                           UDEPTH_4,LDEPTH_4,TSAND_4,TSILT_4,TCLAY_4,ORGCARB_4,PHCA_4,PH2_4,KSAT_4,BD_4)

#merge with cansis soil data
all_data <- merge(all_data, horizon_cols, by="SOIL_ID")

all_data$harvest_jul <- convert_jul(all_data$harvest_ye, all_data$harvest_mo,all_data$harvest_da)


write.csv(all_data[!is.na(all_data$sh_PER_SAN),], file="agData_STICS_wSOIL2.csv")
write.csv(all_data[!is.na(all_data$harvest_ye),], file="agData_STICS_all2.csv")




