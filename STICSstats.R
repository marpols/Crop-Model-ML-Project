# ***********************
# Create statistics summary tables for STICS usms
# Uses package SticsRPacks found at https://github.com/SticsRPacks
# Mariaelisa Polsinelli for AAFC, 2022
# ***********************

if (!require("devtools")){
  install.packages("devtools")
}
library("devtools")
if (!require("SticsRPacks")){
  devtools::install_github("SticsRPacks/SticsRPacks")
}
library("readxl")
library("SticsRPacks")
if (!require("dplyr")){
  install.packages("dplyr")
}
library("dplyr")

workspace = "C:/Users/PolsinelliM/OneDrive - AGR-AGR/LL2022/Atlantic_Potato-kristen/Atlantic_Potato-kristen/JavaSTICS-1.41-stics-9.2-MattRamsayData/Fields_twoFert_rootCalib"

sti_dir <- "/mod_aclim4q03rsmin200"

usms <- SticsRFiles::get_usms_list(file = file.path(workspace, "usms.xml"))

sim <- SticsRFiles::get_sim(workspace = paste(workspace,sti_dir,sep=""),usms_file = file.path(workspace, "usms.xml"))

obs <- SticsRFiles::get_obs(workspace =  workspace,usm = usms,usms_file = file.path(workspace, "usms.xml"))

pred_ylds <- data.frame(matrix(NA,nrow = 20,ncol=3))
colnames(pred_ylds) <- c("CFIDYr","yield","mafrais")
i <- 1
for (s in sim){
  pred_ylds$CFIDYr[i] <- usms[i]
  pred_ylds$yield[i] <- as.double(tail(s[44],n=1))
  pred_ylds$mafrais[i] <- as.double(tail(s[43],n=1))
  i <- i+1
}


write.csv(pred_ylds,file="predictions.csv")  

stats <- summary(sim, obs = obs, all_situations = T)
ver <- 1
write.csv(stats[,c("variable", "n_obs", "sd_obs", "sd_sim","RMSEs","RMSEu")],paste("STICSstats",ver,".csv",sep=""))                            
