rm(list=ls())
graphics.off()

# Loading packages 
library(tidyverse)
library(here) # this package allows you to reproducibly set your directory

# See what here() does (if you open the R Project, it should already be set to
# the correct root folder)
# Use here() to set working directory 
setwd(here("data-raw"))

#Read in data that is currently seperated by observer 
rdata_EA<-read.csv("prawnz_experiment_reflex_data_entry_EA.csv")
rdata_KF<-read.csv("prawnz_experiment_reflex_data_entry_KF - prawnz_experiment_reflex_data_entry.csv")
rdata_KM<-read.csv("prawnz_experiment_reflex_data_entry_KM - prawnz_experiment_reflex_data_entry.csv")

sdata_EA<-read.csv("prawnz_experiment_survival_data_entry_EA.csv")
sdata_KF<-read.csv("prawnz_experiment_survival_data_entry_KF - prawnz_experiment_survival_data_entry.csv")
sdata_KM<-read.csv("prawnz_experiment_survival_data_entry_KM - prawnz_experiment_survival_data_entry.csv")

tdata_EA<-read.csv("prawnz_experiment_trial_data_entry_EA.csv")
tdata_KF<-read.csv("prawnz_experiment_trial_data_entry_KF - prawnz_experiment_trial_data_entry.csv")
tdata_KM<-read.csv("prawnz_experiment_trial_data_entry_KM - prawnz_experiment_trial_data_entry.csv")

#Add a coloumn to indicate observer 
rdata_EA["Person"]<-rep("EA",nrow(rdata_EA))
rdata_KF["Person"]<-rep("KF",nrow(rdata_KF))
rdata_KM["Person"]<-rep("KM",nrow(rdata_KM))

sdata_EA["Person"]<-rep("EA",nrow(sdata_EA))
sdata_KF["Person"]<-rep("KF",nrow(sdata_KF))
sdata_KM["Person"]<-rep("KM",nrow(sdata_KM))

tdata_EA["Person"]<-rep("EA",nrow(tdata_EA))
tdata_KF["Person"]<-rep("KF",nrow(tdata_KF))
tdata_KM["Person"]<-rep("KM",nrow(tdata_KM))

#Merge observer dataframes into one.
rdata_total<-rbind(rdata_EA, rdata_KF, rdata_KM)
tdata_total<-rbind(tdata_EA, tdata_KF, tdata_KM)
sdata_total<-rbind(sdata_EA, sdata_KF, sdata_KM)

#Con
tdata_total$exp_set_lat_1<-(50+as.numeric(sub('...', '', tdata_total$exp_set_lat_1))/60)
tdata_total$exp_set_lat_2<-(50+as.numeric(sub('...', '', tdata_total$exp_set_lat_2))/60)
tdata_total$exp_set_lon_1<-(126+as.numeric(sub('....', '', tdata_total$exp_set_lon_1))/60)
tdata_total$exp_set_lon_2<-(126+as.numeric(sub('....', '', tdata_total$exp_set_lon_2))/60)
tdata_total$exp_haul_lat_1<-(50+as.numeric(sub('...', '', tdata_total$exp_haul_lat_1))/60)
tdata_total$exp_haul_lat_2<-(50+as.numeric(sub('...', '', tdata_total$exp_haul_lat_2))/60)
tdata_total$exp_haul_lon_1<-(126+as.numeric(sub('....', '', tdata_total$exp_haul_lon_1))/60)
tdata_total$exp_haul_lon_2<-(126+as.numeric(sub('....', '', tdata_total$exp_haul_lon_2))/60)

sdata_total$All_id<-paste(sdata_total$trial_number,"-",sdata_total$trap_number,"-",sdata_total$prawn_id)
rdata_total$All_id<-paste(rdata_total$trial_number,"-",rdata_total$trap_number,"-",rdata_total$prawn_id)

rdata_total_2<-rdata_total[which(rdata_total$trial_number!=11),]
tdata_total_2<-tdata_total[which(tdata_total$trial_number!=11),]
sdata_total_2<-sdata_total[which(sdata_total$trial_number!=11),]

rdata_total_3<-rdata_total_2[which(rdata_total_2$trial_number!=12),]
tdata_total_3<-tdata_total_2[which(tdata_total_2$trial_number!=12),]
sdata_total_3<-sdata_total_2[which(sdata_total_2$trial_number!=12),]

setwd(here("data-clean"))

write.csv(rdata_total_3,"2023-05-09_prawn_combined_reflex_data.csv")
write.csv(tdata_total_3,"2023-05-09_prawn_combined_trial_data.csv")
write.csv(sdata_total_3,"2023-05-09_prawn_combined_survival_data.csv")

getwd()

