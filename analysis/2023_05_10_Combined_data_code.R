rm(list=ls())
graphics.off()

# Loading packages 
library(tidyverse)
library(here) # this package allows you to reproducibly set your directory

# Use here() to set working directory 
setwd(here("data-raw"))

##The trial data files contains trial level information on our experiment 
##Each of the files contains data collected by one observer
tdata_EA<-read.csv("prawnz_experiment_trial_data_entry_EA.csv")
tdata_KF<-read.csv("prawnz_experiment_trial_data_entry_KF - prawnz_experiment_trial_data_entry.csv")
tdata_KM<-read.csv("prawnz_experiment_trial_data_entry_KM - prawnz_experiment_trial_data_entry.csv")
head(tdata_KM)

##The survival data files contains individual level survival information about each prawn
##Each of the files contains data collected by one observer
sdata_EA<-read.csv("prawnz_experiment_survival_data_entry_EA.csv")
sdata_KF<-read.csv("prawnz_experiment_survival_data_entry_KF - prawnz_experiment_survival_data_entry.csv")
sdata_KM<-read.csv("prawnz_experiment_survival_data_entry_KM - prawnz_experiment_survival_data_entry.csv")
head(sdata_EA)

##The reflex data files contains individual level reflex information about the prawns that survive
##Each of the files contains data collected by one observer
rdata_EA<-read.csv("prawnz_experiment_reflex_data_entry_EA.csv")
rdata_KF<-read.csv("prawnz_experiment_reflex_data_entry_KF - prawnz_experiment_reflex_data_entry.csv")
rdata_KM<-read.csv("prawnz_experiment_reflex_data_entry_KM - prawnz_experiment_reflex_data_entry.csv")
head(rdata_EA)


##Add a coloumn to indicate the observer that collected the data
rdata_EA["Person"]<-rep("EA",nrow(rdata_EA))
rdata_KF["Person"]<-rep("KF",nrow(rdata_KF))
rdata_KM["Person"]<-rep("KM",nrow(rdata_KM))

sdata_EA["Person"]<-rep("EA",nrow(sdata_EA))
sdata_KF["Person"]<-rep("KF",nrow(sdata_KF))
sdata_KM["Person"]<-rep("KM",nrow(sdata_KM))

tdata_EA["Person"]<-rep("EA",nrow(tdata_EA))
tdata_KF["Person"]<-rep("KF",nrow(tdata_KF))
tdata_KM["Person"]<-rep("KM",nrow(tdata_KM))

##Merge dataframes collected by each observer into one.
rdata_total<-rbind(rdata_EA, rdata_KF, rdata_KM)
tdata_total<-rbind(tdata_EA, tdata_KF, tdata_KM)
sdata_total<-rbind(sdata_EA, sdata_KF, sdata_KM)

##Trial data includes lat and long coordinates at which each trap was set and hauled
##However, the data is in degrees, and seconds. This code converts those units into
##degrees and decimals of degrees. It does this by removing the first few characters, 
##which are the whole degrees and and the decimal. The remainder is arc seconds divided
##60 seconds to convert it into hundredths of degrees.
tdata_total$exp_set_lat_1<-(50+as.numeric(sub('...', '', tdata_total$exp_set_lat_1))/60)
tdata_total$exp_set_lat_2<-(50+as.numeric(sub('...', '', tdata_total$exp_set_lat_2))/60)
tdata_total$exp_set_lon_1<-(126+as.numeric(sub('....', '', tdata_total$exp_set_lon_1))/60)
tdata_total$exp_set_lon_2<-(126+as.numeric(sub('....', '', tdata_total$exp_set_lon_2))/60)
tdata_total$exp_haul_lat_1<-(50+as.numeric(sub('...', '', tdata_total$exp_haul_lat_1))/60)
tdata_total$exp_haul_lat_2<-(50+as.numeric(sub('...', '', tdata_total$exp_haul_lat_2))/60)
tdata_total$exp_haul_lon_1<-(126+as.numeric(sub('....', '', tdata_total$exp_haul_lon_1))/60)
tdata_total$exp_haul_lon_2<-(126+as.numeric(sub('....', '', tdata_total$exp_haul_lon_2))/60)

##This creates an individual ID column. The column prawn_id gives the prawn number for that
##prawn in that trial. It starts at 1 for the first prawn entered in the data and 
## gos up to n (the number of prawns in that trial) for the last prawn. All_id includes
## the trial and trap as well so it is 
sdata_total$All_id<-paste(sdata_total$trial_number,"-",sdata_total$trap_number,"-",sdata_total$prawn_id)
rdata_total$All_id<-paste(rdata_total$trial_number,"-",rdata_total$trap_number,"-",rdata_total$prawn_id)

##This removes two trials, 11 and 12, from the data set.
rdata_total_2<-rdata_total[which(rdata_total$trial_number!=11),]
tdata_total_2<-tdata_total[which(tdata_total$trial_number!=11),]
sdata_total_2<-sdata_total[which(sdata_total$trial_number!=11),]

rdata_total_3<-rdata_total_2[which(rdata_total_2$trial_number!=12),]
tdata_total_3<-tdata_total_2[which(tdata_total_2$trial_number!=12),]
sdata_total_3<-sdata_total_2[which(sdata_total_2$trial_number!=12),]

##Data cleaning
bad_rows<-c(which(is.na(sdata_total_3$dead+sdata_total_3$alive+sdata_total_3$scavenged)),
            which(is.na(sdata_total_3$trap_number)))
sdata_total_3[bad_rows,]

sdata_total_3[bad_rows[1],]$scavenged<-0

sdata_total_4<-sdata_total_3[-bad_rows[2:3],]


bad_rows<-which(is.na(tdata_total_3$X2h_number))
tdata_total_3[bad_rows,]$X2h_number<-0

rdata_total_3$score<-rowSums(rdata_total_3[6:15])

##Change working directory to desired location for new files
setwd(here("data-clean"))

##Save clean data-frames to csv files.
write.csv(rdata_total_3,"2023-05-09_prawn_combined_reflex_data.csv")
write.csv(tdata_total_3,"2023-05-09_prawn_combined_trial_data.csv")
write.csv(sdata_total_4,"2023-05-09_prawn_combined_survival_data.csv")





