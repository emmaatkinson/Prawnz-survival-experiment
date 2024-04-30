#Introduction----

#Date: Wednesday November 22nd 2023
#Author: Jacob Houtman

#I wrote this code with guidance from Emma Atkinson for the paper INSERT TITLE by Emma Atkinson, myself and Dr. Mark Lewis.
#We analyzed data collected by Emma Atkinson, INSERT FIELD TECH NAMES on how prawns survive when
#they are left out of water for different amounts of time.

#We used a GLMM to understand the influence of time out of water, temperature and body length 
#on the probability of a prawn surviving the treatment. The random effect was based on the 123 traps that a prawn could have been 
#in during the experiment and captures differences in traps like location, presence of predators, and trap orientation.

#In this document I combine multiple spreadsheets and remove bad data. 

# Loading packages----
library(tidyverse)
library(here) # this package allows you to reproducibly set your directory

# Set working directory and read in data----
setwd(here("data-raw"))

##The trial data files contains trial level information on our experiment 
##Each of the files contains data collected by one observer
tdata_EA<-read.csv("prawnz_experiment_trial_data_entry_EA.csv")
tdata_KF<-read.csv("prawnz_experiment_trial_data_entry_KF - prawnz_experiment_trial_data_entry.csv")
tdata_KM<-read.csv("prawnz_experiment_trial_data_entry_KM - prawnz_experiment_trial_data_entry.csv")
tail(tdata_KM)

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


#Create an 'observer' column----
#This creates a new column for the observer that entered the data
rdata_EA["Person"]<-rep("EA",nrow(rdata_EA))
rdata_KF["Person"]<-rep("KF",nrow(rdata_KF))
rdata_KM["Person"]<-rep("KM",nrow(rdata_KM))

sdata_EA["Person"]<-rep("EA",nrow(sdata_EA))
sdata_KF["Person"]<-rep("KF",nrow(sdata_KF))
sdata_KM["Person"]<-rep("KM",nrow(sdata_KM))

tdata_EA["Person"]<-rep("EA",nrow(tdata_EA))
tdata_KF["Person"]<-rep("KF",nrow(tdata_KF))
tdata_KM["Person"]<-rep("KM",nrow(tdata_KM))


#Combine data----

#Merge dataframes collected by each observer into one.
rdata_total<-rbind(rdata_EA, rdata_KF, rdata_KM)
tdata_total<-rbind(tdata_EA, tdata_KF, tdata_KM)
sdata_total<-rbind(sdata_EA, sdata_KF, sdata_KM)

#Create a "trial-trap" column----
##This creates an individual ID column. The column prawn_id gives the prawn number for that
##prawn in that trial. It starts at 1 for the first prawn entered in the data and 
## gos up to n (the number of prawns in that trial) for the last prawn. All_id includes
## the trial and trap as well so it is 
sdata_total$All_id<-paste(sdata_total$trial_number,"-",sdata_total$trap_number,"-",sdata_total$prawn_id)
rdata_total$All_id<-paste(rdata_total$trial_number,"-",rdata_total$trap_number,"-",rdata_total$prawn_id)


#Change GPS coordinates to latitude and longitude----

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


#Remove two trials----

#We decided to remove the data from two trials. We stopped trial 12 in the field because there
#was a freshet which resulted in many dead prawns. There is no data for trial 12 as a result.

nrow(sdata_total[which(sdata_total$trial_number==12),])
nrow(rdata_total[which(rdata_total$trial_number==12),])

#We did however keep the trial information for trial 12.
#This line removes trial twelve
tdata_total_2<-tdata_total[which(tdata_total$trial_number!=12),]

#We removed trial 11 because EMMMA.
#The data from trial 11 cn be viewed here. 
sdata_total[which(sdata_total$trial_number==11),]
tdata_total_2[which(tdata_total_2$trial_number==11),]

#This removes trial 11 from the data set. 
rdata_total_2<-rdata_total[which(rdata_total$trial_number!=11),]
sdata_total_2<-sdata_total[which(sdata_total$trial_number!=11),]
tdata_total_3<-tdata_total_2[which(tdata_total_2$trial_number!=11),]


##Data cleaning----
#Record which rows have missing data
bad_rows<-c(which(is.na(sdata_total_2$dead+sdata_total_2$alive+sdata_total_2$scavenged)),
            which(is.na(sdata_total_2$trap_number)))

#Look at bad rows
sdata_total_2[bad_rows,]

#This NA entry was a mistake and should be 0
sdata_total_2[bad_rows[1],]$scavenged<-0

#The other two rows are missing data 
sdata_total_3<-sdata_total_2[-bad_rows[2:3],]

#There is an NA in trial data 
bad_rows<-which(is.na(tdata_total_3$X2h_number))

#This should be a 0 not NA 
tdata_total_3[bad_rows,]$X2h_number<-0


#Create a new column for sums of reflex data----
rdata_total_2$score<-rowSums(rdata_total_2[6:15])


#Change working directory to desired location for new files----
setwd(here("data-clean"))

##Save clean data-frames to csv files.
write.csv(rdata_total_2,"2024-04-19-EMA_prawn_combined_reflex_data.csv")
write.csv(tdata_total_3,"2024-04-19-EMA_prawn_combined_trial_data.csv")
write.csv(sdata_total_3,"2024-04-19-EMA_prawn_combined_survival_data.csv")
