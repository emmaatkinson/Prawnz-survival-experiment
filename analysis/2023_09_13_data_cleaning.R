##This code shows the data-cleaning counducted for the following paper: 'Prawn out of water: survival of Pandalus 
#platyceros following air exposure' by Emma Atkinson, Jacob Houtman, and Dr. Mark Lewis.

#Author: Jacob Houtman
#Date: September 13th 2023 

#rm(list=ls())
#graphics.off()

# Loading packages ----
library(tidyverse)
library(here) # this package allows you to reproducibly set your directory
getwd()
# Set WD ----
setwd(here("data-raw"))

#Read in raw data----
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


#Change Trial coordinate units ----
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


#Add 'Observer' column and 'All ID' coloumn----
##Add a coloumn to indicate the observer that collected the data
rdata_EA["Observer"]<-rep("EA",nrow(rdata_EA))
rdata_KF["Observer"]<-rep("KF",nrow(rdata_KF))
rdata_KM["Observer"]<-rep("KM",nrow(rdata_KM))

sdata_EA["Observer"]<-rep("EA",nrow(sdata_EA))
sdata_KF["Observer"]<-rep("KF",nrow(sdata_KF))
sdata_KM["Observer"]<-rep("KM",nrow(sdata_KM))

tdata_EA["Observer"]<-rep("EA",nrow(tdata_EA))
tdata_KF["Observer"]<-rep("KF",nrow(tdata_KF))
tdata_KM["Observer"]<-rep("KM",nrow(tdata_KM))

##This creates an individual ID column. The column prawn_id gives the prawn number for that
##prawn in that trial. It starts at 1 for the first prawn entered in the data and 
## gos up to n (the number of prawns in that trial) for the last prawn. All_id includes
## the trial and trap as well so it is 
sdata_total$All_id<-paste(sdata_total$trial_number,"-",sdata_total$trap_number,"-",sdata_total$prawn_id)
rdata_total$All_id<-paste(rdata_total$trial_number,"-",rdata_total$trap_number,"-",rdata_total$prawn_id)

#Merge dataframes collected by each observer into one----
rdata_total<-rbind(rdata_EA, rdata_KF, rdata_KM)
tdata_total<-rbind(tdata_EA, tdata_KF, tdata_KM)
sdata_total<-rbind(sdata_EA, sdata_KF, sdata_KM)


#Remove two trials----
##This removes two trials, 11 and 12, from the data set.
rdata_total_2<-rdata_total[which(rdata_total$trial_number!=11),]
tdata_total_2<-tdata_total[which(tdata_total$trial_number!=11),]
sdata_total_2<-sdata_total[which(sdata_total$trial_number!=11),]

rdata_total_3<-rdata_total_2[which(rdata_total_2$trial_number!=12),]
tdata_total_3<-tdata_total_2[which(tdata_total_2$trial_number!=12),]
sdata_total_3<-sdata_total_2[which(sdata_total_2$trial_number!=12),]


#Removing bad rows----
#Store any rows with bad data 
bad_rows<-c(which(is.na(sdata_total_3$dead+sdata_total_3$alive+sdata_total_3$scavenged)),
            which(is.na(sdata_total_3$trap_number)))

#View bad rows
sdata_total_3[bad_rows,]

#Change false NA to 0 (wrong entry)
sdata_total_3[bad_rows[1],]$scavenged<-0

#Remove bad rows
sdata_total_4<-sdata_total_3[-bad_rows[2:3],]

#clean trial data
#Store bad rows of data 
bad_rows<-which(is.na(tdata_total_3$X2h_number))

#Change false NA to 0
tdata_total_3[bad_rows,]$X2h_number<-0

#Create new column in reflex data which is sum of all the specific reflexes
names(rdata_total_3)[6:15]
rdata_total_3$score<-rowSums(rdata_total_3[6:15])

##Change working directory to desired location for new files
setwd(here("data-clean"))

##Save clean data-frames to csv files.
write.csv(rdata_total_3,"2023-05-09_prawn_combined_reflex_data.csv")
write.csv(tdata_total_3,"2023-05-09_prawn_combined_trial_data.csv")
write.csv(sdata_total_4,"2023-05-09_prawn_combined_survival_data.csv")


#Make Model Dataframe----
#Read in data 
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

#Order trial dataframe by trial number 
trial<-trial[order(trial$trial_number),]

#The air temperature is recorded in the trial dataset
temp<-trial$exp_set_temp_air

#Remove unbanded prawns 
model_df<-survival[(is.na(survival$treatment)==FALSE),]

#Change treatment from character to integer
model_df$treatment<-as.integer(model_df$treatment)

#Sort model_df by trial then by prawn id
model_df<-model_df[order(model_df$trial_number,model_df$prawn_id),]

##Length cleaning----

## Length measurements were taken to the hundredth of a millimetre (10^-5 m) using callipers.
## However the measurements were less precise than that in practice.
## To account for this I round up the lengths to the nearest half millimetre.
model_df$length<-round(model_df$length/0.5)*0.5

## Removing length NA's 
model_df<-model_df[is.na(model_df$length)==FALSE,]

##Make random effect levels----

## The trial_trap column is the number referring to the trial and trap that the prawn was in.
## Trial_number is which one of the 21 trials the prawn was in. Trap_number is the number (1-6) that the
##prawn was in. However there are 21 different traps labelled with trap_number=1, because the trap numbers repeat between trials.
##I need the model to know to the specific trap a prawns was in (i.e. a trap ID that doesn't repeat accross trials), 
##for our random effect levels.

##All of the trials included 6 traps, except for #14 which had 3 traps.
##Trial_trap as defined below gives each of the 123 traps (20 trials times 6 traps per trial plus 3= 123)
##a specific ID containing the trial number and trap number as a string.
#Make a grouping factor for the random effect
model_df$trial_trap<-as.factor(paste(model_df$trial_number,"-",model_df$trap_number))

#Add new temperature column 
model_df$temp<-rep(0, nrow(model_df))

#The 11th to 21st rows of the trial data set now correspond to 13th to 23rd 
trial$trial_number

##This loop adds the temperature at which each prawn was hauled to the data set.
## The 'if' branch is to deal with the fact the the 11th-21st elements of temp_scale
##correspond to the 13th-23rd trials because we omitted trials 11 and 12.
n_trials<-nrow(trial)

#Add corresponding temperature to temp column
for(i in 1:n_trials){
  
  #from trial 1 to 10 the trial number corresponds to entry 1 to 10 in temperature vector
  if(i<11){
    #this line adds the air temperature the prawn was exposed to the model_df
    model_df[which(model_df$trial_number==i),]$temp<-rep(temp[i],length(which(model_df$trial_number==i)))
  }
  
  #Because of the two excluded trials (number 11 and 12), trial numbers 13 to 23 
  #correspond to elements 11 to 21 in the 'temp' vector
  if(i>=11){
    #assign the temperature for trial numbers 13 to 23, to the 11th to 21st values in the temp vector
    model_df[which(model_df$trial_number==i+2),]$temp<-rep(temp[i],length(which(model_df$trial_number==i+2)))
  }
}

## Write CSV----
setwd(here("data-clean"))
write.csv(model_df,"2023_08_10_model_dataframe")