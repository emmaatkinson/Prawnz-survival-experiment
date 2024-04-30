# Date created: 22-Nov-2023
# Last updated: 29-Apr-2024
# Authors: Jacob Houtman & Emma Atkinson
# Description: Prawn survival experiment (data prep)
# Notes: This code reads in raw trial, survival, and reflex data and prepares it 
#        for analysis and generating figures. 
#
################################################################################

# Load packages #
library(tidyverse)
library(here) 

# Set working directory and read in data #
setwd(here("data-raw"))

################################################################################
#   
#   Part one: Re-format data
#
#   *Read in raw data sheets (one version of each sheet per data enterer)and
#    collate together reformatted versions.
#   *Filter out unuseable trials.   
#
################################################################################

# The trial data files contains trial level information from the experiment 
# Read in separate data entry spreadsheets (one per data enterer)
tdata_EA<-read.csv("prawnz_experiment_trial_data_entry_EA.csv")
tdata_KF<-read.csv("prawnz_experiment_trial_data_entry_KF - prawnz_experiment_trial_data_entry.csv")
tdata_KM<-read.csv("prawnz_experiment_trial_data_entry_KM - prawnz_experiment_trial_data_entry.csv")
tail(tdata_KM)

# The survival data files contains individual level survival information about each prawn
# Read in separate data entry spreadsheets (one per data enterer)
sdata_EA<-read.csv("prawnz_experiment_survival_data_entry_EA.csv")
sdata_KF<-read.csv("prawnz_experiment_survival_data_entry_KF - prawnz_experiment_survival_data_entry.csv")
sdata_KM<-read.csv("prawnz_experiment_survival_data_entry_KM - prawnz_experiment_survival_data_entry.csv")
head(sdata_EA)

# The reflex data files contains individual level reflex information about the prawns that survive
# Read in separate data entry spreadsheets (one per data enterer)
rdata_EA<-read.csv("prawnz_experiment_reflex_data_entry_EA.csv")
rdata_KF<-read.csv("prawnz_experiment_reflex_data_entry_KF - prawnz_experiment_reflex_data_entry.csv")
rdata_KM<-read.csv("prawnz_experiment_reflex_data_entry_KM - prawnz_experiment_reflex_data_entry.csv")
head(rdata_EA)

# Create a column to keep track of who entered which data
rdata_EA["Person"]<-rep("EA",nrow(rdata_EA))
rdata_KF["Person"]<-rep("KF",nrow(rdata_KF))
rdata_KM["Person"]<-rep("KM",nrow(rdata_KM))

sdata_EA["Person"]<-rep("EA",nrow(sdata_EA))
sdata_KF["Person"]<-rep("KF",nrow(sdata_KF))
sdata_KM["Person"]<-rep("KM",nrow(sdata_KM))

tdata_EA["Person"]<-rep("EA",nrow(tdata_EA))
tdata_KF["Person"]<-rep("KF",nrow(tdata_KF))
tdata_KM["Person"]<-rep("KM",nrow(tdata_KM))

# Collate data
# Merge dataframes entered by each person into one
rdata_total<-rbind(rdata_EA, rdata_KF, rdata_KM)
tdata_total<-rbind(tdata_EA, tdata_KF, tdata_KM)
sdata_total<-rbind(sdata_EA, sdata_KF, sdata_KM)

# Create a "trial-trap" column to use as a random effect in fitted models
# This creates an individual ID column. The column 'prawn_id' gives the prawn number for that
# prawn in that trial. It starts at 1 for the first prawn entered in the data and 
# goes up to n (the number of prawns in that trial) for the last prawn. All_id includes
# the trial and trap as well as individual prawn ID.
sdata_total$All_id<-paste(sdata_total$trial_number,"-",sdata_total$trap_number,"-",sdata_total$prawn_id, sep="")
rdata_total$All_id<-paste(rdata_total$trial_number,"-",rdata_total$trap_number,"-",rdata_total$prawn_id, sep="")


# Cconvert GPS coordinates to latitude and longitude

# Trial data includes lat and long coordinates at which each trap was set and hauled
# However, the data is in degrees, and seconds. This code converts those units into
# degrees and decimals of degrees. It does this by removing the first few characters, 
# which are the whole degrees and and the decimal. The remainder is arc seconds divided
# 60 seconds to convert it into hundredths of degrees.
tdata_total$exp_set_lat_1<-(50+as.numeric(sub('...', '', tdata_total$exp_set_lat_1))/60)
tdata_total$exp_set_lat_2<-(50+as.numeric(sub('...', '', tdata_total$exp_set_lat_2))/60)
tdata_total$exp_set_lon_1<-(126+as.numeric(sub('....', '', tdata_total$exp_set_lon_1))/60)
tdata_total$exp_set_lon_2<-(126+as.numeric(sub('....', '', tdata_total$exp_set_lon_2))/60)
tdata_total$exp_haul_lat_1<-(50+as.numeric(sub('...', '', tdata_total$exp_haul_lat_1))/60)
tdata_total$exp_haul_lat_2<-(50+as.numeric(sub('...', '', tdata_total$exp_haul_lat_2))/60)
tdata_total$exp_haul_lon_1<-(126+as.numeric(sub('....', '', tdata_total$exp_haul_lon_1))/60)
tdata_total$exp_haul_lon_2<-(126+as.numeric(sub('....', '', tdata_total$exp_haul_lon_2))/60)


# Filter out two trials in which prawns were exposed to low salinity due to unanticipated
# freshet. We filtered trials #11 and #12 because the prawns were kept in a total of low
# salinity water (<25 ppt) and thus experienced confounding mortality due to fresh water exposure.
nrow(sdata_total[which(sdata_total$trial_number==12),])
nrow(rdata_total[which(rdata_total$trial_number==12),])

# We did however keep the trial information for trial 12.
# This line removes trial twelve
tdata_total_2<-tdata_total[which(tdata_total$trial_number!=12),]

sdata_total[which(sdata_total$trial_number==11),]
tdata_total_2[which(tdata_total_2$trial_number==11),]

# This removes trial 11 from the data set. 
rdata_total_2<-rdata_total[which(rdata_total$trial_number!=11),]
sdata_total_2<-sdata_total[which(sdata_total$trial_number!=11),]
tdata_total_3<-tdata_total_2[which(tdata_total_2$trial_number!=11),]

################################################################################
#   
#   Part two: Data cleaning
#
################################################################################

## Data cleaning
## Record which rows have missing data
bad_rows<-c(which(is.na(sdata_total_2$dead+sdata_total_2$alive+sdata_total_2$scavenged)),
            which(is.na(sdata_total_2$trap_number)))

# Look at bad rows
sdata_total_2[bad_rows,]

# This NA entry was a mistake and should be 0
sdata_total_2[bad_rows[1],]$scavenged<-0

# The other two rows are missing data 
sdata_total_3<-sdata_total_2[-bad_rows[2:3],]

# There is an NA in trial data 
bad_rows<-which(is.na(tdata_total_3$X2h_number))

# This should be a 0 not NA 
tdata_total_3[bad_rows,]$X2h_number<-0

# Create a new column for sums of reflex data
rdata_total_2$score<-rowSums(rdata_total_2[6:15])

# Change working directory to desired location for new files
setwd(here("data-clean"))

# Save clean data-frames to csv files (for figure making)
write.csv(rdata_total_2,"2024-04-30-EMA_prawn_combined_reflex_data.csv")
write.csv(tdata_total_3,"2024-04-30-EMA_prawn_combined_trial_data.csv")
write.csv(sdata_total_3,"2024-04-30-EMA_prawn_combined_survival_data.csv")

################################################################################
#   
#   Part three: Prepare a data frame for model-fitting
#
################################################################################

# Preparing dataframe for model-fitting 
survival = sdata_total_3
trial = tdata_total_3

# Make new column to store temperature 
survival$temp<-rep(0, nrow(survival))

# Pull temperature from trial dataframe
for (i in unique(survival$trial_number)){
  
  survival[survival$trial_number==i,]$temp = trial[trial$trial_number==i,]$exp_set_temp_air

}

# Filter out missing data

# Some prawns were missing data, either length (due to carapace damage) or treatment (due to losing their band)
# Some of the code below requires a dataset with no NAs.

# Remove length NAs
no_length_nas<-survival[which(!is.na(survival$length)),]

# Remove unbanded (Treatment NAs)
no_unbanded<-survival[which(!is.na(survival$treatment)),]

# Remove both unbanded and length NAs
model_df_2<-no_unbanded[which(!is.na(no_unbanded$length)),]

# Create trial x trap column for the random effect
# There were 20 trials with 6 traps and one trial with 3 traps
# That means that there were 123 traps a prawn could have been in.
# The trap_number column only goes from 1-6, and then repeats for each trial.
# However traps labelled "5" for different trials were unrelated.
# This line creates a new column with 123 unique names for each trial x trap combination
model_df_2$trial_trap<-paste(model_df_2$trial_number,model_df_2$trap_number,sep="-")

#Change column to a factor
model_df_2$trial_trap<-as.factor(model_df_2$trial_trap)

# Save model dataframe
# The models we fit in another document need no missing data.
setwd(here("data-clean"))
write.csv(model_df_2,"2024-04-29_model_dataframe.csv")

