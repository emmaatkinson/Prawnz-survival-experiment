
##In this code I create a new data frame, fit multiple models to data,
##and use BIC to select the best one

##load packages----
library(here)
library(glmmTMB)
library(lme4)
library(MuMIn)


setwd(here("data-clean"))

##Read in data----
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

#Make dataframe----

##Order trial dataframe by trial number 
trial<-trial[order(trial$trial_number),]

##Model_df is the data-frame I will use for the fitting models. 
##I create it here as a the same as our survival data set but 
##with the unbanded prawns removed
model_df<-survival[(is.na(survival$treatment)==FALSE),]

##Treatment (time left out of water in minutes) is changed from a character to an integer
model_df$treatment<-as.integer(model_df$treatment)

##Order the data-frame first by trial number, and second by prawn_id.
##prawn_id ranges from 1 for the first prawn to n (the number of prawns in the trial)
##for each trial
model_df<-model_df[order(model_df$trial_number,model_df$prawn_id),]
model_df<-model_df[c(-564,-1255),]

##Make 
n_traps<-rep(0,23)
for (i in 1:23){n_traps[i]<-length(unique(model_df[model_df$trial_number==i,]$trap_number))}
n_traps

##Make random effect levels----

## The trial_trap column is the number referring to the trial and trap that the prawn was in.
## Trial_number is which one of the 21 trials the prawn was in. Trap_number is the number (1-6) that the
##prawn was in. However there are 21 different traps labelled with trap_number=1, because the trap numbers repeat between trials.
##I need the model to know to the specific trap a prawns was in (i.e. a trap ID that doesn't repeat accross trials), 
##for our random effect levels.

##All of the trials included 6 traps, except for #14 which had 3 traps.
##Trial_trap as defined below gives each of the 123 traps (20 trials times 6 traps per trial plus 3= 123)
##a specific ID containing the trial number and trap number as a string.
model_df$trial_trap<-paste(model_df$trial_number,"-",model_df$trap_number)

##Convert trial_trap to factor so it can be used as a random effect
model_df$trial_trap<-as.factor(model_df$trial_trap)

##Create new column for temperature
model_df$temp<-rep(0, nrow(model_df))

##Create vector of temperatures, ordred by trial
temp<-trial$exp_set_temp_air

##This loop adds the temperature at which each prawn was hauled to the data set.
## The 'if' branch is to deal with the fact the the 11th-21st elements of temp_scale
##correspond to the 13th-23rd trials because we omitted trials 11 and 12.
n_trials<-nrow(trial)

## Loop over the 21 trials
for(i in 1:n_trials){
  
  #Trials 1-10
  if(i<11){
    
    #The temperature values for all prawns in a trial is the air temperature for that trial
    model_df[which(model_df$trial_number==i),]$temp<-rep(temp[i],length(which(model_df$trial_number==i)))
  }
  
  #trials 13-23 (trials 11 and 12 were omitted)
  if(i>=11){
    
    #The temperature values for all prawns in a trial is the air temperature for that trial
    # NOTE: Because trials 11 and 12 were excluded temp[11] refers to the temperature for trial 13
    model_df[which(model_df$trial_number==i+2),]$temp<-rep(temp[i],length(which(model_df$trial_number==i+2)))
     }
}

## Length measurements were taken to the hundredth of a millimetre (10^-5 m) using callipers.
## However the measurements were less precise than that in practice.
## To account for this I round up the lengths to the nearest half millimetre.
model_df$length<-round(model_df$length/0.5)*0.5

## Removing length NA's 
model_df<-model_df[is.na(model_df$length)==FALSE,]

## Write CSV
setwd(here("data-clean"))
write.csv(model_df,"2023_08_10_model_dataframe")

