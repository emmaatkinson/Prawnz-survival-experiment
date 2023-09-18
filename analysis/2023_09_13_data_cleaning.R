##This code shows the data-cleaning counducted for the following paper: 'Prawn out of water: survival of Pandalus 
#platyceros following air exposure' by Emma Atkinson, Jacob Houtman, and Dr. Mark Lewis.

#Author: Jacob Houtman
#Date: September 13th 2023 

#rm(list=ls())
#graphics.off()

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

#set working directory
setwd(here("data-clean"))

#read in data
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

#unbanded prawns have an NA value for treatment.
#this gives two subsets with only banded prawns and only unbanded prawns,respectively
banded_survival<-survival[!is.na(survival$treatment),]
unbanded_survival<-survival[is.na(survival$treatment),]

#T test comparing banded and unbanded 
t.test(unbanded_survival$length,banded_survival$length)
var(unbanded_survival$length,na.rm=T)

#Chi Square test comparing number of scavenged prawns in each treatment
no_length<-subset(survival,is.na(survival$length))
yes_length<-subset(survival,!is.na(survival$length))

no_length<-no_length[!is.na(no_length$treatment),]
yes_length<-yes_length[!is.na(yes_length$treatment),]

nrow(no_length)+nrow(yes_length)


chisq.test(t(data.frame(no_length=hist(no_length$treatment, breaks =c(0,15,45,75,95,105,125))$counts,
                      yes_length =hist(yes_length$treatment, breaks =c(0,15,45,75,95,105,125))$counts))
)

##DELETE?----
#Read data in
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

#Order trial dataframe
trial<-trial[order(trial$trial_number),]
temp<-trial$exp_set_temp_air

#
model_df<-survival[(is.na(survival$treatment)==FALSE),]
model_df$treatment<-as.integer(model_df$treatment)
model_df<-model_df[order(model_df$trial_number,model_df$prawn_id),]
model_df$trial_trap<-paste(model_df$trial_number,"-",model_df$trap_number)


model_df$temp<-rep(0, nrow(model_df_1))
n_trials<-nrow(trial)
for(i in 1:n_trials){
  if(i<11){
    model_df_1[which(model_df_1$trial_number==i),]$temp<-rep(temp[i],length(which(model_df_1$trial_number==i)))
  }
  if(i>=11){
    model_df_1[which(model_df_1$trial_number==i+2),]$temp<-rep(temp[i],length(which(model_df_1$trial_number==i+2)))
  }
}
unique(model_df_1$temp)
temp
model_df_1$trial_trap<-as.factor(model_df_1$trial_trap)
model_df[1255,]

model_df_1$length<-round(model_df_1$length/0.5)*0.5

mean(model_df_1$length, na.rm=TRUE)
install.packages("glmmTMB")
install.packages("lme4")

library("glmmTMB")
library("lme4")

model_df_1
model_df_2<-model_df_1[is.na(model_df_1$length)==FALSE,]
unique(model_df_2$temp)
trial$exp_set_temp_air
nrow(model_df_2)

##STOP DELETING ----
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

















##FIGURES----
#set working directory for figures
setwd(here("figures"))

#make upcoming figure into png
png(paste(Sys.Date(), "Banded_Unbanded_length_boxplot.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

#boxplot compari
boxplot(banded_survival$length,unbanded_survival$length, xlab= "Banded vs Unbanded", names = c("Banded", "Unbanded"))

#png complete
dev.off()

##Theoretical survival experiment----

## Survival was measured based on prawns remaining in the trap after the experiment.
## If either dead or alive prawns were more likely to be lost, our survival estimates 
## may have been biased.

## There are a range of possible bias scenarios. The two most extreme cases would be
## if only dead prawns were lost, and if only alive prawns were lost. The following code
## shows how different bias scenarios influence the difference between the observed 
## survival and the true survival.

#treatments are the possible times out of water a prawn might have experienced
treatments<-c(0,30,60,90,120)

#total.prawns represents the number of prawns in each trial.
total.prawns<-c(100,100,100,100,100)

#true.alive represents the number of living prawns after the trial.
true.alive<-c(90,80,60,40,10)

#dead.lost provides the percentage of observed prawns that lived, given the
#true survival (i.e. of lost and recovered prawns) and percent of dead prawns lost, 
#under the assumption only dead prawns are lost

dead.lost<-function(totals,true.alive, percent){
  
  #total prawns minus living prawns gives dead prawns
  true.dead=totals-true.alive
  
  #lost is the number of prawns lost, calculated as dead prawns times probability of losing a dead prawn
  #(the probability of losing a living prawn are 0 in this scenario)
  lost=true.dead*percent
  
  #observed.alive is the observed percent of prawns that survived
  observed.alive=100*true.alive/(totals-lost)
  
  return(observed.alive)
}

#alive.lost provides the percentage of observed prawns that lived, given the
#true survival (i.e. of lost and recovered prawns) and percent of alive prawns lost, 
#under the assumption only dead prawns are lost

alive.lost<-function(totals,true.alive, percent){
  
  #lost is the true number of living prawns times the probability of losing a living prawn 
  #(probability of losing a dead prawn are 0 in this scenario)
  lost=true.alive*percent
  
  #observed.alive is the observed percent of prawns that survived
  observed.alive=100*(true.alive-lost)/(totals-lost)
  
  return(observed.alive)
}

#equal.lost provides the percentage of observed prawns that lived, given the
#true survival (i.e. of lost and recovered prawns) and percent of prawns lost, 
#under the assumption living and dead prawns have equal likelihood of being lost

equal.lost<-function(totals,true.alive, percent){
  
  #true.dead is the total prawns minus the number of living ones
  true.dead=totals-true.alive
  
  #lost.alive is the true number of living prawns times the probability of losing a living prawn 
  lost.alive=true.alive*percent
  
  #lost.dead is the true number of dead prawns times the probability of losing a dead prawn 
  lost.dead=true.dead*percent
  
  #total lost is the number of lost dead prawns plus the number of lost living prawns
  lost=lost.alive+lost.dead
  
  #observed.alive is the observed percent of prawns that survived
  observed.alive=100*(true.alive-lost.alive)/(totals-lost)
  return(observed.alive)
}

#Save upcoming figures as a png
setwd(here("figures"))
png(paste(Sys.Date(), "lost_bias_20.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(3,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

#Plot showing true surivival vs observed survival at different treatments (times out of water)
#assuming only dead prawns are lost
plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when 20% of dead are lost")
legend(x=80,y=90,c("True survival","Observed survival"), pch=c(1,2), cex=0.7)
points(treatments,dead.lost(total.prawns,true.alive,0.2), pch=2)

#Plot showing true surivival vs observed survival at different treatments (times out of water)
#assuming only living prawns are lost
plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when 20% of alive are lost")
legend(x=80,y=90,c("True survival","Observed survival"), pch=c(1,2), cex=0.7)
points(treatments,alive.lost(total.prawns,true.alive,0.2), pch=3)

#Plot showing true surivival vs observed survival at different treatments (times out of water)
#assuming dead and alive prawns are lost at an equal frequency
plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when loss (20%) is unbiased")
legend(x=80,y=90,c("True survival","Observed survival"), pch=c(1,2), cex=0.7)
points(treatments,equal.lost(total.prawns,true.alive,0.2), pch=4)

#Stop saving figures as a png
dev.off()

citation()
##DELETE \/


plot(NULL, xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival with different loss biases")
legend(x=80,y=1,c("True survival","Dead lost","Alive lost","Equal lost"), pch=c(1,2,3,4), cex=0.5)
points(c(0, 60, 120), y=c(0.9, 0.6,0.1))
points(c(0, 60, 120), y=c(0.92, 0.65,0.12), pch = 2)
points(c(0, 60, 120), y=c(0.88, 0.56,0.082), pch=3)
points(c(0, 60, 120), y=c(0.9, 0.6,0.1), pch=4)


