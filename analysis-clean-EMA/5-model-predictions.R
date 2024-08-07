### Producing model predictions for main text results
### For now, all model estimates are coming from top model (lme4 version)

library(here)
library(lme4)
library(TMB)
library(DescTools)
library(broom.mixed)
library(dotwhisker)
library(effects)

rm(list=ls())

# Read in data #
reflexes<-read.csv(here("data-clean","2023-05-09_prawn_combined_reflex_data.csv"))
survival<-read.csv(here("data-clean","2023-05-09_prawn_combined_survival_data.csv"))
trial<-read.csv(here("data-clean","2023-05-09_prawn_combined_trial_data.csv"))

model.dat = read.csv(here("data-clean","2024-04-29_model_dataframe.csv"))

# Read in model
m1<-readRDS(here("model-outputs-EMA","treat-temp_temp-length_lme4.rds"))
m2<-readRDS(here("model-outputs-EMA","treat-temp_temp-length_TMB.rds"))

# Read in predictions
p1<-read.csv(here("model-outputs-EMA","2024-04-29-predictions-lme4.csv"))
p2<-read.csv(here("model-outputs-EMA","2024-04-29-predictions-TMB.csv"))

# function to solve for the time out of water for which model-predicted survival crosses 50%

treat50 = function(m, dat, length, temp){
  
  b0_int=summary(m1)$coefficients[1,1]
  b1_temp=summary(m1)$coefficients[2,1]
  b2_length=summary(m1)$coefficients[3,1]
  b3_treat=summary(m1)$coefficients[4,1]
  b4_temp_length=summary(m1)$coefficients[5,1]
  b5_temp_treat=summary(m1)$coefficients[6,1]

  time_for_50 = -(b0_int+(b1_temp*temp)+(b2_length*length)+(b4_temp_length*temp*length))/(b3_treat+(b5_temp_treat*temp))

  return(time_for_50)
  
}

treat50(m=m2, dat=model.dat, length=32, temp=10.7)
treat50(m=m2, dat=model.dat, length=32, temp=25.7)

# extracting predictions for immediate release survival 

#35 mm prawn, low/high temp
p1[p1$treatment==0 & p1$length==32 & p1$temp==11,]
p1[p1$treatment==0 & p1$length==32 & p1$temp==26,]

#23 mm prawn, low/high temp
p1[p1$treatment==0 & p1$length==23 & p1$temp==11,]
p1[p1$treatment==0 & p1$length==23 & p1$temp==26,]

#23 mm prawn, low/high temp
p1[p1$treatment==0 & p1$length==39 & p1$temp==11,]
p1[p1$treatment==0 & p1$length==39 & p1$temp==26,]


### coefficient plot
glance(m1)
tidy(m1)

m1tidy = tidy(m1, conf.int=TRUE)
