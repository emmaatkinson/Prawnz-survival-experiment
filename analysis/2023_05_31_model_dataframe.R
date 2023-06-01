
#MODEL FITTING TIME 
install.packages("here")
library(here)
setwd(here("data-clean"))
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")
trial<-trial[order(trial$trial_number),]
trial$trial_number
trial$exp_set_temp_air
model_df<-survival[(is.na(survival$treatment)==FALSE),]
model_df$treatment<-as.integer(model_df$treatment)
model_df<-model_df[order(model_df$trial_number,model_df$prawn_id),]
model_df$trial_trap<-6*(model_df$trial_number-1)+model_df$trap_number
model_df[which(model_df$trial_number>11),]$trial_trap<-model_df[which(model_df$trial_number>11),]$trial_trap-12
model_df_1<-model_df[-1255,]
model_df_1$temp<-rep(0, nrow(model_df_1))
for(i in 1:n_trials){
  if(i<11){
  model_df_1[which(model_df_1$trial_number==i),]$temp<-rep(trial$exp_set_temp_air[i],length(which(model_df_1$trial_number==i)))
  }
  if(i>=11){
    model_df_1[which(model_df_1$trial_number==i+2),]$temp<-rep(trial$exp_set_temp_air[i],length(which(model_df_1$trial_number==i+2)))
  }
}
model_df_1$trial_trap<-as.factor(model_df_1$trial_trap)
unique(model_df_1$temp)

model_df_1$length<-round(model_df_1$length/0.5)*0.5

model_df_1$length<-(model_df_1$length-mean(model_df_1$length, na.rm=TRUE))/sqrt(var(model_df_1$length, na.rm = TRUE))
model_df_1$temp<-(model_df_1$temp-mean(model_df_1$temp))/sqrt(var(model_df_1$temp))

install.packages("glmmTMB")
install.packages("lme4")
model_df_1$

#Intercept only model
model_null1<-glmer(alive~(1|trial_trap),data=model_df_1,family=binomial)
model_null2<-glmmTMB(alive~(1|trial_trap),data=model_df_1,family=binomial)

#treatment model
model_treat1<-glmer(alive~treatment+(1|trial_trap),data=model_df_1,family=binomial)
model_treat2<-glmmTMB(alive~treatment+(1|trial_trap),data=model_df_1,family=binomial)

#temp model
model_temp1<-glmer(alive~temp+(1|trial_trap),data=model_df_1,family=binomial)
model_temp2<-glmmTMB(alive~temp+(1|trial_trap),data=model_df_1,family=binomial)

#length model
model_length1<-glmer(alive~length+(1|trial_trap),data=model_df_1,family=binomial)
model_length2<-glmmTMB(alive~length+(1|trial_trap),data=model_df_1,family=binomial)

#treatment and temp model 
model_tt1<-glmer(alive~treatment+temp+(1|trial_trap),data=model_df_1,family=binomial)
model_tt2<-glmmTMB(alive~treatment+temp+(1|trial_trap),data=model_df_1,family=binomial)

#treatment and temp and interaction model 
model_tti1<-glmer(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df_1,family=binomial)
model_tti2<-glmmTMB(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df_1,family=binomial)

#treatment and temp and length model 
model_ttl1<-glmer(alive~treatment+temp+length+(1|trial_trap),data=model_df_1,family=binomial)
model_ttl2<-glmmTMB(alive~treatment+temp+length+(1|trial_trap),data=model_df_1,family=binomial)

#treatment and temp and length and length interactions model
model_big1<-glmer(alive~treatment+temp+length+temp*length+treatment*length+(1|trial_trap),data=model_df_1,family=binomial)
model_big2<-glmmTMB(alive~treatment+temp+length+temp*length+treatment*length+(1|trial_trap),data=model_df_1,family=binomial)





















