
#MODEL FITTING TIME 
library(here)
setwd(here("data-clean"))

#Read in dataframe 
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")


#Order trial dataframe
trial<-trial[order(trial$trial_number),]

#
model_df<-survival[(is.na(survival$treatment)==FALSE),]
model_df$treatment<-as.integer(model_df$treatment)
model_df<-model_df[order(model_df$trial_number,model_df$prawn_id),]
model_df$trial_trap<-paste(model_df$trial_number,"-",model_df$trap_number)
unique(model_df$trial_trap)

model_df_1<-model_df[c(-564,-1255),]
unique(model_df_1$trial_trap)
model_df_1$temp<-rep(0, nrow(model_df_1))
n_trials<-nrow(trial)
temp_dummy<-(trial$exp_set_temp_air-mean(trial$exp_set_temp_air))/sqrt(var(trial$exp_set_temp_air))
for(i in 1:n_trials){
  if(i<11){
    model_df_1[which(model_df_1$trial_number==i),]$temp<-rep(temp_dummy[i],length(which(model_df_1$trial_number==i)))
  }
  if(i>=11){
    model_df_1[which(model_df_1$trial_number==i+2),]$temp<-rep(temp_dummy[i],length(which(model_df_1$trial_number==i+2)))
  }
}

model_df_1$trial_trap<-as.factor(model_df_1$trial_trap)


model_df_1$length<-round(model_df_1$length/0.5)*0.5
model_df_1$length<-(model_df_1$length-mean(model_df_1$length, na.rm=TRUE))/sqrt(var(model_df_1$length, na.rm = TRUE))


mean(model_df_1$length, na.rm=TRUE)
install.packages("glmmTMB")
install.packages("lme4")

library("glmmTMB")
library("lme4")
summary(model_ttl1)
summary(model_big2)
#Intercept only model
model_null1<-lme4::glmer(alive~(1|trial_trap),data=model_df_1,family=binomial)
model_null2<-glmmTMB::glmmTMB(alive~(1|trial_trap),data=model_df_1,family=binomial)

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




install.packages("MuMIn") # needed for BIC weights

# Get BIC values for each mixed-effects model
BIC.values <- BIC(model_null2, model_treat2, model_temp2, model_length2, model_tt2,model_tti2, model_ttl2,model_big2)

# Shorthand names for models (in the same order as in BIC.values
BIC.names <- c("Null", "Treatment", "Temperature", "Length","Treatment + Temperature", "Treatment + Temperature + Treatment x Temperature","Treatment + Temperature + Length","Treatment + Temperature + Length + Treatment x Length + Temperature x Length")

# Order the BIC and model name vectors by ascending order of the AIC vector elements
BIC.order <- order(BIC.values$BIC)
BIC.names <- BIC.names[BIC.order]
BIC.values <- BIC.values$BIC[BIC.order]

# Calculate BIC weights 
BIC.weights <- MuMIn::Weights(BIC.values)

# Calculate BIC cumulative weights
BIC.cumul.weights <- numeric(length(BIC.weights))
BIC.cumul.weights[1] <- BIC.weights[1]
for (i in 2:length(BIC.weights)) {
  BIC.cumul.weights[i] <- BIC.cumul.weights[i-1] + BIC.weights[i]
}

# Calculate maximum log likelihood values and re-order
BIC.logLik <- c(logLik(model_null2), logLik(model_length2), logLik(model_temp2), logLik(model_treat2), logLik(model_tt2),logLik(model_tti2),logLik(model_ttl2),logLik(model_big2))
BIC.logLik <- BIC.logLik[BIC.order]


# Create delta BIC function and calculate for each model relative to top model
deltaFcn = function(listBIC) {
  deltas = -(min(listBIC) - listBIC)
}
BIC.deltas = deltaFcn(BIC.values)

# Create BIC table
BIC.table <- data.frame(
  model.name = BIC.names, BIC=BIC.values,
  deltaBIC = round(BIC.deltas,2),
  weight = round(BIC.weights,3),
  cumul.weight = round(BIC.cumul.weights,3),
  logLik = round(BIC.logLik,2))
# Save BIC table #
setwd(here("figures"))
write.csv(BIC.table, paste(Sys.Date(),"Prawn_Survival_BIC_table.csv"))


















