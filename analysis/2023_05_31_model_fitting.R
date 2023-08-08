library(MuMIn) 
library(here)

#set working directory
setwd("/Users/jacobhoutman/Downloads/Git Hub/Prawnz-survival-experiment/data-clean")


#Read in dataframe 
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


model_df_1<-model_df[c(-564,-1255),]

model_df_1$temp<-rep(0, nrow(model_df_1))
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

nrow(model_df_2)
#NULL:0

model_0_1<-lme4::glmer(alive~(1|trial_trap),data=model_df_2,family=binomial)
model_0_2<-glmmTMB::glmmTMB(alive~(1|trial_trap),data=model_df_2,family=binomial)


#1 MAIN EFFECT:1

#treatment model
model_1.1_1<-glmer(alive~treatment+(1|trial_trap),data=model_df_2,family=binomial)
model_1.1_2<-glmmTMB(alive~treatment+(1|trial_trap),data=model_df_2,family=binomial)

#temp model
model_1.2_1<-glmer(alive~temp+(1|trial_trap),data=model_df_2,family=binomial)
model_1.2_2<-glmmTMB(alive~temp+(1|trial_trap),data=model_df_2,family=binomial)

#length model
model_1.3_1<-glmer(alive~length+(1|trial_trap),data=model_df_2,family=binomial)
model_1.3_2<-glmmTMB(alive~length+(1|trial_trap),data=model_df_2,family=binomial)


#2 MAIN EFFECTS:2

#treatment and temp model 
model_2.1_1<-glmer(alive~treatment+temp+(1|trial_trap),data=model_df_2,family=binomial)
model_2.1_2<-glmmTMB(alive~treatment+temp+(1|trial_trap),data=model_df_2,family=binomial)

#temp and length model 
model_2.2_1<-glmer(alive~length+temp+(1|trial_trap),data=model_df_2,family=binomial)
model_2.2_2<-glmmTMB(alive~length+temp+(1|trial_trap),data=model_df_2,family=binomial)

#treatment and length model 
model_2.3_1<-glmer(alive~treatment+length+(1|trial_trap),data=model_df_2,family=binomial)
model_2.3_2<-glmmTMB(alive~treatment+length+(1|trial_trap),data=model_df_2,family=binomial)


#3 MAIN EFFECTS:3

#treatment and temp and length 
model_3_1<-glmer(alive~treatment+temp+length+(1|trial_trap),data=model_df_2,family=binomial)
model_3_2<-glmmTMB(alive~treatment+temp+length+(1|trial_trap),data=model_df_2,family=binomial)


# 2 main 1 interaction:4

#Treatment*Temp 
model_4.1_1<-glmer(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)
model_4.1_2<-glmmTMB(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)

#treatment*length
model_4.2_1<-glmer(alive~treatment+length+treatment*length+(1|trial_trap),data=model_df_2,family=binomial)
model_4.2_2<-glmmTMB(alive~treatment+length+treatment*length+(1|trial_trap),data=model_df_2,family=binomial)

#temp*length
model_4.3_1<-glmer(alive~temp+length+temp*length+(1|trial_trap),data=model_df_2,family=binomial)
model_4.3_2<-glmmTMB(alive~temp+length+temp*length+(1|trial_trap),data=model_df_2,family=binomial)


# 3 main 1 interaction:5

#Treatment*Temp +length
model_5.1_1<-glmer(alive~length+treatment+temp+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)
model_5.1_2<-glmmTMB(alive~length+treatment+temp+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)

#treatment*length +temp
model_5.2_1<-glmer(alive~temp+treatment+length+treatment*length+(1|trial_trap),data=model_df_2,family=binomial)
model_5.2_2<-glmmTMB(alive~temp+treatment+length+treatment*length+(1|trial_trap),data=model_df_2,family=binomial)

#temp*length+treatment
model_5.3_1<-glmer(alive~treatment+temp+length+temp*length+(1|trial_trap),data=model_df_2,family=binomial)
model_5.3_2<-glmmTMB(alive~treatment+temp+length+temp*length+(1|trial_trap),data=model_df_2,family=binomial)


#2 interaction:6

#treatment*temp+temp*length
model_6.1_1<-glmer(alive~temp*length+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)
model_6.1_2<-glmmTMB(alive~temp*length+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)

#treatment*temp+treatment*length
model_6.2_1<-glmer(alive~treatment*length+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)
model_6.2_2<-glmmTMB(alive~treatment*length+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)

#treatment*length+temp*length 
model_6.3_1<-glmer(alive~temp*length+treatment*length+(1|trial_trap),data=model_df_2,family=binomial)
model_6.3_2<-glmmTMB(alive~temp*length+treatment*length+(1|trial_trap),data=model_df_2,family=binomial)


# 2-way interactions:7

#treatment*length+temp*length +temp*treatment
model_7_1<-glmer(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df_2,family=binomial)
model_7_2<-glmmTMB(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df_2,family=binomial)







#ANALYSIS AND PLOTTING

back_trans<-function(x){
  return(exp(x)/(1+exp(x)))
}
trans<-function(x){
  return(log(x/(1-x)))
}
back_trans_1<-function(x){
  return(exp(0.3-0.14*x)/(1+exp(0.3-0.14*x)))
}


nrow(model_df_2)

# Get BIC values for each mixed-effects model
BIC.values <- BIC(model_0_2, model_1.1_2, model_1.2_2, model_1.3_2, model_2.1_2, model_2.2_2, model_2.3_2,model_3_2, model_4.1_2, model_4.2_2,model_4.3_2, model_5.1_2, model_5.2_2,model_5.3_2,model_6.1_2, model_6.2_2,model_6.3_2,model_7_2)

# Shorthand names for models (in the same order as in BIC.values
BIC.names <- c("Null", "Treatment", "Temperature", "Length","Treatment + Temperature","Temperature + Length","Treatment + Length","Length + Treatment + Temperature", "Treatment x Temperature","Treatment x Length","Temperature x Length", "Treatment x Temperature + Length","Treatment x Length + Temperature","Temperature x Length + Treatment","Treatment x Temperature + Temperature x Length","Treatment x Temperature + Treatment x Length","Treatment x Length + Temperature x Length", "Treatment x Temperature + Treatment x Length + Temperature x Length")

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
BIC.logLik <- c(logLik(model_0_2), logLik(model_1.1_2), logLik(model_1.2_2), logLik(model_1.3_2), logLik(model_2.1_2), logLik(model_2.2_2), logLik(model_2.3_2),logLik(model_3_2), logLik(model_4.1_2), logLik(model_4.2_2),logLik(model_4.3_2), logLik(model_5.1_2), logLik(model_5.2_2),logLik(model_5.3_2),logLik(model_6.1_2), logLik(model_6.2_2),logLik(model_6.3_2),logLik(model_7_2))
BIC.logLik <- BIC.logLik[BIC.order]

summary(model_6.1_2)
# Create delta BIC function and calculate for each model relative to top model
deltaFcn = function(listBIC) {
  deltas = -(min(listBIC) - listBIC)
}
BIC.deltas = deltaFcn(BIC.values)

# Create BIC table
BIC.table <- data.frame(
  model.name = BIC.names, BIC=BIC.values,
  deltaBIC = round(BIC.deltas,2),
  logLik = round(BIC.logLik,2))
formula(model_big1)

getwd()

# Save BIC table 
setwd("/Users/jacobhoutman/Documents/Git Hub/Prawnz-survival-experiment/figures")
write.csv(BIC.table, paste(Sys.Date(),"Prawn_Survival_BIC_table_noweights.csv"))
BIC.table








