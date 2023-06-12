
#MODEL FITTING TIME 
library(here)
setwd(here("data-clean"))

#Read in dataframe 
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


model_df_1$length<-round(model_df_1$length/0.5)*0.5

mean(model_df_1$length, na.rm=TRUE)
install.packages("glmmTMB")
install.packages("lme4")

library("glmmTMB")
library("lme4")
summary(model_ttl1)
summary(model_big2)
model_df_1
model_df_2<-model_df_1[is.na(model_df_1$length)==FALSE,]
#Intercept only model
model_null1<-lme4::glmer(alive~(1|trial_trap),data=model_df_2,family=binomial)
model_null2<-glmmTMB::glmmTMB(alive~(1|trial_trap),data=model_df_2,family=binomial)

#treatment model
model_treat1<-glmer(alive~treatment+(1|trial_trap),data=model_df_2,family=binomial)
model_treat2<-glmmTMB(alive~treatment+(1|trial_trap),data=model_df_2,family=binomial)

#temp model
model_temp1<-glmer(alive~temp+(1|trial_trap),data=model_df_2,family=binomial)
model_temp2<-glmmTMB(alive~temp+(1|trial_trap),data=model_df_2,family=binomial)

#length model
model_length1<-glmer(alive~length+(1|trial_trap),data=model_df_2,family=binomial)
model_length2<-glmmTMB(alive~length+(1|trial_trap),data=model_df_2,family=binomial)

#treatment and temp model 
model_tt1<-glmer(alive~treatment+temp+(1|trial_trap),data=model_df_2,family=binomial)
model_tt2<-glmmTMB(alive~treatment+temp+(1|trial_trap),data=model_df_2,family=binomial)

#temp and length model 
model_templ1<-glmer(alive~length+temp+(1|trial_trap),data=model_df_2,family=binomial)
model_templ2<-glmmTMB(alive~length+temp+(1|trial_trap),data=model_df_2,family=binomial)

#treatment and length model 
model_treatl1<-glmer(alive~treatment+length+(1|trial_trap),data=model_df_2,family=binomial)
model_treatl2<-glmmTMB(alive~treatment+length+(1|trial_trap),data=model_df_2,family=binomial)

#treatment and temp and interaction model 
model_tti1<-glmer(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)
model_tti2<-glmmTMB(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df_2,family=binomial)

#treatment and temp and length model 
model_ttl1<-glmer(alive~treatment+temp+length+(1|trial_trap),data=model_df_2,family=binomial)
model_ttl2<-glmmTMB(alive~treatment+temp+length+(1|trial_trap),data=model_df_2,family=binomial)

#treatment and temp and length and length interactions model
model_big1<-glmer(alive~treatment+temp+length+temp*length+treatment*length+(1|trial_trap),data=model_df_2,family=binomial)
model_big2<-glmmTMB(alive~treatment+temp+length+temp*length+treatment*length+(1|trial_trap),data=model_df_2,family=binomial)


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

par(mfrow=c(2,2))
plot(model_tti1)




pred0<-predict(model_treatl1,newdata=data.frame(treatment=model.matrix(model_treatl1)[,2],length=model.matrix(model_treatl1)[,3]), re.form=NA)
plot(model.matrix(model_treatl1)[,],back_trans(pred0))



pframe<-model.frame(model_tti1)
model.matrix(formula(model_tti1,fixed.only=TRUE)[-2],pframe)




pframe <- data.frame(trial_trap=factor(levels(model_df_2$trial_trap), levels=levels(model_df_2$trial_trap)))
cpred1 <- predict(model_length1,re.form=NA,newdata=pframe,type="response")



easyPredCI <- function(model,newdata=NULL,alpha=0.05) {
  ## baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  ## fixed-effects model matrix for new data
  X <- model.matrix(formula(model,fixed.only=TRUE)[-2],newdata)
  beta <- fixef(model) ## fixed-effects coefficients
  V <- vcov(model)     ## variance-covariance matrix of beta
  pred.se <- sqrt(diag(X %*% V %*% t(X))) ## std errors of predictions
  ## inverse-link function
  linkinv <- family(model)$linkinv
  ## construct 95% Normal CIs on the link scale and
  ##  transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  linkinv(cbind(conf.low=pred0-crit*pred.se,
                conf.high=pred0+crit*pred.se))
}
cpred1.CI <- easyPredCI(cmod_lme4_L,pframe)

mfrow(2,2)
plot(model_tti1, type=c(1,2,3,4))




g_pframe <- cbind(expand.grid(year=2004:2006,prev=0:80),Area=1)
g_pred <- predict(gmod_lme4_L,newdata=g_pframe,re.form=NA,
                  type="response")
g_predCI <- easyPredCI(gmod_lme4_L,newdata=g_pframe)
set.seed(101)
g_bb <- bootMer(gmod_lme4_L,FUN=function(x),predict(x,re.form=NA,newdata=g_pframe,type="response"),nsim=400)


p1 <- plot(model_tti1,id=0.05,idLabels=~.obs)
p2 <- plot(model_tti1,ylim=c(-1.5,1),type=c("p","smooth"))






















predict(model_length1,re.form=NA,newdata=model.frame(model_length1))


easyPredCI <- function(model,newdata=NULL,alpha=0.05) {
  ## baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  ## fixed-effects model matrix for new data
  X <- model.matrix(formula(model,fixed.only=TRUE)[-2],model.frame(model))
  beta <- fixef(model) ## fixed-effects coefficients
  V <- vcov(model)     ## variance-covariance matrix of beta
  pred.se <- sqrt(diag(X %*% V %*% t(X))) ## std errors of predictions
  ## inverse-link function
  linkinv <- family(model)$linkinv
  ## construct 95% Normal CIs on the link scale and
  ##  transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  linkinv(cbind(conf.low=pred0-crit*pred.se,conf.high=pred0+crit*pred.se))
}


plot(sort(model_df_2$length),back_trans(summary(model_length1)$coef[1]+summary(model_length1)$coef[2]*sort(model_df_2$length)),ylim=c(0,1))
lines(model_df_2$length,easyPredCI(model_length1)[,1])
lines(model_df_2$length,easyPredCI(model_length1)[,2])


plot(sort(model_df_2$treatment),back_trans(summary(model_treat1)$coef[1]+summary(model_treat1)$coef[2]*sort(model_df_2$treatment)),ylim=c(0,1))
points(model_df_2$treatment,easyPredCI(model_treat1)[,1], col="red")
points(model_df_2$treatment,easyPredCI(model_treat1)[,2], col="blue")


plot(sort(model_df_2$treatment),back_trans(summary(model_tti1)$coef[1]+summary(model_tti1)$coef[2]*sort(model_df_2$treatment)),ylim=c(0,1))
points(model_df_2$treatment,easyPredCI(model_treat1)[,1], col="red")
points(model_df_2$treatment,easyPredCI(model_treat1)[,2], col="blue")






plot(model_df_2$treatment, model_df_2$alive)
points(sort(model_df_2$length),back_trans(summary(model_length1)$coef[1]+summary(model_length1)$coef[2]*sort(model_df_2$length)))











BIC.names
fixef(model_tt2)





install.packages("MuMIn") # needed for BIC weights

# Get BIC values for each mixed-effects model
BIC.values <- BIC(model_null2, model_treat2, model_temp2, model_length2, model_tt2,model_templ2,model_treatl2,model_tti2, model_ttl2,model_big2)

# Shorthand names for models (in the same order as in BIC.values
BIC.names <- c("Null", "Treatment", "Temperature", "Length","Treatment + Temperature","Temperature + Length","Treatment + Length", "Treatment + Temperature + Treatment x Temperature","Treatment + Temperature + Length","Treatment + Temperature + Length + Treatment x Length + Temperature x Length")

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
BIC.logLik <- c(logLik(model_null2), logLik(model_treat2), logLik(model_temp2), logLik(model_length2), logLik(model_tt2),logLik(model_templ2),logLik(model_treatl2),logLik(model_tti2),logLik(model_ttl2),logLik(model_big2))
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
formula(model_big1)



# Save BIC table 
setwd(here("figures"))
write.csv(BIC.table, paste(Sys.Date(),"Prawn_Survival_BIC_table.csv"))











