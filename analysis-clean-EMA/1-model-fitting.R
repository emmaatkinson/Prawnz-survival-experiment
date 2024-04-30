# Date created: 22-Nov-2023
# Last updated: 24-Apr-2024
# Authors: Jacob Houtman & Emma Atkinson
# Description: Prawn survival experiment (model-fitting)
# Notes: This code analyses survival data from a field experiment investigating 
#        the post-release survival of spot prawns caught in a trap fishery.
#
################################################################################

# Load packages #
library(MuMIn) 
library(here)
library(glmmTMB)
library(lme4)
library(DHARMa)

# Read in clean dataset for analysis (output from XXX) #
setwd(here("data-clean"))
model_df<-read.csv("2024-04-29_model_dataframe.csv")

model_df$trial_trap = as.factor(model_df$trial_trap)

################################################################################
#   
#   Part one: Model-fitting
#
#   *We fit 18 formulations of a binomial GLMM with all biologically reasonable
#    combinations of three continuous predictor variables and their two-way 
#    interactions: carapace length (mm), air temperature (deg C), and experimental
#    treatment (mins out of water). Response variable is survival (1=alive, 0=dead).
#   *We include a random effect on the intercept for trap (123 levels).
#   *We fit the full set of models in both lme4 and glmmTMB to check for any
#    model-fitting inconsistencies. 
#   *Note that many of the lme4 model fits will throw convergence errors.
#
################################################################################

##0: Null model----
model_0_1<-lme4::glmer(alive~(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_0_2<-glmmTMB::glmmTMB(alive~(1|trial_trap),data=model_df,family=binomial)

##1: Models with one main effect----

#treatment model
model_1.1_1<-glmer(alive~treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.1_2<-glmmTMB(alive~treatment+(1|trial_trap),data=model_df,family=binomial)

#temp model
model_1.2_1<-glmer(alive~temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.2_2<-glmmTMB(alive~temp+(1|trial_trap),data=model_df,family=binomial)

#length model
model_1.3_1<-glmer(alive~length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.3_2<-glmmTMB(alive~length+(1|trial_trap),data=model_df,family=binomial)


##2: Models with two main effects----

#treatment and temp model 
model_2.1_1<-glmer(alive~treatment+temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.1_2<-glmmTMB(alive~treatment+temp+(1|trial_trap),data=model_df,family=binomial)

#temp and length model 
model_2.2_1<-glmer(alive~length+temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.2_2<-glmmTMB(alive~length+temp+(1|trial_trap),data=model_df,family=binomial)

#treatment and length model 
model_2.3_1<-glmer(alive~treatment+length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.3_2<-glmmTMB(alive~treatment+length+(1|trial_trap),data=model_df,family=binomial)


##3: Models with three main effects----

#treatment and temp and length 
model_3_1<-glmer(alive~treatment+temp+length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_3_2<-glmmTMB(alive~treatment+temp+length+(1|trial_trap),data=model_df,family=binomial)


##4: Models with 2 main effects, and a 2-way interaction----

#Treatment*Temp 
model_4.1_1<-glmer(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.1_2<-glmmTMB(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length
model_4.2_1<-glmer(alive~treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.2_2<-glmmTMB(alive~treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial)

#temp*length
model_4.3_1<-glmer(alive~temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.3_2<-glmmTMB(alive~temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial)


##5: Models with three main effects, and one 2-way interaction----

#Treatment*Temp +length
model_5.1_1<-glmer(alive~length+treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.1_2<-glmmTMB(alive~length+treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length +temp
model_5.2_1<-glmer(alive~temp+treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.2_2<-glmmTMB(alive~temp+treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial)

#temp*length+treatment
model_5.3_1<-glmer(alive~treatment+temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.3_2<-glmmTMB(alive~treatment+temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial)


##6: Models with two 2-way interactions----

#treatment*temp+temp*length
model_6.1_1<-glmer(alive~temp*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.1_2<-glmmTMB(alive~temp*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*temp+treatment*length
model_6.2_1<-glmer(alive~treatment*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.2_2<-glmmTMB(alive~treatment*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length+temp*length 
model_6.3_1<-glmer(alive~temp*length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.3_2<-glmmTMB(alive~temp*length+treatment*length+(1|trial_trap),data=model_df,family=binomial)


##7: Models with three 2-way interactions----

#treatment*length+temp*length +temp*treatment
model_7_1<-glmer(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10,na.action="na.fail")
model_7_2<-glmmTMB(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df,family=binomial,na.action="na.fail")

################################################################################
#   Part two: Model diagnostics
#
#   *We assess quality of model fit using functions from the DHARMa package
#
################################################################################

# Model diagnostics for lme4 models #

model.list=c(model_0_1, model_1.1_1, model_1.2_1, model_1.3_1,
             model_2.1_1, model_2.2_1, model_2.3_1,model_3_1, 
             model_4.1_1, model_4.2_1,model_4.3_1, model_5.1_1, 
             model_5.2_1,model_5.3_1,model_6.1_1, model_6.2_1,
             model_6.3_1,model_7_1)

model.names=c("model_0_1", "model_1.1_1", "model_1.2_1", "model_1.3_1",
              "model_2.1_1", "model_2.2_1", "model_2.3_1", "model_3_1", 
              "model_4.1_1", "model_4.2_1", "model_4.3_1", "model_5.1_1", 
              "model_5.2_1", "model_5.3_1", "model_6.1_1", "model_6.2_1",
              "model_6.3_1", "model_7_1")

for (i in 1:length(model.list)) {

      simres = simulateResiduals(fittedModel=model.list[[i]])
      
      png(here("model-outputs-EMA","model-diagnostics",paste(model.names[i], "residuals.png",sep="-")), width=8, height=4, units="in", res=400, pointsize=10)
      plot(simres, asFactor=TRUE)
      dev.off()
      
      png(here("model-outputs-EMA","model-diagnostics",paste(model.names[i], "residuals-2.png",sep="-")), width=8, height=4, units="in", res=400, pointsize=10)
      plotResiduals(simres, model_df$treatment, quantreg=TRUE)
      dev.off()
      
      png(here("model-outputs-EMA","model-diagnostics",paste(model.names[i], "dispersion.png",sep="-")), width=8, height=4, units="in", res=400, pointsize=10)
      par(mfrow=c(1,2))
      testDispersion(simres)
      simres2=recalculateResiduals(simres, group=model_df$trial_trap)
      testDispersion(simres2)
      dev.off()
      
  }


# Model diagnostics for TMB models #

model.list.TMB =list(model_0_2, model_1.1_2, model_1.2_2, model_1.3_2,
                  model_2.1_2, model_2.2_2, model_2.3_2, model_3_2, 
                  model_4.1_2, model_4.2_2, model_4.3_2, model_5.1_2, 
                  model_5.2_2, model_5.3_2, model_6.1_2, model_6.2_2,
                  model_6.3_2, model_7_2)

model.names=c("model_0_2", "model_1.1_2", "model_1.2_2", "model_1.3_2",
              "model_2.1_2","model_2.2_2", "model_2.3_2", "model_3_2", 
              "model_4.1_2", "model_4.2_2", "model_4.3_2", "model_5.1_2", 
              "model_5.2_2", "model_5.3_2", "model_6.1_2", "model_6.2_2",
              "model_6.3_2", "model_7_2")

for (i in 1:length(model.list.TMB)) {
  
  simres = simulateResiduals(fittedModel=model.list.TMB[[i]])
  
  png(here("model-outputs-EMA","model-diagnostics",paste("TMB",model.names[i], "residuals.png",sep="-")), width=8, height=4, units="in", res=400, pointsize=10)
  plot(simres, asFactor=TRUE)
  dev.off()
  
  png(here("model-outputs-EMA","model-diagnostics",paste("TMB", model.names[i], "residuals-2.png",sep="-")), width=8, height=4, units="in", res=400, pointsize=10)
  plotResiduals(simres, model_df$treatment, quantreg=TRUE)
  dev.off()
  
  png(here("model-outputs-EMA","model-diagnostics",paste("TMB",model.names[i], "dispersion.png",sep="-")), width=8, height=4, units="in", res=400, pointsize=10)
  par(mfrow=c(1,2))
  testDispersion(simres)
  simres2=recalculateResiduals(simres, group=model_df$trial_trap)
  testDispersion(simres2)
  dev.off()
  
}
################################################################################
#   Part three: Model-selection & model-averaging
#
#   *We generate BIC estimates and summary tables for the full model set from
#    both the lme4 and TMB model fits.
#   *Generate model-averaged predictions 
#
################################################################################

# --- BIC table for lme4 models --- #

# Get BIC values for each mixed-effects model
BIC.values <- BIC(model_0_1, model_1.1_1, model_1.2_1, model_1.3_1,
                  model_2.1_1, model_2.2_1, model_2.3_1,model_3_1, 
                  model_4.1_1, model_4.2_1,model_4.3_1, model_5.1_1, 
                  model_5.2_1,model_5.3_1,model_6.1_1, model_6.2_1,
                  model_6.3_1,model_7_1)

##### BIC Table for lme4 models ############

# Shorthand names for models (in the same order as in BIC.values)
BIC.names <- c("Null", "Treatment", "Temperature", "Length",
               "Treatment + Temperature","Temperature + Length",
               "Treatment + Length","Length + Treatment + Temperature",
               "Treatment x Temperature","Treatment x Length",
               "Temperature x Length", "Treatment x Temperature + Length",
               "Treatment x Length + Temperature","Temperature x Length + Treatment",
               "Treatment x Temperature + Temperature x Length",
               "Treatment x Temperature + Treatment x Length",
               "Treatment x Length + Temperature x Length", 
               "Treatment x Temperature + Treatment x Length + Temperature x Length")

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
BIC.logLik <- c(logLik(model_0_1), logLik(model_1.1_1), logLik(model_1.2_1), logLik(model_1.3_1), logLik(model_2.1_1), logLik(model_2.2_1), logLik(model_2.3_1),logLik(model_3_1), logLik(model_4.1_1), logLik(model_4.2_1),logLik(model_4.3_1), logLik(model_5.1_1), logLik(model_5.2_1),logLik(model_5.3_1),logLik(model_6.1_1), logLik(model_6.2_1),logLik(model_6.3_1),logLik(model_7_1))
BIC.logLik <- BIC.logLik[BIC.order]

# Create delta BIC function and calculate for each model relative to top model
deltaFcn = function(listBIC) {
  deltas = -(min(listBIC) - listBIC)
}
BIC.deltas = deltaFcn(BIC.values)

# Create BIC table
BIC.table <- data.frame(
  model.name = BIC.names, BIC= round(BIC.values,1),
  deltaBIC = round(BIC.deltas,1),
  weights=round(BIC.weights,2),
  logLik = round(BIC.logLik,1))

# Save BIC table 
write.csv(BIC.table, here("model-outputs-EMA", paste(Sys.Date(),"-BIC-table-lme4.csv", sep="")))

############# BIC table for TMB models #################
BIC.values.TMB <- BIC(model_0_2, model_1.1_2, model_1.2_2, model_1.3_2,
                      model_2.1_2, model_2.2_2, model_2.3_2,model_3_2, 
                      model_4.1_2, model_4.2_2,model_4.3_2, model_5.1_2, 
                      model_5.2_2,model_5.3_2,model_6.1_2, model_6.2_2,
                      model_6.3_2,model_7_2)
# Shorthand names for models (in the same order as in BIC.values)
BIC.names <- c("Null", "Treatment", "Temperature", "Length",
               "Treatment + Temperature","Temperature + Length",
               "Treatment + Length","Length + Treatment + Temperature",
               "Treatment x Temperature","Treatment x Length",
               "Temperature x Length", "Treatment x Temperature + Length",
               "Treatment x Length + Temperature","Temperature x Length + Treatment",
               "Treatment x Temperature + Temperature x Length",
               "Treatment x Temperature + Treatment x Length",
               "Treatment x Length + Temperature x Length", 
               "Treatment x Temperature + Treatment x Length + Temperature x Length")

# Order the BIC and model name vectors by ascending order of the AIC vector elements
BIC.order <- order(BIC.values.TMB$BIC)
BIC.names <- BIC.names[BIC.order]
BIC.values.TMB <- BIC.values.TMB$BIC[BIC.order]

# Calculate BIC weights 
BIC.weights <- MuMIn::Weights(BIC.values.TMB)

# Calculate BIC cumulative weights
BIC.cumul.weights <- numeric(length(BIC.weights))
BIC.cumul.weights[1] <- BIC.weights[1]
for (i in 2:length(BIC.weights)) {
  BIC.cumul.weights[i] <- BIC.cumul.weights[i-1] + BIC.weights[i]
}

# Calculate maximum log likelihood values and re-order
BIC.logLik <- c(logLik(model_0_2), logLik(model_1.1_2), logLik(model_1.2_2), logLik(model_1.3_2), logLik(model_2.1_2), logLik(model_2.2_2), logLik(model_2.3_2),logLik(model_3_2), logLik(model_4.1_2), logLik(model_4.2_2),logLik(model_4.3_2), logLik(model_5.1_2), logLik(model_5.2_2),logLik(model_5.3_2),logLik(model_6.1_2), logLik(model_6.2_2),logLik(model_6.3_2),logLik(model_7_2))
BIC.logLik <- BIC.logLik[BIC.order]

# Create delta BIC function and calculate for each model relative to top model
deltaFcn = function(listBIC) {
  deltas = -(min(listBIC) - listBIC)
}
BIC.deltas = deltaFcn(BIC.values.TMB)

# Create BIC table
BIC.table.TMB <- data.frame(
  model.name = BIC.names, BIC= round(BIC.values.TMB,1),
  deltaBIC = round(BIC.deltas,1),
  weights=round(BIC.weights,2),
  logLik = round(BIC.logLik,1))

# Save BIC table 
write.csv(BIC.table.TMB, here("model-outputs-EMA", paste(Sys.Date(),"-BIC-table-TMB.csv", sep="")))

# Save top model (TMB and lme4 versions)
saveRDS(model_6.1_1, here("model-outputs-EMA","treat-temp_temp-length_lme4.rds"))
saveRDS(model_6.1_2, here("model-outputs-EMA","treat-temp_temp-length_TMB.rds"))

################################################################################
#   Part four: Model-averaging, model predictions, and confidence intervals
#
#   *We generate model-averaged predictions from top 5 models and generate
#    confidence intervals on model estimates
#
################################################################################

### Model predictions for lme4 models ###
newdat = expand.grid(treatment=seq(0,120,5),
                     length=seq(18,53,1),
                     temp=seq(10,26,1),
                     alive=NA)

newdat$alive = predict(model_6.1_1, newdat, re.form=NA)
mm = model.matrix(terms(model_6.1_1,),newdat)

newdat$alive_prob = 1/(1+exp(-newdat$alive))

pvar1 = diag(mm %*% tcrossprod(vcov(model_6.1_1),mm))
#tvar1 = pvar1+VarCorr(model_6.1_1)$Subject[1]
cmult = 1.96
newdat2 = data.frame(
  newdat
  , plo = newdat$alive-cmult*sqrt(pvar1)
  , phi = newdat$alive+cmult*sqrt(pvar1)
#  , tlo = newdat$alive-cmult*sqrt(tvar1)
#  , thi = newdat$alive+cmult*sqrt(tvar1)
)

newdat2$plo_prob = 1/(1+exp(-newdat2$plo))
newdat2$phi_prob = 1/(1+exp(-newdat2$phi))

d_low_small = newdat2[newdat2$length==24 & newdat2$temp==10,]
d_med_small = newdat2[newdat2$length==24 & newdat2$temp==15,]
d_high_small = newdat2[newdat2$length==24 & newdat2$temp==19,]
d_vhigh_small = newdat2[newdat2$length==24 & newdat2$temp==24,]


plot(d_low_small$treatment, d_low_small$alive_prob, type="l", ylim=c(0,1), col="white", bty="n")

polygon(c(d_low_small$treatment,rev(d_low_small$treatment)), c(d_low_small$phi_prob, rev(d_low_small$plo_prob)), col=alpha("black",.25), border=NA)
lines(d_low_small$treatment, d_low_small$alive_prob, lwd=2)

polygon(c(d_med_small$treatment,rev(d_med_small$treatment)), c(d_med_small$phi_prob, rev(d_med_small$plo_prob)), col=alpha("black",.25), border=NA)
lines(d_med_small$treatment, d_med_small$alive_prob, lwd=2)

polygon(c(d_high_small$treatment,rev(d_high_small$treatment)), c(d_high_small$phi_prob, rev(d_high_small$plo_prob)), col=alpha("black",.25), border=NA)
lines(d_high_small$treatment, d_high_small$alive_prob, lwd=2)

polygon(c(d_vhigh_small$treatment,rev(d_vhigh_small$treatment)), c(d_vhigh_small$phi_prob, rev(d_vhigh_small$plo_prob)), col=alpha("black",.25), border=NA)
lines(d_vhigh_small$treatment, d_vhigh_small$alive_prob, lwd=2)

abline(h=.5, lty=2)
abline(v=c(23.84278,46.12117,72.46385,112.50474))

#################################################################################
### Model predictions for TMB models ###
#################################################################################

newdatTMB = expand.grid(treatment=seq(0,120,5),
                        length=seq(18,53,1),
                        temp=seq(10,26,1))

mmTMB = model.matrix(delete.response(terms(model_6.1_2)), newdatTMB) 
newdatTMB$alive = drop(mmTMB %*% fixef(model_6.1_2)[["cond"]])
newdatTMB$alive_prob = 1/(1+exp(-newdatTMB$alive))
predvarTMB = diag(mmTMB %*% vcov(model_6.1_2)[["cond"]] %*% t(mmTMB))
newdatTMB$SE = sqrt(predvarTMB)
newdatTMB$SE2 = sqrt(predvarTMB+sigma(model_6.1_2)^2)

newdatTMB$phi = newdatTMB$alive + 1.96*newdatTMB$SE
newdatTMB$plo = newdatTMB$alive - 1.96*newdatTMB$SE
newdatTMB$phi_prob = 1/(1+exp(-newdatTMB$phi))
newdatTMB$plo_prob = 1/(1+exp(-newdatTMB$plo))

d_low_small = newdatTMB[newdatTMB$length==24 & newdatTMB$temp==10,]
d_med_small = newdatTMB[newdatTMB$length==24 & newdatTMB$temp==15,]
d_high_small = newdatTMB[newdatTMB$length==24 & newdatTMB$temp==19,]
d_vhigh_small = newdatTMB[newdatTMB$length==24 & newdatTMB$temp==24,]


plot(d_low_small$treatment, d_low_small$alive_prob, type="l", ylim=c(0,1), col="white", bty="n")

polygon(c(d_low_small$treatment,rev(d_low_small$treatment)), c(d_low_small$phi_prob, rev(d_low_small$plo_prob)), col=alpha("black",.25), border=NA)
lines(d_low_small$treatment, d_low_small$alive_prob, lwd=2)

polygon(c(d_med_small$treatment,rev(d_med_small$treatment)), c(d_med_small$phi_prob, rev(d_med_small$plo_prob)), col=alpha("black",.25), border=NA)
lines(d_med_small$treatment, d_med_small$alive_prob, lwd=2)

polygon(c(d_high_small$treatment,rev(d_high_small$treatment)), c(d_high_small$phi_prob, rev(d_high_small$plo_prob)), col=alpha("black",.25), border=NA)
lines(d_high_small$treatment, d_high_small$alive_prob, lwd=2)

polygon(c(d_vhigh_small$treatment,rev(d_vhigh_small$treatment)), c(d_vhigh_small$phi_prob, rev(d_vhigh_small$plo_prob)), col=alpha("black",.25), border=NA)
lines(d_vhigh_small$treatment, d_vhigh_small$alive_prob, lwd=2)



 ############# Write prediction dataframes to file #############################
write.csv(newdat2, here("model-outputs-EMA","2024-04-29-predictions-lme4.csv"))
write.csv(newdatTMB, here("model-outputs-EMA","2024-04-29-predictions-TMB.csv"))



################################################################################
################################################################################

#Model Averaging and Coefficients----

##Alter variables for model comparison----
#In this section I filter the smaller prawns out to see how the model coefficients change when small
#prawns are removed
#I also scale variables so that they are Normal (0,1) for comparison

#Rescale the variables so they are ~Normal(0,1), for comparison
model_df$temp<-scale(model_df$temp)
model_df$treatment<-scale(model_df$treatment)

#Remove small prawns, to see how much it influences 
model_df_nosmall<-model_df[which(model_df$length>28.5),]

#Rescale Length variable
model_df$length<-scale(model_df$length)

#Rescale altered length variable
model_df_nosmall$length<-scale(model_df_nosmall$length)


##Re-fit models with new variables----

#Main Effects only model
model_main<-glmer(alive~length+temp+treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)

#Re-fit  best model
model_best<-glmer(alive~length*temp+temp*treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)

#Maximum model so we can model average with it
model_all<-glmmTMB(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df,family=binomial,na.action="na.fail")

#Best model, with no smaller prawns 
model_best_nolength<-glmer(alive~length*temp+temp*treatment+(1|trial_trap),data=model_df_nosmall,family=binomial,nAGQ=10)

#Maximum model, no small prawns, so we can model average with it
model_all_nosmall<-glmmTMB(alive~length*temp+treatment*length+temp*treatment+(1|trial_trap),data=model_df_nosmall,family=binomial,na.action="na.fail")

#Make BIC function
BIC <- function(x) AIC(x, k = log(length(residuals(x))))

#Dredge maximal model fo model averaging
ms1 <- dredge(model_all, rank=BIC)
confset.95p <- get.models(ms1, subset = weight >0)
avgm<-model.avg(confset.95p, rank = BIC)

#Do the same thing but for the models with no small prawns 
ms1_nosmall <- dredge(model_all_nosmall, rank=BIC)
confset.95p <- get.models(ms1_nosmall, subset = weight >0)
avgm_nosmall<-model.avg(confset.95p, rank = BIC)

#View coefficients
coefficients(avgm)
coefficients(avgm_nosmall)[c(1,5,2,3,7,4,6)]
fixef(model_best)
fixef(model_main)
fixef(model_best_nolength)

##Create model_table----
model_table<-data.frame(cbind(round(c(fixef(model_best),0),1),round(c(fixef(model_best_nolength),0),1),round(coefficients(avgm),1),
                              round(coefficients(avgm_nosmall)[c(1,5,2,3,7,4,6)],1),round(c(fixef(model_main),0,0,0),1)))
#Name rows and columns 
rownames(model_table)[7]<-"length:treatment"
colnames(model_table)<-c("Best model","Best model(No small prawns)","Average model","Average model (No small prawns)","Main Effects")


#Manually get values for the random effect hyperparameter

summary(model_best)
summary(model_best_nolength)

summary(avgm)
summary(avgm_nosmall)

summary(model_main)

hyperparameter<-c(0.88,0.93,NA,NA,0.87)


#Add row to model_table
model_table["RE SD",]<-hyperparameter


#Model Comparison----
##Accuracy----
#Accuracy for a logistic regression model is calculated as the number of data-points
#which the model correctly predicted.
#In other words, if the data point is a 0 (a dead prawn) and the model predicts the 
#probability of survival as below 0.5 that would count as a correct prediction. 

#Store the true survival values data frame 
model_df_pred<-data.frame(alive=model_df$alive)

#Predict survival probability for each prawn and store as a vector, for each model
avgm_pred<-predict(avgm,model_df, type = "response")
best_pred<-predict(model_best,model_df, type = "response")
main_pred<-predict(model_all,model_df, type = "response")

#For each element of the vectors above, round to whichever 1 or 0 is closer
model_df_pred$predict_avg <- ifelse(avgm_pred>= .5, 1,0)
model_df_pred$predict_best <- ifelse(best_pred>= .5, 1,0)
model_df_pred$predict_main <- ifelse(main_pred>= .5, 1,0)

#Count the proportion of model predictions that were right, for each model
accuracy_avg<-mean(model_df_pred$alive==model_df_pred$predict_avg)
accuracy_best<-mean(model_df_pred$alive==model_df_pred$predict_best)
accuracy_main<-mean(model_df_pred$alive==model_df_pred$predict_main)

#Store all three proportions calculated above 
accuracy=c(accuracy_avg,accuracy_best,accuracy_main)


##Correlation Factor----
#Logistic regression uses a link function (canonically the logit link) to transform the range of the response variable (0 or 1) 
#to the range of linear regression (-∞,∞). The data is transformed by the logit link to negative infinity and infinity. A line is drawn
#and the data is projected onto it. Each new y value is back-transformed (inverse logit, AKA expit) and a probability that that data-point
#achieved its true value, these values are multiplied to find the likelihood of the line. The process is repeated until the likelihood (or log thereof)
#is maximized.

#Correlation can be used to measure how related two sets of data are.

#We can therefore back transform the data (0 or 1, discrete) and model predictions (0 to 1, continuous) so they are both (-∞,∞) and see how related they are.

#expit is the inverse of the logit function
expit<-function(x){
  return(exp(x)/(1+exp(x)))
}

##Correlation with data
correlation<-c(cor(expit(avgm_pred),expit(model_df$alive)),
               cor(expit(best_pred),expit(model_df$alive)),
               cor(expit(main_pred),expit(model_df$alive)))

##Maximum Deviance----
#Each of these models can be thought of as a 4D plane (in logit transformed space)
#The 4 dimensions include the predictor variables (length, temperature, treatment time),
#and the response variable (probability of survival)
#In the code below look for how different these model-planes are by
#creating 8000 fake data points and looking for the biggest difference in prediction between models

#Store bounds of data
minlength<-min(model_df$length)
maxlength<-max(model_df$length)
mintemp<-min(model_df$temp)
maxtemp<-max(model_df$temp)
mintreat<-min(model_df$treatment)
maxtreat<-max(model_df$treatment)

#Create fake data, in the bounds
length=seq(minlength,maxlength,length.out=20)
temp=seq(mintemp,maxtemp,length.out=20)
treatment=seq(mintemp,maxtemp,length.out=20)
trial_trap=c(NA)

#Create a fake data grid, with every combination of the above vectors
dev_data<-expand.grid(length=length,temp=temp,treatment=treatment,trial_trap=trial_trap)

#Predict survival for each fake data point with each model
avgm_pred_new<-predict(avgm,dev_data,re.form=NA, type = "response")
best_pred_new<-predict(model_best,dev_data,re.form=NA, type = "response")
main_pred_new<-predict(model_all,dev_data,re.form=NA, type = "response")

#Calculate differences between model predictions
max_dev_best<-max(c(abs(max(avgm_pred_new-best_pred_new)),abs(min(avgm_pred_new-best_pred_new))))
max_dev_main<-max(c(abs(max(avgm_pred_new-main_pred_new)),abs(min(avgm_pred_new-main_pred_new))))

#Maximum deviance for the average model is 0 (because we are comparing models against the average model itself)
max_dev_average<-0

#Store deviance measures
deviance<-c(max_dev_average,max_dev_best,max_dev_main)

#Add Accuracy, correlation and deviance to model table----
model_table["Correlation",]<-round(c(correlation[2],NA,correlation[1],NA,correlation[3]),3)

model_table["Accuracy",]<-round(c(accuracy[2],NA,accuracy[1],NA,accuracy[3]),3)
model_table["Max Deviance",]<-round(c(deviance[2],NA,deviance[1],NA,deviance[3]),3)

setwd(here("New-figures"))
write.csv(model_table, paste(Sys.Date(),"model_comparison.csv",sep ="_"))


