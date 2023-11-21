#Read in Packages ----
library(MuMIn) 
library(here)
library(glmmTMB)
library(lme4)

#Set working directory + read in data ----
setwd(here("data-clean"))

model_df<-read.csv("2023_08_10_model_dataframe")


##Model fitting ----
#NULL:0
model_0_1<-lme4::glmer(alive~(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_0_2<-glmmTMB::glmmTMB(alive~(1|trial_trap),data=model_df,family=binomial)

#1 MAIN EFFECT:1

#treatment model
model_1.1_1<-glmer(alive~treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.1_2<-glmmTMB(alive~treatment+(1|trial_trap),data=model_df,family=binomial)

#temp model
model_1.2_1<-glmer(alive~temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.2_2<-glmmTMB(alive~temp+(1|trial_trap),data=model_df,family=binomial)

#length model
model_1.3_1<-glmer(alive~length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.3_2<-glmmTMB(alive~length+(1|trial_trap),data=model_df,family=binomial)


#2 MAIN EFFECTS:2

#treatment and temp model 
model_2.1_1<-glmer(alive~treatment+temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.1_2<-glmmTMB(alive~treatment+temp+(1|trial_trap),data=model_df,family=binomial)

#temp and length model 
model_2.2_1<-glmer(alive~length+temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.2_2<-glmmTMB(alive~length+temp+(1|trial_trap),data=model_df,family=binomial)

#treatment and length model 
model_2.3_1<-glmer(alive~treatment+length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.3_2<-glmmTMB(alive~treatment+length+(1|trial_trap),data=model_df,family=binomial)


#3 MAIN EFFECTS:3

#treatment and temp and length 
model_3_1<-glmer(alive~treatment+temp+length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_3_2<-glmmTMB(alive~treatment+temp+length+(1|trial_trap),data=model_df,family=binomial)


# 2 main 1 interaction:4

#Treatment*Temp 
model_4.1_1<-glmer(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.1_2<-glmmTMB(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length
model_4.2_1<-glmer(alive~treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.2_2<-glmmTMB(alive~treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial)

#temp*length
model_4.3_1<-glmer(alive~temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.3_2<-glmmTMB(alive~temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial)


# 3 main 1 interaction:5

#Treatment*Temp +length
model_5.1_1<-glmer(alive~length+treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.1_2<-glmmTMB(alive~length+treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length +temp
model_5.2_1<-glmer(alive~temp+treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.2_2<-glmmTMB(alive~temp+treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial)

#temp*length+treatment
model_5.3_1<-glmer(alive~treatment+temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.3_2<-glmmTMB(alive~treatment+temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial)


#2 interaction:6

#treatment*temp+temp*length
model_6.1_1<-glmer(alive~temp*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.1_2<-glmmTMB(alive~temp*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*temp+treatment*length
model_6.2_1<-glmer(alive~treatment*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.2_2<-glmmTMB(alive~treatment*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length+temp*length 
model_6.3_1<-glmer(alive~temp*length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.3_2<-glmmTMB(alive~temp*length+treatment*length+(1|trial_trap),data=model_df,family=binomial)


# 2-way interactions:7

#treatment*length+temp*length +temp*treatment
model_7_1<-glmer(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10,na.action="na.fail")
model_7_2<-glmmTMB(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df,family=binomial,na.action="na.fail")




##BIC Table----
# Get BIC values for each mixed-effects model
BIC.values <- BIC(model_0_1, model_1.1_1, model_1.2_1, model_1.3_1,
                  model_2.1_1, model_2.2_1, model_2.3_1,model_3_1, 
                  model_4.1_1, model_4.2_1,model_4.3_1, model_5.1_1, 
                  model_5.2_1,model_5.3_1,model_6.1_1, model_6.2_1,
                  model_6.3_1,model_7_1)

c("model_0_1", 'model_1.1_1', 'model_1.2_1', 'model_1.3_1',
   'model_2.1_1', 'model_2.2_1', 'model_2.3_1','model_3_1', 
   'model_4.1_1', 'model_4.2_1','model_4.3_1', 'model_5.1_1', 
   'model_5.2_1','model_5.3_1','model_6.1_1', 'model_6.2_1',
   'model_6.3_1','model_7_1')

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
BIC.logLik <- c(logLik(model_0_2), logLik(model_1.1_2), logLik(model_1.2_2), logLik(model_1.3_2), logLik(model_2.1_2), logLik(model_2.2_2), logLik(model_2.3_2),logLik(model_3_2), logLik(model_4.1_2), logLik(model_4.2_2),logLik(model_4.3_2), logLik(model_5.1_2), logLik(model_5.2_2),logLik(model_5.3_2),logLik(model_6.1_2), logLik(model_6.2_2),logLik(model_6.3_2),logLik(model_7_2))
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
  weights=BIC.weights,
  logLik = round(BIC.logLik,2))

# Save BIC table 
setwd(here("figures"))
write.csv(BIC.table, paste(Sys.Date(),"prawn_survival_BIC_table.csv"))

#Model Averaging----
#Resale the variables so they are ~Normal(0,1), for comparison
model_df$temp<-scale(model_df$temp)
model_df$treatment<-scale(model_df$treatment)

#Remove small prawns, to see how much it influences 
model_df_nosmall<-model_df[which(model_df$length>28.5),]

#Rescale Length variable
model_df$length<-scale(model_df$length)

#Rescale altered length variable
model_df_nosmall$length<-scale(model_df_nosmall$length)

#Make BIC function
BIC <- function(x) AIC(x, k = log(length(residuals(x))))

#Re-fit models with new variables----
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

#
saveRDS(model_6.1_1, "mymodel.rds")

#Dredge maximal model
ms1 <- dredge(model_all, rank=BIC)
confset.95p <- get.models(ms1, subset = weight >0)
avgm<-model.avg(confset.95p, rank = BIC)

avgm$coefArray
ms1_nosmall <- dredge(model_all_nosmall, rank=BIC)
confset.95p <- get.models(ms1_nosmall, subset = weight >0)
avgm_nosmall<-model.avg(confset.95p, rank = BIC)
avgm

coefficients(avgm)
coefficients(avgm_nosmall)[c(1,5,2,3,7,4,6)]
fixef(model_best)
fixef(model_main)
fixef(model_best_nolength)

round(2.1345,3)
model_table<-data.frame(cbind(round(c(fixef(model_best),0),3),round(c(fixef(model_best_nolength),0),3),round(coefficients(avgm),3),
                              round(coefficients(avgm_nosmall)[c(1,5,2,3,7,4,6)],3),round(c(fixef(model_main),0,0,0),3)))
#model_table<-model_table[,2:4]
rownames(model_table)[7]<-"length:treatment"
colnames(model_table)<-c("Best model","Best model(No small)","Average model","Average model (No small)","Main Effects")



#Manually get values for the random effect hyperparameter

summary(model_best)
summary(model_best_nolength)

summary(avgm)
summary(avgm_nosmall)

summary(model_main)

hyperparameter<-c(0.8791,0.9342,NA,NA,0.8697)

model_table["RE SD",]<-hyperparameter


#Accuracy
model_df_pred<-data.frame(alive=model_df$alive)

avgm_pred<-predict(avgm,model_df, type = "response")
best_pred<-predict(model_best,model_df, type = "response")
main_pred<-predict(model_all,model_df, type = "response")

model_df_pred$predict_avg <- ifelse(avgm_pred>= .5, 1,0)
model_df_pred$predict_best <- ifelse(best_pred>= .5, 1,0)
model_df_pred$predict_main <- ifelse(main_pred>= .5, 1,0)

accuracy_avg<-mean(model_df_pred$alive==model_df_pred$predict_avg)
accuracy_best<-mean(model_df_pred$alive==model_df_pred$predict_best)
accuracy_main<-mean(model_df_pred$alive==model_df_pred$predict_main)

accuracy=c(accuracy_avg,accuracy_best,accuracy_main)

##Correlation Factor

expit<-function(x){
  return(exp(x)/(1+exp(x)))
}

##Correlation with data
correlation<-c(cor(expit(avgm_pred),expit(model_df$alive)),
               cor(expit(best_pred),expit(model_df$alive)),
               cor(expit(main_pred),expit(model_df$alive)))

#MAXIMUM DEVIANCE

##Make Fake data
minlength<-min(model_df$length)
maxlength<-max(model_df$length)
mintemp<-min(model_df$temp)
maxtemp<-max(model_df$temp)
mintreat<-min(model_df$treatment)
maxtreat<-max(model_df$treatment)


length=seq(minlength,maxlength,length.out=20)
temp=seq(mintemp,maxtemp,length.out=20)
treatment=seq(mintemp,maxtemp,length.out=20)
trial_trap=c(NA)


dev_data<-expand.grid(length=length,temp=temp,treatment=treatment,trial_trap=trial_trap)
head(dev_data)

avgm_pred_new<-predict(avgm,dev_data,re.form=NA, type = "response")
best_pred_new<-predict(model_best,dev_data,re.form=NA, type = "response")
main_pred_new<-predict(model_all,dev_data,re.form=NA, type = "response")

max_dev_average<-0
max_dev_best<-max(c(abs(max(avgm_pred-best_pred)),abs(min(avgm_pred-best_pred))))
max_dev_main<-max(c(abs(max(avgm_pred-main_pred)),abs(min(avgm_pred-main_pred))))

deviance<-c(max_dev_average,max_dev_best,max_dev_main)

model_table["Correlation",]<-c(correlation[2],NA,correlation[1],NA,correlation[3])

model_table["Accuracy",]<-c(accuracy[2],NA,accuracy[1],NA,accuracy[3])
model_table["Max Deviance",]<-c(deviance[2],NA,deviance[1],NA,deviance[3])

setwd(here("figures"))
write.csv(model_table, paste(Sys.Date(),"model_comparison.csv",sep ="_"))


