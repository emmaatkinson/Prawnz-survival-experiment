#Introduction----

#Date: Wednesday November 22nd 2023
#Author: Jacob Houtman

#I wrote this code with guidance from Emma Atkinson for the paper INSERT TITLE by Emma Atkinson, myself and Dr. Mark Lewis.
#We analyzed data collected by Emma Atkinson, INSERT FIELD TECH NAMES on how prawns survive when
#they are left out of water for different amounts of time.

#In this document I fit a Binomial GLMM to the prawn survival data to understand how time out of water,
#air temperature and body length influence the probability of a prawn surviving the treatment. 
#Subsequently, I compare the different candidate models using measures like accuracy and maximum deviance. 

#We included a random effect based on the 123 traps that a prawn could have been 
#in during the experiment. It captures differences in traps like location, presence of predators, and trap orientation.

#Read in Packages ----
library(MuMIn) 
library(here)
library(glmmTMB)
library(lme4)


#Read in and filter data ----
setwd(here("data-clean"))
model_df<-read.csv("2023-11-28_model_dataframe.csv")


#Model fitting ----
#Here I fit 18 models to our prawn survival data. I fit each model twice, using two packages: lme4 and glmmTMB.
#Each model is a logistic regression that predicts prawn survival (1 or 0, dead or alive). Every model also
#includes a normally distributed random effect term, based on the trap a prawn was in (123 levels). 

#The generalized formula for our models is as follows:

#logit(Ps)= ßo + (ß1 x V1 + ...) + N(0,∂)

#Where:
# Ps is the probability of a prawn survival.
# ßo is an intercept term (in logit space).
# ß1 is a slope coefficient. 
# V1 is a predictor variable or covariate. It could be a main effect length, temperature or treatment or an interaction term. 
# ∂ is the standard deviation term of the random effect. 

#The models include some combination of length, treatment time, and temperature as predictor variables. 
#If an interaction term is included in a model, both of the covariates are
#included in the model as main effects too. The main effects are not specified explicitly, the default of these packages is 
#to include interaction variables as main effects too.

#The following sections are labelled according to the covariates they include;
#section 0 includes no covariates (null model); section 1 - 3 include only main effects; 
#section 4 includes one two way interaction and only the two main effects present in the interaction;
#section 5 includes one two way interaction and all three  main effects;
#section 6 includes two two way interactions, and therefore all three main effects;
#section 7 includes all three two way interactions. 

#We did not include a model with a three-way interaction.

##0: Null model----
model_0_1<-lme4::glmer(alive~(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_0_2<-glmmTMB::glmmTMB(alive~(1|trial_trap),data=model_df,family=binomial)

##1: 1 main effect----

#treatment model
model_1.1_1<-glmer(alive~treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.1_2<-glmmTMB(alive~treatment+(1|trial_trap),data=model_df,family=binomial)

#temp model
model_1.2_1<-glmer(alive~temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.2_2<-glmmTMB(alive~temp+(1|trial_trap),data=model_df,family=binomial)

#length model
model_1.3_1<-glmer(alive~length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_1.3_2<-glmmTMB(alive~length+(1|trial_trap),data=model_df,family=binomial)


##2: 2 main effects----

#treatment and temp model 
model_2.1_1<-glmer(alive~treatment+temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.1_2<-glmmTMB(alive~treatment+temp+(1|trial_trap),data=model_df,family=binomial)

#temp and length model 
model_2.2_1<-glmer(alive~length+temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.2_2<-glmmTMB(alive~length+temp+(1|trial_trap),data=model_df,family=binomial)

#treatment and length model 
model_2.3_1<-glmer(alive~treatment+length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_2.3_2<-glmmTMB(alive~treatment+length+(1|trial_trap),data=model_df,family=binomial)


##3: 3 main effects----

#treatment and temp and length 
model_3_1<-glmer(alive~treatment+temp+length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_3_2<-glmmTMB(alive~treatment+temp+length+(1|trial_trap),data=model_df,family=binomial)


##4: 2 main effects, and a 2-way interaction----

#Treatment*Temp 
model_4.1_1<-glmer(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.1_2<-glmmTMB(alive~treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length
model_4.2_1<-glmer(alive~treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.2_2<-glmmTMB(alive~treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial)

#temp*length
model_4.3_1<-glmer(alive~temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_4.3_2<-glmmTMB(alive~temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial)


##5: 3 main effects, and a 2-way interaction----

#Treatment*Temp +length
model_5.1_1<-glmer(alive~length+treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.1_2<-glmmTMB(alive~length+treatment+temp+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length +temp
model_5.2_1<-glmer(alive~temp+treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.2_2<-glmmTMB(alive~temp+treatment+length+treatment*length+(1|trial_trap),data=model_df,family=binomial)

#temp*length+treatment
model_5.3_1<-glmer(alive~treatment+temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_5.3_2<-glmmTMB(alive~treatment+temp+length+temp*length+(1|trial_trap),data=model_df,family=binomial)


##6: Two 2-way interactions----

#treatment*temp+temp*length
model_6.1_1<-glmer(alive~temp*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.1_2<-glmmTMB(alive~temp*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*temp+treatment*length
model_6.2_1<-glmer(alive~treatment*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.2_2<-glmmTMB(alive~treatment*length+treatment*temp+(1|trial_trap),data=model_df,family=binomial)

#treatment*length+temp*length 
model_6.3_1<-glmer(alive~temp*length+treatment*length+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_6.3_2<-glmmTMB(alive~temp*length+treatment*length+(1|trial_trap),data=model_df,family=binomial)


##7: Three 2-way interactions----

#treatment*length+temp*length +temp*treatment
model_7_1<-glmer(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10,na.action="na.fail")
model_7_2<-glmmTMB(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df,family=binomial,na.action="na.fail")



#BIC Table----
# Get BIC values for each mixed-effects model
BIC.values <- BIC(model_0_1, model_1.1_1, model_1.2_1, model_1.3_1,
                  model_2.1_1, model_2.2_1, model_2.3_1,model_3_1, 
                  model_4.1_1, model_4.2_1,model_4.3_1, model_5.1_1, 
                  model_5.2_1,model_5.3_1,model_6.1_1, model_6.2_1,
                  model_6.3_1,model_7_1)

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

#Save best model
setwd(here("data-clean"))
saveRDS(model_6.1_1, "mymodel.rds")


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
model_table<-data.frame(cbind(round(c(fixef(model_best),0),3),round(c(fixef(model_best_nolength),0),3),round(coefficients(avgm),3),
                              round(coefficients(avgm_nosmall)[c(1,5,2,3,7,4,6)],3),round(c(fixef(model_main),0,0,0),3)))
#Name rows and columns 
rownames(model_table)[7]<-"length:treatment"
colnames(model_table)<-c("Best model","Best model(No small)","Average model","Average model (No small)","Main Effects")


#Manually get values for the random effect hyperparameter

summary(model_best)
summary(model_best_nolength)

summary(avgm)
summary(avgm_nosmall)

summary(model_main)

hyperparameter<-c(0.8792,0.9319,NA,NA,0.8697)


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
model_table["Correlation",]<-c(correlation[2],NA,correlation[1],NA,correlation[3])

model_table["Accuracy",]<-c(accuracy[2],NA,accuracy[1],NA,accuracy[3])
model_table["Max Deviance",]<-c(deviance[2],NA,deviance[1],NA,deviance[3])

setwd(here("figures"))
write.csv(model_table, paste(Sys.Date(),"model_comparison.csv",sep ="_"))


