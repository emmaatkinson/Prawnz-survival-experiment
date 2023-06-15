#June 9th 2023
#This document is 

#Coding was done by Jacob Houtman, with guidance from Emma Atkinson and Dr. Mark Lewis 

#Load required packages 
library("here")
library("ggplot2")
library("qpdf")

#Set working directory
setwd(here("data-clean"))

#Read in data
#'trial' has trial level data such as location, numbers of prawns and environmental data. 
#'survival' has individual level data such as the treatment a prawn experienced and whether it survived.
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")

#Sort dataset by trial number
trial<-trial[order(trial$trial_number),]

#Number of trials 
n_trials<-length(unique(trial$trial_number))


#Set up empty vectors with one element for each trial
trial_number<-vector(mode="numeric", length=n_trials)
lost_prawnz<-vector(mode="numeric", length=n_trials)
total_treatments<-vector(mode="numeric", length=n_trials)
unbanded<-vector(mode="numeric", length=n_trials)
scavenged<-vector(mode="numeric", length=n_trials)
dead<-vector(mode="numeric", length=n_trials)
alive<-vector(mode="numeric", length=n_trials)
max_surv_sum<-vector(mode="numeric", length=n_trials)
min_surv_sum<-vector(mode="numeric", length=n_trials)
pulled<-vector(mode="numeric", length=n_trials)
remain<-vector(mode="numeric", length=n_trials)
quarts<-vector(mode="numeric", length=n_trials)

stage_0_per_trial<-vector(mode="numeric", length=n_trials)
stage_1_per_trial<-vector(mode="numeric", length=n_trials)
stage_2_per_trial<-vector(mode="numeric", length=n_trials)
stage_3_per_trial<-vector(mode="numeric", length=n_trials)
stage_NA_per_trial<-vector(mode="numeric", length=n_trials)

lost_immediate<-vector(mode="numeric", length=n_trials)
lost_30<-vector(mode="numeric", length=n_trials)
lost_60<-vector(mode="numeric", length=n_trials)
lost_90<-vector(mode="numeric", length=n_trials)
lost_120<-vector(mode="numeric", length=n_trials)

file_names<-vector(mode="character", length=n_trials)
file_names_1<-vector(mode="character", length=n_trials)

alive_0<-vector(mode="character", length=n_trials)
alive_30<-vector(mode="character", length=n_trials)
alive_60<-vector(mode="character", length=n_trials)
alive_90<-vector(mode="character", length=n_trials)
alive_120<-vector(mode="character", length=n_trials)
remain_0<-vector(mode="character", length=n_trials)
remain_30<-vector(mode="character", length=n_trials)
remain_60<-vector(mode="character", length=n_trials)
remain_90<-vector(mode="character", length=n_trials)
remain_120<-vector(mode="character", length=n_trials)

#'For' loop, looping once for each trial number
for (i in 1:n_trials){

  #datasets of trial and survival for a trial number
  df<-subset(trial, trial_number==sort(trial$trial_number)[i])
  df1<-subset(survival, trial_number==sort(trial$trial_number)[i])
  
  #sum of treatment numbers to ascertain total prawns in each trial
  pulled[i]<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number), na.rm = TRUE)
  
  #total prawns left after soak including dead, scavenged and alive
  remain[i]<-nrow(df1)
  
  #record the 1st quartile for carapace length per trial
  quarts[i]<-quantile(df1$length,probs = .25,na.rm = TRUE)
  
  #Lost prawns in each treatment, calculated as the number in the treatment group at the start minus the
  #number of prawns with that colour band after the release stage. The difference is therefore prawns 
  #from each treatment. However prawns could be lost in two ways: losing their band and no longer being in the trap.
  lost_immediate[i]<-df$immediate_release_number-length(which(df1$treatment=="0"))
  lost_30[i]<-df$X30min_number-length(which(df1$treatment=="30"))
  lost_60[i]<-df$X1h_number-length(which(df1$treatment=="60"))
  lost_90[i]<-df$X1h30min_numner-length(which(df1$treatment=="90"))-length(which(df1$treatment=="100"))
  lost_120[i]<-df$X2h_number-length(which(df1$treatment=="120"))
  
  #the maxima and minima of the sums of three survival values for each prawn for a trial. 
  #The sums should all be 1, since the categories are mutually exclusive. Used for checking data was 
  #entered correctly
  max_surv_sum[i]<-max(df1$alive+df1$dead+df1$scavenged, na.rm=TRUE)
  min_surv_sum[i]<-min(df1$alive+df1$dead+df1$scavenged, na.rm=TRUE)
  
  #number of different treatments for each trial, as some did not include a 120 minute treatment
  total_treatments[i]<-length(unique(df1$treatment))
  

  #unbanded prawns are entered as treatment=NA so the sum of these, per trial
  #is the number of unbanded per trial
  unbanded[i]<-length(which(is.na(df1$treatment)))

  #the number of scavenged,dead and alive prawns per trial 
  scavenged[i]<-sum(df1$scavenged,na.rm=TRUE)
  dead[i]<-sum(df1$dead,na.rm=TRUE)
  alive[i]<-sum(df1$alive,na.rm=TRUE)
  
  #the remaining prawns after each treatment 
  remain_0[i]<-sum(df1$treatment==0)
  remain_30[i]<-sum(df1$treatment==30)
  remain_60[i]<-sum(df1$treatment==60)
  remain_90[i]<-sum(df1$treatment==90)
  remain_120[i]<-sum(df1$treatment==120)
  
  #alive prawns after each treatment 
  alive_0[i]<-sum(df1[which(df1$treatment==0),]$alive)
  alive_30[i]<-sum(df1[which(df1$treatment==30),]$alive)
  alive_60[i]<-sum(df1[which(df1$treatment==60),]$alive)
  alive_90[i]<-sum(df1[which(df1$treatment==90),]$alive)
  alive_120[i]<-sum(df1[which(df1$treatment==120),]$alive)
  
  #change working directory
  setwd(here("figures"))
  
  #Create a data frame with two coloumns: length and sex. Sex (or stage) is 0 for juvenile,
  # 1 for male, 2 for transitional and 3 for female. Length refers to carapace length. 
  violin_length<-data.frame(length=append(df1[which(df1$stage<=1),]$length,df1[which(df1$stage>1),]$length),sex=append(rep("Juv/Male",length(df1[which(df1$stage<=1),]$length)),rep("Trans/Female",length(df1[which(df1$stage>1),]$length))))
  violin_length$sex<-as.factor(violin_length$sex)
  
  #violin plot comparing length distributions for juveniles and males vs transitionals and females
  ggplot(violin_length, aes(x=sex, y=length)) + geom_violin()+ylim(15,55)
  
  #save plot to file
  #ggsave(paste(Sys.Date(), "trial",df$trial_number, "violin.pdf", sep="_"))
  
  #Record file name
  file_names_1[i]<-paste(Sys.Date(), "trial",df$trial_number, "violin.pdf", sep="_")
  
 #Create a pdf (for each trial) with up to twelve summary graphs on it
 pdf(paste(Sys.Date(), "trial",df$trial_number, "hist.pdf", sep="_"), width=7, height=7, pointsize=12)
 par(mfrow=c(6,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
 
 # histogram of lengths in the trial
 hist(df1$length,main=paste("Trial",df$trial_number,"Total"),xlab="Length")
 
 # histograms of lengths in each treatment for that trial 
 # not all trials had all five treatments so the 'if' conditions filter out trial-treatments with no data
 if (df$immediate_release_number>0){hist(subset(df1, treatment=="0")$length, main=paste("Trial",df$trial_number,"Immediate"),xlab="Length")}
 if (df$X30min_number>0){hist(subset(df1, treatment=="30")$length,main=paste("Trial",df$trial_number,"30 min"),xlab="Length")}
 if (df$X1h_number>0){hist(subset(df1, treatment=="60")$length,main=paste("Trial",df$trial_number,"60 min"),xlab="Length")}
 if ((df$X2h_number>0) & is.na(df$X2h_number)==FALSE){hist(subset(df1, treatment=="120")$length,main=paste("Trial",df$trial_number,"120 min"),xlab="Length")}

 # for one trial, the 90 minute treatment was left for 10 minutes too long and was recorded as 
 #treatment=100. In the 'trial' data it was still recorded as 1h30min. The nested branching solves 
 #this issue
  if (df$X1h30min_numner>0){
  if (nrow(df1[which(df1$treatment=="90"),])>0){hist(subset(df1, treatment=="90")$length,main=paste("Trial",df$trial_number,"90 min"),xlab="Length")}
  if (nrow(df1[which(df1$treatment=="100"),])>0){hist(subset(df1, treatment=="100")$length,main=paste("Trial",df$trial_number,"100 min"),xlab="Length")}
 }
#length histogram for unbanded prawns 
 if (sum(is.na(df1$treatment))>0){hist(df1[is.na(df1$treatment),]$length,main=paste("Trial",df$trial_number,"Unbanded"),xlab="Length")}
 
#Scatter plot showing the proportion of each treatment lost (by losing their band or being lost from the trap) 
 #as a fraction of the number in the treatment group at the start of each trial. 
 
  plot(NULL, ylim=c(0,1),xlim=c(-5,125), main=paste("Trial",df$trial_number,"Proportion of Treatment Lost"), xlab="Treatment Time", ylab="Proportion of Loss" )
 points(0,lost_immediate[i]/(df$immediate_release_number))
 points(30,lost_30[i]/(df$X30min_number))
 points(60,lost_60[i]/(df$X1h_number))
 points(90,lost_90[i]/(df$X1h30min_numner))
 points(120,lost_120[i]/(df$X2h_number))
 
 #number of prawns without length and stage data in each treatment 
 barplot(c(sum(is.na(subset(df1,treatment=="0")$length)),sum(is.na(subset(df1,treatment=="30")$length)), sum(is.na(subset(df1,treatment=="60")$length)),sum(is.na(subset(df1,treatment=="90")$length)),sum(is.na(subset(df1,treatment=="120")$length)), sum(is.na(df1[which(is.na(df1$treatment)),]$length))), names=c("0","30","60","90","120","Unbanded"),xlab="Treatment", ylab="Length NA's", main=paste("Prawns without length data: Trial",df$trial_number),ylim=c(0,20))
 barplot(c(sum(is.na(subset(df1,treatment=="0")$stage)),sum(is.na(subset(df1,treatment=="30")$stage)), sum(is.na(subset(df1,treatment=="60")$stage)),sum(is.na(subset(df1,treatment=="90")$stage)),sum(is.na(subset(df1,treatment=="120")$stage)), sum(is.na(df1[which(is.na(df1$treatment)),]$stage))), names=c("0","30","60","90","120","Unbanded"),xlab="Treatment", ylab="Stage NA's", main=paste("Prawns without stage data: Trial",df$trial_number),ylim=c(0,20))
 barplot(c(df$immediate_release_number, df$X30min_number,df$X1h_number, df$X1h30min_numner,df$X2h_number), names=c("0","30","60","90","120"), ylim = c(0,100))
 
 #end pdf writing
 dev.off()
  
 #record name of pdf (for compiling later)
 file_names[i]<-paste(Sys.Date(), "trial",df$trial_number, "hist.pdf", sep="_")
  
  #The number of prawns of each stage per trial 
  stage_0_per_trial[i]<-nrow(subset(df1, stage==0))
  stage_1_per_trial[i]<-nrow(subset(df1, stage==1))
  stage_2_per_trial[i]<-nrow(subset(df1, stage==2))
  stage_3_per_trial[i]<-nrow(subset(df1, stage==3))
  stage_NA_per_trial[i]<-sum(is.na(df1$stage))
}
df
#remain_90
#alive_90 #100? 
#min surv sum

#prawns lost is the difference between the start number of prawns and the prawns remaining for each trial
lost_prawnz<-pulled-remain

#Combine pdfs of summary graphs and violin plots, respectively, into two multipage pdfs
pdf_combine(input=file_names,output = paste0(Sys.Date(),"_combined_lost_summary.pdf"))
pdf_combine(input=file_names_1,output = paste0(Sys.Date(),"_combined_violins.pdf"))

#Data frame of per trial information
trial_df<-data.frame(trial_number=trial$trial_number, treatments=total_treatments, air_temp= trial$exp_set_temp_air,pulled=pulled, remain=remain, lost=lost_prawnz, uunbanded=unbanded, scavenged=scavenged, dead=dead, alive=alive, stage0=stage_0_per_trial, stage1=stage_1_per_trial,stage2=stage_2_per_trial,stage3=stage_3_per_trial)

#Creates a new column in the summary dataframe showing salinity (at 0m). If salinity was recorded at 
#the start salinity takes that value. If not, salinity at the end is used.
salinity<-trial$exp_set_sal_0m
salinity[is.na(salinity)]<-trial$exp_haul_sal_0m[is.na(salinity)]
trial_df$salinity<-salinity

# The maxima and minima of the sums of the alive, dead and scavenged per trial
max_surv_sum
min_surv_sum

#bar graph of unbanded prawns per trial
setwd(here("figures"))
png(paste(Sys.Date(), "unbanded_bar.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1,res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(unbanded, xlab="Trial", ylab="Unbanded Prawns")
dev.off()

#boxplot of the length distribution for each trial
setwd(here("figures"))
png(paste(Sys.Date(), "length_trial_boxplot.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1,res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
boxplot(lengths_1, lengths_2, lengths_3,lengths_4, lengths_5, lengths_6,lengths_7, lengths_8, lengths_9,lengths_10, lengths_11, lengths_12, lengths_13,lengths_14, lengths_15, lengths_16,lengths_17, lengths_18, lengths_19,lengths_20,lengths_21, names = sort(trial$trial_number))
dev.off()

#Number of each stage per trial
setwd(here("figures"))
png(paste(Sys.Date(), "stage_trial_barplot.png", sep="_"), width=2000, height=2000, units = "px", pointsize=1,res=300)
par(mfrow=c(2,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(stage_0_per_trial, xlab="Trial", ylab="Stage 0 Prawns")
barplot(stage_1_per_trial, xlab="Trial", ylab="Stage 1 Prawns")
barplot( stage_2_per_trial, xlab="Trial", ylab="Stage 2 Prawns")
barplot(stage_3_per_trial, xlab="Trial", ylab="Stage 3 Prawns")
dev.off()

#lost, dead, scavenged, alive prawns per trial
setwd(here("figures"))
png(paste(Sys.Date(), "lost_prawns.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1,res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(lost_prawnz, xlab="Trial", ylab="Lost Prawns")
dev.off()


#Create dataframe with three coloumns: trial number, condition and number in that condition (called "prawns" below)
#There are 63 rows beacause there are 3 condition values (alive, dead and scavenged) times 21 trials. 
 
trial_repeated <- c(rep("1" , 3) , rep("2" , 3) , rep("3" , 3) , rep("4" , 3),rep("5" , 3) , rep("6" , 3) , rep("7" , 3) , rep("8" , 3),rep("9" , 3) , rep("10" , 3) ,rep("13" , 3) , rep("14" , 3) , rep("15" , 3) , rep("16" , 3),rep("17" , 3) , rep("18" , 3) , rep("19" , 3) , rep("20" , 3),rep("21" , 3) , rep("22" , 3) , rep("23" , 3))
condition <- rep(c("Alive" , "Dead" , "Scavenged") , n_trials)
prawns<-vector(mode="numeric", length=n_trials*3)

#for loop to create a vector corresponding to the number of prawns in the given trial and condition from the vectors above
for (i in 1:n_trials){
prawns[3*i-2]<-alive[i]
prawns[3*i-1]<-dead[i]
prawns[3*i]<-scavenged[i]
}
rep_data<- data.frame(trial_repeated,condition,prawns)


#create and save stacked barplot showing the number of each treatment in each trial 
setwd(here("figures"))
png(paste(Sys.Date(), "condition_barplot.png", sep="_"), width=2000, height=2000, units = "px", pointsize=1,res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(rep_data, aes(fill=condition, y=prawns, x=trial_repeated)) + 
  geom_bar(position="stack", stat="identity")+xlab("Trial")+ylab("Prawns")
dev.off()


#Percent lost, dead, scavenged, alive prawns per trial
#NOTE: dead alive and scavenged are shown as proportions of remaining
#(not including lost) while lost is shown as a proportion of the total 
#at the start

trial_df$percent_lost<-(100*lost_prawnz/pulled)
trial_df$percent_dead<-100*trial_df$dead/trial_df$remain
trial_df$percent_alive<-100*trial_df$alive/trial_df$remain
trial_df$percent_scavenged<-100*trial_df$scavenged/trial_df$remain


#Plots of Percent survival vs Temperature 
p1<-ggplot(data=trial_df, aes(x=salinity, y=percent_alive))+geom_point()
p2<-ggplot(data=trial_df, aes(x=temperature, y=percent_alive))+geom_point()


#Plot of lost per treatment over trial
setwd(here("figures"))
png(paste(Sys.Date(), "lost_by_treatment.png", sep="_"), width=4000, height=2000, units = "px", pointsize=1,res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(sort(trial$trial_number), lost_30,col='purple', xlab="Trial", ylab="Lost or Unbanded Prawns")
points(sort(trial$trial_number),lost_120, col='red')
points(sort(trial$trial_number),lost_60, col='blue')
points(sort(trial$trial_number),lost_90, col='green')
points(sort(trial$trial_number),lost_immediate)
legend(20, 20, legend=c("0 min", "30 min","60 min", "90 min","120 min"),col=c("black","purple", "blue","green", "red"),pch=1, cex=1)
dev.off()




#
setwd(here("figures"))
png(paste(Sys.Date(), "lost_vs_quartile.png", sep="_"), width=2000, height=2000, units = "px", pointsize=1,res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(quarts,lost_prawnz, xlab="25th Percentile of Length", ylab="Lost Prawns")
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "lost_by_treatment_barplot.png", sep="_"), width=2000, height=2000, units = "px", pointsize=1,res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(c(sum(lost_immediate), sum(lost_30),sum(lost_60), sum(lost_90),sum(lost_120)), names=c("0","30","60","90","120"), ylab = "Lost Prawns", xlab="Treatment")
dev.off()

#NOTE: for graph below there are ~21x5 points. Adding together 5 from the same trial=1. 
#Points are the proportion of prawns lost from the trial that were lost from that trialxtreatment
setwd(here("figures"))
png(paste(Sys.Date(), "lost_by_treatment_and_trial_points.png", sep="_"), width=2000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(NULL,xlim=c(-5,125),ylim=c(0,1), ylab="Proportion lost or Unbanded", xlab="Treatment Time", main="Proportion of each Treatment Lost")
points(rep(0,21),lost_immediate/(trial$immediate_release_number))
points(rep(30,21),lost_30/(trial$X30min_number))
points(rep(60,21),lost_60/(trial$X1h_number))
points(rep(90,21),lost_90/(trial$X1h30min_numner))
points(rep(120,21),lost_120/(trial$X2h_number))
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "lost_percent_by_treatment_barplots.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(c(sum(lost_immediate)/(sum(lost_prawnz)+sum(unbanded)), sum(lost_30)/(sum(lost_prawnz)+sum(unbanded)),sum(lost_60)/(sum(lost_prawnz)+sum(unbanded)), sum(lost_90)/(sum(lost_prawnz)+sum(unbanded)),sum(lost_120)/(sum(lost_prawnz)+sum(unbanded))),names=c("0","30","60","90","120"), main="Proportion of Lost by Treatment")
barplot(c(sum(lost_immediate)/sum(trial$immediate_release_number), sum(lost_30)/sum(trial$X30min_number),sum(lost_60)/sum(trial$X1h_number), sum(lost_90)/sum(trial$X1h30min_numner),sum(lost_120)/sum(trial$X2h_number)),names=c("0","30","60","90","120"), main="Proportion of each Treatment Lost")
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "lost_percent_by_treatment_barplots.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(c(sum(lost_immediate)/(sum(lost_prawnz)+sum(unbanded)), sum(lost_30)/(sum(lost_prawnz)+sum(unbanded)),sum(lost_60)/(sum(lost_prawnz)+sum(unbanded)), sum(lost_90)/(sum(lost_prawnz)+sum(unbanded)),sum(lost_120)/(sum(lost_prawnz)+sum(unbanded))),names=c("0","30","60","90","120"), main="Proportion of Lost by Treatment")
barplot(c(sum(lost_immediate)/sum(trial$immediate_release_number), sum(lost_30)/sum(trial$X30min_number),sum(lost_60)/sum(trial$X1h_number), sum(lost_90)/sum(trial$X1h30min_numner),sum(lost_120)/sum(trial$X2h_number)),names=c("0","30","60","90","120"), main="Proportion of each Treatment Lost")
dev.off()
#MAKE BIG PDFs

100*(unbanded)/(trial$immediate_release_number+trial$X30min_number+trial$X1h_number+trial$X1h30min_numner+trial$X2h_number)


sum(lost_immediate)/(sum(lost_prawnz)+sum(unbanded))+ sum(lost_30)/(sum(lost_prawnz)+sum(unbanded))+sum(lost_60)/(sum(lost_prawnz)+sum(unbanded))+ sum(lost_90)/(sum(lost_prawnz)+sum(unbanded))+sum(lost_120)/(sum(lost_prawnz)+sum(unbanded))
#Write Trial summary dataframe csv
setwd(here("data-clean"))
write.csv(trial_df,"2023-05-17_trial_summary.csv")


plot(trial_df$air_temp,trial_df$remain, ylim=c(0,350))
points(trial_df$air_temp, trial_df$alive, col="red")

trial_df$alive/trial_df$remain
hist()

opar = par(oma = c(2,2,2,2)) # Large right margin for plot
barplot(c(sum(as.integer(remain_0)),sum(as.integer(remain_30)),sum(as.integer(remain_60)),sum(as.integer(remain_90)),sum(as.integer(remain_120))),col=rgb(0,0,1,1/4), xlab="Time out of Water (minutes)",names.arg = c("0","30","60","90","120"), beside=TRUE)
barplot(c(sum(as.integer(alive_0)),sum(as.integer(alive_30)),sum(as.integer(alive_60)),sum(as.integer(alive_90)),sum(as.integer(alive_120))),col=rgb(0,1,0,1/4),add=T)#, args.legend = list(legend=c("Total Prawns","Surviving Prawns"),col=c("lightblue","purple")))
opar = par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x="right", legend = c("Total Prawns","Surviving Prawns"), fill=c(rgb(0,0,1,1/4),rgb(0,1,0.3,1/4)))
par(opar)

graphics.off()
barplot_df<-t(matrix(c(c(sum(as.integer(alive_0)),sum(as.integer(alive_30)),sum(as.integer(alive_60)),sum(as.integer(alive_90)),sum(as.integer(alive_120))),c(sum(as.integer(remain_0))-sum(as.integer(alive_0)),sum(as.integer(remain_30))-sum(as.integer(alive_30)),sum(as.integer(remain_60))-sum(as.integer(alive_60)),sum(as.integer(remain_90))-sum(as.integer(alive_90)),sum(as.integer(remain_120))-sum(as.integer(alive_120)))), nrow=5))
colnames(barplot_df)<-c("0","30","60","90","120")
barplot(barplot_df, beside=FALSE, col=c("lightblue", "pink"),legend=c("Surviving Prawns","Total Prawns"),names.arg=c("0","30","60","90","120"),xlab="Time out of Water (minutes)")
barplot_df2<-matrix(c(barplot_df[1,]/(barplot_df[1,]+barplot_df[2,])),nrow=1)
barplot_df2<-rbind(barplot_df2,c(1-c(barplot_df[1,]/(barplot_df[1,]+barplot_df[2,]))))
colnames(barplot_df2)<-c("0","30","60","90","120")
barplot(barplot_df2, beside=FALSE, col=c("lightblue", "pink"),legend=c("Surviving Prawns","Total Prawns"),names.arg=c("0","30","60","90","120"),xlab="Time out of Water (minutes)")

