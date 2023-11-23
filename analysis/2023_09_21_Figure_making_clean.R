#Read in package
library(qpdf)
library(viridis)
library(here)
library(ggplot2)
library(dplyr)


#set working directory
setwd(here("data-clean"))
setwd("/Users/jacobhoutman/Documents/Git Hub/Prawnz-survival-experiment/data-clean")


#read in data
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")


#Order dataframe
trial<-trial[order(trial$trial_number),]

#Record the number of trials
n_trials<-length(unique(trial$trial_number))

#Set up empty vectors with one element for each trial
total_traps<-trial_number<-lost_prawnz<-total_treatments<-vector(mode="numeric", length=n_trials)
unbanded<-scavenged<-dead<-alive<-vector(mode="numeric", length=n_trials)
max_surv_sum<-min_surv_sum<-pulled<-remain<-quarts<-vector(mode="numeric", length=n_trials)
stage_0_per_trial<-stage_1_per_trial<-stage_2_per_trial<-stage_3_per_trial<-stage_NA_per_trial<-vector(mode="numeric", length=n_trials)
lost_immediate<-lost_30<-lost_60<-lost_90<-lost_120<-vector(mode="numeric", length=n_trials)
file_names<-file_names_1<-vector(mode="character", length=n_trials)
alive_0<-alive_30<-alive_60<-alive_90<-alive_120<-vector(mode="integer", length=n_trials)
remain_0<-remain_30<-remain_60<-remain_90<-remain_120<-vector(mode="integer", length=n_trials)


#'For' loop, looping once for each trial number
for (i in 1:n_trials){
  
  #Trial contains trial-level information like temperature, salinity and location.
  #This line makes a data frame (with only one row) with information for the trial 
  #specified by the i index. 
  #NOTE: The trial number specified by "sort(trial$trial_number)[i]" is not equal to the index 'i'.
  #It is for trials 1-10; however, trials number 11 and 12 were omitted. 
  #Trial numbers 13-23 correspond to i values of 11-21
  df<-subset(trial, trial_number==sort(trial$trial_number)[i])
  
  #Survival contains information on individual prawns like carapace length, treatment, and whether it survived.
  #This line subsets the survival data frame (with one row for each prawn in the trial) to only the prawns
  #in the current trial number (specified by "sort(trial$trial_number)[i]").
  df1<-subset(survival, survival$trial_number==sort(trial$trial_number)[i])
  
  #Reflex scores for this trial
  df2<-subset(reflexes, reflexes$trial_number==sort(trial$trial_number)[i])
  
  #the number of scavenged,dead and alive prawns at the end of each trial 
  scavenged[i]<-sum(df1$scavenged)
  dead[i]<-sum(df1$dead)
  alive[i]<-sum(df1$alive)
  
  #The trial data contains the number of prawns in each treatment at the beginning of the treatment.
  # The sum of the treatment numbers is the total prawns at the start of the trial.
  pulled[i]<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number))
  
  #df1 contains a row of data for each prawn,including dead, scavenged, and alive,
  #remaining in the trap after the release stage. 
  #The number of rows is the remaining prawns after the release stage.
  remain[i]<-nrow(df1)
  
  #unbanded prawns are entered as treatment=NA so the sum of these, per trial
  #is the number of prawns that lost their band per trial
  unbanded[i]<-length(which(is.na(df1$treatment)))
  
  #Lost prawns in each treatment, calculated as the number in the treatment group at the start minus the
  #number of prawns with that colour band after the release stage. The difference is therefore prawns lost
  #from each treatment. However prawns could be lost in two ways: by losing their band, or by leaving the trap.
  #On a treatment level, the number of prawns that lost there band and truly lost prawns (no longer in the trap) 
  #cannot be differentiated. These vectors are the sum of these two quantites: prawns that lost their band and truly lost prawns.
  lost_immediate[i]<-df$immediate_release_number-length(which(df1$treatment=="0"))
  lost_30[i]<-df$X30min_number-length(which(df1$treatment=="30"))
  lost_60[i]<-df$X1h_number-length(which(df1$treatment=="60"))
  
  #There was one trial with 100 minutes rather than 90. It was still recorded in '1h30min_numner'. 
  lost_90[i]<-df$X1h30min_numner-length(which(df1$treatment=="90"))-length(which(df1$treatment=="100"))
  lost_120[i]<-df$X2h_number-length(which(df1$treatment=="120"))
  
  #record the 1st quartile for carapace length per trial
  quarts[i]<-quantile(df1$length,probs = .25,na.rm = TRUE)
  
  
  #the maxima and minima of the sums of three survival values for each prawn for a trial. 
  #The sums should all be 1, since the categories are mutually exclusive and . Used for checking data was 
  #entered correctly
  max_surv_sum[i]<-max(df1$alive+df1$dead+df1$scavenged)
  min_surv_sum[i]<-min(df1$alive+df1$dead+df1$scavenged)
  
  #number of different treatments for each trial
  #Prawns that lost their band were recorded as treatment==NA.
  #This line removes the NAs and counts the remaining treatment values (should be a max of 5 for 5 treatment times)
  total_treatments[i]<-length(unique(subset(df1,!is.na(df1$treatment))$treatment))
  
  #Count the number of traps in each trial
  total_traps[i]<-length(unique(df1$trap_number))
  
  #the remaining prawns after each treatment 
  remain_0[i]<-length(which(df1$treatment==0))
  remain_30[i]<-length(which(df1$treatment==30))
  remain_60[i]<-length(which(df1$treatment==60))
  remain_90[i]<-length(which(df1$treatment==90))+length(which(df1$treatment==100))
  remain_120[i]<-length(which(df1$treatment==120))
  
  #alive prawns after each treatment 
  alive_0[i]<-sum(df1[which(df1$treatment==0),]$alive)
  alive_30[i]<-sum(df1[which(df1$treatment==30),]$alive)
  alive_60[i]<-sum(df1[which(df1$treatment==60),]$alive)
  alive_90[i]<-sum(df1[which(df1$treatment==90),]$alive)+sum(df1[which(df1$treatment==100),]$alive)
  alive_120[i]<-sum(df1[which(df1$treatment==120),]$alive)
  
  #The number of prawns of each stage per trial 
  stage_0_per_trial[i]<-nrow(subset(df1, stage==0))
  stage_1_per_trial[i]<-nrow(subset(df1, stage==1))
  stage_2_per_trial[i]<-nrow(subset(df1, stage==2))
  stage_3_per_trial[i]<-nrow(subset(df1, stage==3))
  stage_NA_per_trial[i]<-sum(is.na(df1$stage))
  
  #change working directory
  #setwd(here("New-figures"))
  
  ##Length Violin Plots----
  #Create a data frame with two coloumns: length and sex. Sex (or stage) is 0 for juvenile,
  # 1 for male, 2 for transitional and 3 for female. Length refers to carapace length. 
  # The length column is all the length for stage 0 and 1 prawns followed by all the stage 2 and 3 prawns.
  # The stage column contains the stage of the prawn: "Juv/Male" or "Trans/Female".
  # violin_length<-data.frame(length=append(df1[which(df1$stage<=1),]$length,df1[which(df1$stage>1),]$length),
  #                           stage=append(rep("Juv/Male",length(df1[which(df1$stage<=1),]$length)),
  #                           rep("Trans/Female", length(df1[which(df1$stage>1),]$length))))
  
  #Stage as a factor
  #violin_length$stage<-as.factor(violin_length$stage)
  
  #violin plot comparing length distributions for juveniles and males vs transitionals and females
  # ggplot(violin_length, aes(x=stage, y=length)) + geom_violin()+ylim(15,55)
  
  #save plot to file
  #ggsave(paste(Sys.Date(), "trial",df$trial_number, "violin.pdf", sep="_"))
  
  #Record file name of the violin plot above. This will be used to combine them into one large PDF.
  #file_names_1[i]<-paste(Sys.Date(), "trial",df$trial_number, "violin.pdf", sep="_")
  
  ##Summary graphs----
  #Create a pdf (for each trial) with up to twelve summary graphs on it
  #the graphs created below this line will be added to the 
  # pdf(paste(Sys.Date(), "trial",df$trial_number, "hist.pdf", sep="_"), width=7, height=7, pointsize=12)
  #par(mfrow=c(6,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
  
  # histogram of length data in the trial
  #hist(df1$length,main=paste("Trial",df$trial_number,"Total"),xlab="Length")
  
  # histograms of lengths in each treatment for that trial 
  # not all trials had all five treatments so the 'if' conditions filter out trial-treatments with no data
  # if (df$immediate_release_number>0){hist(subset(df1, treatment=="0")$length, main=paste("Trial",df$trial_number,
  #                                 "Immediate"),xlab="Length")}
  # if (df$X30min_number>0){hist(subset(df1, treatment=="30")$length,main=paste("Trial",df$trial_number,"30 min"),
  #                      xlab="Length")}
  # if (df$X1h_number>0){hist(subset(df1, treatment=="60")$length,main=paste("Trial",df$trial_number,"60 min"),
  #                       xlab="Length")}
  # if ((df$X2h_number>0) & is.na(df$X2h_number)==FALSE){hist(subset(df1, treatment=="120")$length,
  #                    main=paste("Trial",df$trial_number,"120 min"),xlab="Length")}
  
  
  # for one trial, the 90 minute treatment was left for 10 minutes too long and was recorded as 
  #treatment=100. In the 'trial' data it was still recorded as 1h30min. The nested branching solves 
  #this issue 
  # if (df$X1h30min_numner>0){
  # if (nrow(df1[which(df1$treatment=="90"),])>0){hist(subset(df1, treatment=="90")$length,
  #                             main=paste("Trial",df$trial_number,"90 min"),xlab="Length")}
  #if (nrow(df1[which(df1$treatment=="100"),])>0){hist(subset(df1, treatment=="100")$length,
  #                           main=paste("Trial",df$trial_number,"100 min"),xlab="Length")}
  #}
  #length histogram for unbanded prawns 
  # if (sum(is.na(df1$treatment))>0){hist(df1[is.na(df1$treatment),]$length,main=paste("Trial",
  #                              df$trial_number,"Unbanded"),xlab="Length")}
  
  #Scatter plot showing the proportion of each treatment lost (by losing their band or being lost from the trap) 
  #as a fraction of the number in the treatment group at the start of each trial. 
  
  #plot(NULL, ylim=c(0,1),xlim=c(-5,125), main=paste("Trial",df$trial_number,"Proportion of Treatment Lost"), xlab="Treatment Time", ylab="Proportion of Loss" )
  # points(0,lost_immediate[i]/(df$immediate_release_number))
  # points(30,lost_30[i]/(df$X30min_number))
  # points(60,lost_60[i]/(df$X1h_number))
  #points(90,lost_90[i]/(df$X1h30min_numner))
  # points(120,lost_120[i]/(df$X2h_number))
  
  #number of prawns without length data in each treatment 
  #barplot(c(sum(is.na(subset(df1,treatment=="0")$length)),sum(is.na(subset(df1,treatment=="30")$length)), 
  #         sum(is.na(subset(df1,treatment=="60")$length)),sum(is.na(subset(df1,treatment=="90")$length)),
  #        sum(is.na(subset(df1,treatment=="120")$length)), sum(is.na(df1[which(is.na(df1$treatment)),]$length))), 
  #     names=c("0","30","60","90","120","Unbanded"),xlab="Treatment", ylab="Length NA's", 
  #    main=paste("Prawns without length data: Trial",df$trial_number),ylim=c(0,20))
  
  #number of prawns without stage data in each treatment 
  # barplot(c(sum(is.na(subset(df1,treatment=="0")$stage)),sum(is.na(subset(df1,treatment=="30")$stage)), 
  #         sum(is.na(subset(df1,treatment=="60")$stage)),sum(is.na(subset(df1,treatment=="90")$stage)),
  #         sum(is.na(subset(df1,treatment=="120")$stage)), sum(is.na(df1[which(is.na(df1$treatment)),]$stage))), 
  #         names=c("0","30","60","90","120","Unbanded"),xlab="Treatment", ylab="Stage NA's", 
  #         main=paste("Prawns without stage data: Trial",df$trial_number),ylim=c(0,20))
  
  #prawns in each treatment group at the beginning of the trial
  #barplot(c(df$immediate_release_number, df$X30min_number,df$X1h_number, df$X1h30min_numner,df$X2h_number), 
  #        names=c("0","30","60","90","120"), ylim = c(0,100), main=paste("Prawns in each treatment group in trial",df$trial_number))
  
  #
  #hist(df2$score, main=paste("Reflex scores in trial",df$trial_number), xlab="Reflex Score")
  
  #end pdf writing
  # dev.off()
  
  #record name of pdf (for compiling later)
  # file_names[i]<-paste(Sys.Date(), "trial",df$trial_number, "hist.pdf", sep="_")
}

#Combine PDFs of summary graphs and violin plots, respectively, into two multi-page PDFs
setwd(here("figures"))
pdf_combine(input=file_names,output = paste0(Sys.Date(),"_combined_lost_summary.pdf"))
pdf_combine(input=file_names_1,output = paste0(Sys.Date(),"_combined_violins.pdf"))


#prawns lost is the difference between the start number of prawns and the prawns 
#remaining for each trial.
lost_prawnz<-pulled-remain

#Data frame of per trial information
trial_df<-data.frame("Trial number"=trial$trial_number, "Immediate"=trial$immediate_release_number, "30 min"=trial$X30min_number,"60 min"=trial$X1h_number,"90 min"=trial$X1h30min_numner,"120 min"=trial$X2h_number,
                     "Total"=pulled
                     , "Traps"=total_traps, "Remain"=remain, "Lost"=lost_prawnz, 
                     "Unbanded"=unbanded, "Scavenged"=scavenged, "Dead"=dead, "Alive"=alive, "Stage 0"=stage_0_per_trial, 
                     "Stage 1"=stage_1_per_trial,"Stage 2"=stage_2_per_trial, "Stage 3"=stage_3_per_trial,"Air temp"= trial$exp_set_temp_air)


#Creates a new column in the summary dataframe showing salinity (at 0m). If salinity was recorded at 
#the start salinity takes that value. If not, salinity at the end is used.
salinity<-trial$exp_set_tote_sal
salinity[is.na(salinity)]<-trial$exp_set_sal_0m[is.na(salinity)]
trial_df$salinity<-salinity

#Save summary trial information
setwd(here("figures"))
write.csv(trial_df, paste(Sys.Date(),"trial_summary.csv"))

# The maxima and minima, and NAs, of the sums of the alive, dead and scavenged per trial
#This should be 1
max(survival$alive+survival$dead+survival$scavenged)
#This should be 0
min(survival$alive+survival$dead+survival$scavenged)
#This should be 0
sum(is.na(survival$alive+survival$dead+survival$scavenged))

#Bar graph of unbanded prawns per trial. 'unbanded' refers to the number of prawns with
#no rostrum band after the trial
#Figure S1, Supplement 1
setwd(here("New-figures"))
tiff(paste(Sys.Date(), "unbanded_bar.tiff", sep="_"), width=800, height=600, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(unbanded, xlab="Trial number", ylab="Unbanded prawns", names=as.character(unique(trial$trial_number)), col=viridis(1,0.8))
dev.off()

#Number of each stage per trial
#Figure S2, Supplement 2
setwd(here("New-figures"))
tiff(paste(Sys.Date(), "stage_trial_barplot.tiff", sep="_"), width=1000, height=1000, units = "px", pointsize=12)
par(mfrow=c(2,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(stage_0_per_trial,names=as.character(unique(trial$trial_number)), xlab="Trial number", ylab="Number of prawns",main="Juvenile prawns",col=viridis(4)[1],ylim=c(0,230),cex.lab=1.4,cex.main=1.6,cex.axis = 0.9,cex.names=0.9)
barplot(stage_1_per_trial, names=as.character(unique(trial$trial_number)),xlab="Trial number", ylab="Number of prawns",main="Male prawns",col=viridis(4)[2],ylim=c(0,230),cex.lab=1.4,cex.main=1.6,cex.axis = 0.9,cex.names=0.9)
barplot( stage_2_per_trial, names=as.character(unique(trial$trial_number)),xlab="Trial number", ylab="Number of prawns",main="Transitional prawns",col=viridis(4)[3],ylim=c(0,230),cex.lab=1.4,cex.main=1.6,cex.axis = 0.9,cex.names=0.9)
barplot(stage_3_per_trial, names=as.character(unique(trial$trial_number)),xlab="Trial number", ylab="Number of prawns",main="Female prawns",col=viridis(4)[4],ylim=c(0,230),cex.lab=1.4,cex.main=1.6,cex.axis = 0.9,cex.names=0.9)
dev.off()

#Lost prawns per trial
#Figure S4, Supplement 4
setwd(here("New-figures"))
tiff(paste(Sys.Date(), "lost_prawns.tiff", sep="_"), width=800, height=600, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(lost_prawnz, ylim=c(-5,70),names=as.character(unique(trial$trial_number)), xlab="Trial number", ylab="Lost prawns",col=viridis(4)[1])
dev.off()

##Stacked condition bar-plot ----
#Create dataframe with three coloumns: trial number, condition, and number in that condition (called "prawns" below)
#There are 63 rows beacause there are 3 condition values (alive, dead and scavenged) times 21 trials. 

#Create trial number
trial_repeated <- c(rep(" 1" , 3) , rep(" 2" , 3) , rep(" 3" , 3) , rep(" 4" , 3),
                    rep(" 5" , 3) , rep(" 6" , 3) , rep(" 7" , 3) , rep(" 8" , 3),
                    rep(" 9" , 3) , rep("10" , 3) ,rep("13" , 3) , rep("14" , 3) ,
                    rep("15" , 3) , rep("16" , 3),rep("17" , 3) , rep("18" , 3) ,
                    rep("19" , 3) , rep("20" , 3),rep("21" , 3) , rep("22" , 3) ,
                    rep("23" , 3))

#Condition of prawns
Condition <- rep(c("Alive" , "Dead" , "Scavenged") , n_trials)

#empty vector where number of prawns in each trial and condition will be added
prawns<-vector(mode="numeric", length=n_trials*3)

#create a vector corresponding to the number of prawns in the given trial and condition 
for (i in 1:n_trials){
  # 3*i -2 where i is 1-21 gives 1, 4, 7, etc
  prawns[3*i-2]<-alive[i]
  
  # 3*i -1 where i is 1-21 gives 2, 5, 8, etc
  prawns[3*i-1]<-dead[i]
  
  # 3*i where i is 1-21 gives 3, 6, 9, etc
  prawns[3*i]<-scavenged[i]
}

#data frame of 
rep_data<- data.frame(trial_repeated,Condition,prawns)


#create and save stacked barplot showing the number of each treatment in each trial
#Figure S6, Supplement 6
setwd(here("New-figures"))
tiff(paste(Sys.Date(), "condition_barplot.tiff", sep="_"), width=800, height=800, res=150,units = "px", pointsize=10)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(rep_data, aes(fill=Condition, y=prawns, x=trial_repeated)) + 
  geom_bar(position="stack", stat="identity")+xlab("Trial")+ylab("Prawns")+
  theme(panel.border=element_rect(fill=NA),panel.background = element_rect(fill = "white", colour = "grey50"))+scale_fill_viridis(discrete = T,direction=1) 
dev.off()


#Percent lost, dead, scavenged, alive prawns per trial
#NOTE: dead alive and scavenged are shown as proportions of remaining
#(not including lost) while lost is shown as a proportion of the total 
#at the start
#DELETE??
trial_df$percent_lost<-(100*lost_prawnz/pulled)
trial_df$percent_dead<-100*trial_df$dead/trial_df$remain
trial_df$percent_alive<-100*trial_df$alive/trial_df$remain
trial_df$percent_scavenged<-100*trial_df$scavenged/trial_df$remain


#Plots of Percent survival vs Temperature 
p1<-ggplot(data=trial_df, aes(x=salinity, y=percent_alive))+geom_point()
p2<-ggplot(data=trial_df, aes(x=temperature, y=percent_alive))+geom_point()


#Plot of lost per treatment over trial
setwd(here("New-figures"))
tiff(paste(Sys.Date(), "lost_by_treatment.tiff", sep="_"), width=800, height=600, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(sort(trial$trial_number), lost_30,col=viridis(5)[2], xlab="Trial", ylab="Lost or unbanded prawns", ylim=c(-5,50), main="Prawns lost from each Treatment")
points(sort(trial$trial_number),lost_120, col=viridis(5)[5])
points(sort(trial$trial_number),lost_60, col=viridis(5)[3])
points(sort(trial$trial_number),lost_90, col=viridis(5)[4])
points(sort(trial$trial_number),lost_immediate,col=viridis(5)[1])
legend(18, 50, legend=c("0 min", "30 min","60 min", "90 min","120 min"),col=viridis(5),pch=1, cex=0.8)
dev.off()

#DELETE?
setwd(here("figures"))
png(paste(Sys.Date(), "lost_vs_quartile.png", sep="_"), width=800, height=600, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(quarts,lost_prawnz, xlab="25th Percentile of Length", ylab="Lost Prawns", col=viridis(1), ylim=c(-10,80))
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "lost_by_treatment_barplot.png", sep="_"), width=800, height=600, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(c(sum(lost_immediate), sum(lost_30),sum(lost_60), sum(lost_90),sum(lost_120)), main="Lost prawns in each treatment",names=c("0","30","60","90","120"), ylab = "Lost Prawns", xlab="Treatment time ",ylim=c(0,250),col=viridis(1, 0.5))
dev.off()

#NOTE: for graph below there are ~21x5 points. Adding together 5 from the same trial=1. 
#Points are the proportion of prawns lost from the trial that were lost from that trialxtreatment, 
#hence they could be unbanded or truly lost.
#Figure S5, Supplement 5
setwd(here("New-figures"))
tiff(paste(Sys.Date(), "lost_percent_by_treatment_barplots.tiff", sep="_"), width=800, height=600, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(c(sum(lost_immediate)/sum(trial$immediate_release_number), sum(lost_30)/sum(trial$X30min_number),
          sum(lost_60)/sum(trial$X1h_number), sum(lost_90)/sum(trial$X1h30min_numner),
          sum(lost_120)/sum(trial$X2h_number)),ylim=c(-0.03,1),names=c("0","30","60","90","120"),col=viridis(1,0.7),
        xlab="Treatment time (minutes out of water)",)
points(rep(0.7,21),lost_immediate/(trial$immediate_release_number), col=viridis(1))
points(rep(1.92,21),lost_30/(trial$X30min_number), col=viridis(1))
points(rep(3.1,21),lost_60/(trial$X1h_number), col=viridis(1))
points(rep(4.3,21),lost_90/(trial$X1h30min_numner), col=viridis(1))
points(rep(5.5,21),lost_120/(trial$X2h_number), col=viridis(1))
dev.off()



barplot_df<-t(matrix(c(c(sum(as.integer(alive_0)),sum(as.integer(alive_30)),
                         sum(as.integer(alive_60)),sum(as.integer(alive_90)),
                         sum(as.integer(alive_120))),
                       c(sum(as.integer(remain_0))-sum(as.integer(alive_0)),
                         sum(as.integer(remain_30))-sum(as.integer(alive_30)),
                         sum(as.integer(remain_60))-sum(as.integer(alive_60)),
                         sum(as.integer(remain_90))-sum(as.integer(alive_90)),
                         sum(as.integer(remain_120))-sum(as.integer(alive_120)))),
                     nrow=5))
colnames(barplot_df)<-c("0","30","60","90","120")
setwd("/Users/jacobhoutman/Documents/Git Hub/Prawnz-survival-experiment/New-figures")

barplot_df2<-matrix(c(barplot_df[1,]/(barplot_df[1,]+barplot_df[2,])),nrow=1)
barplot_df2<-rbind(barplot_df2,c(1-c(barplot_df[1,]/(barplot_df[1,]+barplot_df[2,]))))
colnames(barplot_df2)<-c("0","30","60","90","120")

setwd(here("New-figures"))

#Figure 3- Survival by Treatment ----
treatments<-c(0,30,60,90,100,120)
means<-vector(length = 5)
lower_95<-vector(length = 5)
upper_95<-vector(length = 5)
sum(is.na(model_df_2$alive))
for (i in 1:5){
  
  if (i %in% c(1,2,3)){
    
    #This is for the 0, 30, and 60 minute treamtent times
    current_data<-model_df_2[which(model_df_2$treatment==treatments[i]),]$alive
    means[i]<-mean(current_data)
    boot<-vector(length=1000)
    
    #Bootstrapped confidence interval
    for (k in 1:1000){
      
      # current_data contains one entry (1 or 0/alive or dead) for each prawn in the treatment group.
      
      #This line samples, with replacement, from those survival values.
      #It takes a number of samples equal to the number of prawns in the treatment group and stores 
      #the mean (average survival) in the boot vector.
      boot[k]<-mean(sample(current_data,replace=TRUE,size=length(current_data)))
    }
    
    #These lines store the 5th and 95th percentiles, respectively, of the bootstrapped survival averages.
    #the 1-3rd elements of these correspond to 0, 30, and 60 min treatments.
    lower_95[i]<-quantile(boot,probs=0.025)
    upper_95[i]<-quantile(boot,probs=0.975)
    
    #End of 0, 30, 60 minute branch
  }
  
  if (i == 4){
    
    #This is for the 90 and 100 minute treatment times (shown together)
    current_data<-model_df_2[c(which(model_df_2$treatment==treatments[4]),which(model_df_2$treatment==treatments[5])),]$alive
    means[i]<-mean(current_data)
    boot<-vector(length=1000)
    
    #Bootstrapped confidence interval
    for (k in 1:1000){
      
      # current_data contains one entry (1 or 0/alive or dead) for each prawn in the 90 and treatment group.
      
      #This line samples, with replacement, from those survival values.
      #It takes a number of samples equal to the number of prawns in the treatment group and stores 
      #the mean (average survival) in the boot vector.
      boot[k]<-mean(sample(current_data,replace=TRUE,size=length(current_data)))
    }
    
    #These lines store the 5th and 95th percentiles, respectively, of the bootstrapped survival averages.
    #the 4th element of these vectors correspond to prawns in the 90 and 100 minute treatments.
    lower_95[i]<-quantile(boot,probs=0.025)
    upper_95[i]<-quantile(boot,probs=0.975)
    
    #End of 90 and 100 minute branch
  }
  
  
  if (i == 5){
    
    #This is for the 120 minute treatment time
  
    current_data<-model_df_2[which(model_df_2$treatment==treatments[6]),]$alive
    means[i]<-mean(current_data)
    boot<-vector(length=1000)
    
    #Bootstrapped confidence interval
    for (k in 1:1000){
      
      # current_data contains one entry (1 or 0/alive or dead) for each prawn in the 90 and treatment group.
      
      #This line samples, with replacement, from those survival values.
      #It takes a number of samples equal to the number of prawns in the treatment group and stores 
      #the mean (average survival) in the boot vector.
      boot[k]<-mean(sample(current_data,replace=TRUE,size=length(current_data)))
    }
    
    #These lines store the 5th and 95th percentiles, respectively, of the bootstrapped survival averages.
    #the 5th element of these correspond to 120 min treatments.
    lower_95[i]<-quantile(boot,probs=0.025)
    upper_95[i]<-quantile(boot,probs=0.975)
    
    #End of 120 minute branch
  }
#End of for loop
}


barplot_df3<-data.frame("Time out of water"=c(0,30,60, 90,120), "Percent alive"=means*100, "Upper"=upper_95*100, "Lower"=lower_95*100)

p<-ggplot(barplot_df3, aes(x=Time.out.of.water, y=Percent.alive, fill=viridis(1)))+
  geom_bar(stat="identity", color=viridis(1,0.9 )[1], show.legend=FALSE)+
  geom_errorbar(aes(ymin=lower_95*100, ymax=upper_95*100), width=.4,
                position=position_dodge(.9))+ylim(0,100)

tiff(paste(Sys.Date(), "survival_treatment_barplot.tiff", sep="_"), width=800, height=800, res=200,units = "px", pointsize=8)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

p+labs( x="Time out of water", y = "Percent alive")+
  scale_fill_manual(values=viridis(1,0.9))+
  theme_minimal()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name= "Time out of water (minutes)",labels = c("0","30","60","90","120"),breaks=c(0,30,60,90,120))
dev.off()


##Survival Histogram Figures----
setwd(here("data-clean"))

model_df_2<-read.csv("2023_08_10_model_dataframe")

cols=viridis(2,0.25)

living<-model_df_2[which(model_df_2$alive==1),]
not_living<-model_df_2[which(model_df_2$alive==0),]


##Length
hgA <- hist(living$length, breaks = pretty(17.5:53.5, n = 69), col = cols[1],plot = FALSE)
hgB <- hist(not_living$length, breaks = pretty(17.5:53.5, n = 69), col = cols[2],plot = FALSE)
hgA$mids
length(hgA$counts)/(hgA$counts+hgB$counts)
length(pretty(17.5:53.5, n = 69))



cols=viridis(2)

#Figure 2, Temperature survival histogram----
##Temp
hist_temps<-hist(model_df_2$temp,breaks=10:26, plot=FALSE)
  
#store number of temperature bins
n_temp_bins<-length(hist_temps$breaks)-1
  
  #create a 16 x 4 matrix 
  #16 rows for each temperature bin
  #4 columns for middle of temperature bin, mean, upper CI limit, mean, and lower CI limit
  
CI_matrix<-matrix(0, n_temp_bins,4)
  
  #We are going to repeat the following procedure for each temperature bin 
  for (i in 1:n_temp_bins){
    
    #Start with Length data in given length bin:
    temp_mid<-hist_temps$mids[i]
    
    #Temperature boundaries for the temperature bin in this iteration of the 'for' loop.
    temp_bound<-hist_temps$breaks[c(i,i+1)]
    
    #Subset data to current temperature bin
    temp_bin_data<-model_df_2[between(model_df_2$temp,temp_bound[1],temp_bound[2]),]$alive
    if (length(temp_bin_data>0)){
    #A vector to store sampled survival proportions
    boot<-vector(length=1000)
      
      #Sample from calculated survival proportions, with replacement
      for (k in 1:1000){
        
        #survival_props contains one entry for for each trial-trap in the temperature bin.
        #There were 123 trial-trap levels total, some temperature bins may have over 60, some only 12.
        #the length of the survival_props vector equals the value of n_trial_traps
        
        #This line samples, with replacement, from the survival proportions calculated for each trial-trap level in the temperature bin.
        #It takes a number of samples equal to the nummber of trial-trap levels and stores it in the 'boot' vector
        boot[k]<-mean(sample(temp_bin_data,replace=TRUE,size=length(temp_bin_data)))
      }
    
      #
      row_number<-i
      
      #Store temperature bin, specifically the middle of the bin
      CI_matrix[row_number,1]<-temp_mid
      
      #Store mean for this temperature bin
      CI_matrix[row_number,2]<-mean(temp_bin_data)*100
      
      #Store 97.5th survival percentile for upper CI limit
      CI_matrix[row_number,3]<-quantile(boot, probs=0.975)*100
      
      #Store 2.5th survival percentile for upper CI limit
      CI_matrix[row_number,4]<-quantile(boot, probs=0.025)*100
  }
    }

barplot_df4<-data.frame("Temperature\u00B0C"=CI_matrix[c(1:6,8,9,13,16),1], "Percent alive"=CI_matrix[c(1:6,8,9,13,16),2], "Upper"=CI_matrix[c(1:6,8,9,13,16),3], "Lower"=CI_matrix[c(1:6,8,9,13,16),4])

p<-ggplot(barplot_df4, aes(x=Temperature.C, y=Percent.alive, fill=viridis(1)))+
  geom_bar(stat="identity", color=viridis(1,0.9 )[1], show.legend=FALSE)+
  geom_errorbar(aes(ymin=CI_matrix[c(1:6,8,9,13,16),4], ymax=CI_matrix[c(1:6,8,9,13,16),3]), width=.2,
                position=position_dodge(.9))+ylim(0,100)

tiff(paste(Sys.Date(), "survival_temp_barplot.tiff", sep="_"), width=800, height=800, res=200,units = "px", pointsize=8)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

p+labs( x="Temperature\u00B0C", y = "Percent alive")+
  scale_fill_manual(values=viridis(1,0.9))+
  theme_minimal()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()
getwd()

l<-hist(living$temp,breaks=pretty(10:26, n = 32),plot=FALSE)$counts
nl<-hist(not_living$temp,breaks=pretty(10:26, n = 32),plot=FALSE)$counts
total<-hist(model_df_2$temp,breaks=pretty(10:26, n = 32),plot=FALSE)$counts
barplot(l/total,space=pretty(10:26, n = 32), width=5)
multihist(l, breaks=pretty(10:26, n = 16),col = cols,xlab="Temperature (\u00B0C)",ylab="Number of prawns",width=2)
legend("topright",c("Living", "Dead"),fill=cols, cex=1.3)



##Length Figure----

survival[which(survival$stage==1),]


s_l_1 <- list(survival[which(survival$alive==1),]$length,survival[which(survival$alive==0),]$length)

plotrix::multhist(s_l_1, breaks = pretty(17.5:53.5, n = 69),beside=FALSE,col = cols,main=NULL, 
                  xlab="Length (mm)", ylab="Number of prawns")




s0<-survival[which(survival$stage==0),]
s1<-survival[which(survival$stage==1),]
s2<-survival[which(survival$stage==2),]
s3<-survival[which(survival$stage==3),]
sna<-survival[is.na(survival$stage),]

s_l <- list(s0$length,s1$length,s2$length,s3$length, sna$length)

cols<-viridis(5)


#Figure 5, Length histograms----
setwd(here("New-figures"))
setwd("/Users/jacobhoutman/Documents/Git Hub/Prawnz-survival-experiment/New-figures")

tiff(paste(Sys.Date(), "length_histograms.tiff", sep="_"), width=800, height=600, units = "px", pointsize=12)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

cols<-viridis(2,0.7)
plot(hgA, col = cols[1], xlab="Length (mm)", ylab="Number of prawns", xaxt='n',main=NULL) # Plot 1st histogram using a transparent color
plot(hgB, col = cols[2], add = TRUE, main=NULL) 
axis(1,pos=0,tck=0,seq(18,52))

#lines(hgA$mids,pmax(hgA$counts,hgB$counts)*hgA$counts/(hgA$counts+hgB$counts), lwd=2)

legend('topright',c("Living", "Dead"),fill=c(cols[1],cols[2]))
#legend(45,80,c("Fraction Alive"),lwd=2,lty=1,col="black")

cols<-viridis(5)
plotrix::multhist(s_l, breaks = pretty(17.5:53.5, n = 69), xaxt='n',beside=FALSE,col = cols,main=NULL, 
                  xlab="Length (mm)", ylab="Number of prawns", main=NULL)

#abline(v=36,h=, lty=2, lwd=2)
legend('topright',c("Juvenile", "Male",'Transitional','Female', 'Unknown'),fill=cols)
dev.off()



getwd()





#Reflex histogram
reflexes$score<-rowSums(reflexes[7:16])
abc<-hist(reflexes$score,plot=FALSE)
abc$counts

numbers=abc$counts
percents=rev(c(1,2,4,10,28,60, 84, 96, 98, 99)/100)

sum(numbers*percents)

abc<-hist(rep(10,length(reflexes$score))-reflexes$score, col="hotpink", xlab="Impairment", ylab="Number of Prawns", main="Reflex score Distribution")

t0<-subset(reflexes,reflexes$treatment==0)$score
t30<-subset(reflexes,reflexes$treatment==30)$score
t60<-subset(reflexes,reflexes$treatment==60)$score
t90<-subset(reflexes,reflexes$treatment==90)$score
t120<-subset(reflexes,reflexes$treatment==120)$score

t0_pr_mort<-sum(hist(t0,breaks=10,plot=TRUE)$counts*percents)
t30_pr_mort<-sum(hist(t30,breaks=10,plot=FALSE)$counts*percents)
t60_pr_mort<-sum(hist(t60,breaks=10,plot=FALSE)$counts*percents)
t90_pr_mort<-sum(hist(t90,breaks=10,plot=FALSE)$counts*percents)
t120_pr_mort<-sum(hist(t120,breaks=10,plot=FALSE)$counts*percents)


true_t0<-(sum(survival[which(survival$treatment==0),]$alive)-t0_pr_mort)/sum(remain_0)
true_t30<-(sum(survival[which(survival$treatment==30),]$alive)-t30_pr_mort)/sum(remain_30)
true_t60<-(sum(survival[which(survival$treatment==60),]$alive)-t60_pr_mort)/sum(remain_60)
true_t90<-(sum(survival[c(which(survival$treatment==90), which(survival$treatment==100)),]$alive)-t90_pr_mort)/sum(remain_90)
true_t120<-(sum(survival[which(survival$treatment==120),]$alive)-t120_pr_mort)/sum(remain_120)


post_exp_mort <-c(1/(sum(survival[which(survival$treatment==0),]$alive)/t0_pr_mort)
                  ,1/(sum(survival[which(survival$treatment==30),]$alive)/t30_pr_mort)
                  ,1/(sum(survival[which(survival$treatment==60),]$alive)/t60_pr_mort)
                  ,1/(sum(survival[c(which(survival$treatment==90), which(survival$treatment==100)),]$alive)/t90_pr_mort)
                  ,1/(sum(survival[which(survival$treatment==120),]$alive)/t120_pr_mort))


##Stacked version

surv_percentage<-matrix



observed_mort_p<-100*c(sum(remain_0-alive_0)/sum(remain_0),sum(remain_30-alive_30)/sum(remain_30),sum(remain_60-alive_60)/sum(remain_60),
                       sum(remain_90-alive_90)/sum(remain_90),sum(remain_120-alive_120)/sum(remain_120))

post_release_mort_p<-100*c(t0_pr_mort/sum(as.integer(remain_0)),t30_pr_mort/sum(as.integer(remain_30)),
                           t60_pr_mort/sum(as.integer(remain_60)),t90_pr_mort/sum(as.integer(remain_90)),
                           t120_pr_mort/sum(as.integer(remain_120)))
predicted_alive<-100*c((sum(as.integer(alive_0))-t0_pr_mort)/sum(as.integer(remain_0)),(sum(as.integer(alive_30))-t30_pr_mort)/sum(as.integer(remain_30)),
                       (sum(as.integer(alive_60))-t60_pr_mort)/sum(as.integer(remain_60)),(sum(as.integer(alive_90))-t90_pr_mort)/sum(as.integer(remain_90)),
                       (sum(as.integer(alive_120))-t120_pr_mort)/sum(as.integer(remain_120)))

true_t120
predicted_alive+observed_mort_p+post_release_mort_p
rm_data <- matrix(c(predicted_alive,observed_mort_p,post_release_mort_p),nrow=3, byrow = TRUE)
rownames(rm_data) <- c("Alive at 24 hours (observed)","Dead at 24 hours (observed)","Died after 24 hours (estimated)")
colnames(rm_data) <- c("0","30","60","90","120")


#Figure 4
setwd(here("New-figures"))
tiff(paste(Sys.Date(), "release_mortality.tiff", sep="_"), width=1200, height=800,res=200, units = "px", pointsize=8)
par(mfrow=c(1,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
barplot(100*post_exp_mort, names=c(0,30,60,90,120), xlab = "Time out of water (min)",
        ylab="Estimated post-24-hour mortality (%)", col=rev(viridis(3))[1],ylim=c(0,16))

barplot(rm_data, 
        col=viridis(3) , 
        border="white", 
        font.axis=1, 
        beside=F, 
        xlab="Treatment time (min)", ylab="Percent of prawns",
        font.lab=1)
legend(2.8,90,rev(rownames(rm_data)), fill=rev(viridis(3)), cex=0.6)

dev.off()


##Per treatment length hist----
#Figure S3, Supplement 3
setwd(here("New-figures"))
tiff(paste(Sys.Date(), "per_treatment_length_hist.tiff", sep="_"), width=1800, height=1800, res = 250, pointsize=12)
par(mfrow=c(3,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
hist(model_df_2[which(model_df_2$treatment==0),]$length,breaks=pretty(17.5:53.5, n = 69),col=viridis(6)[1],main="0 minutes", 
     xlab="Length (mm)", ylab="Number of prawns", ylim=c(0,70))

hist(model_df_2[which(model_df_2$treatment==30),]$length,breaks=pretty(17.5:53.5, n = 69),col=viridis(6)[2],main="30 minutes", 
     xlab="Length (mm)", ylab="Number of prawns",ylim=c(0,70))

hist(model_df_2[which(model_df_2$treatment==60),]$length,breaks=pretty(17.5:53.5, n = 69),col=viridis(6)[3],main="60 minutes", 
     xlab="Length (mm)", ylab="Number of prawns",ylim=c(0,70))
hist(model_df_2[which(model_df_2$treatment==90),]$length,breaks=pretty(17.5:53.5, n = 69),col=viridis(6)[4],main="90 minutes", 
     xlab="Length (mm)", ylab="Number of prawns",ylim=c(0,70))
hist(model_df_2[which(model_df_2$treatment==120),]$length,breaks=pretty(17.5:53.5, n = 69),col=viridis(6)[5],main="120 minutes", 
     xlab="Length (mm)", ylab="Number of prawns",ylim=c(0,70))
hist(survival[is.na(survival$treatment),]$length,breaks=pretty(17.5:53.5, n = 69),col=viridis(6)[6],main="Unbanded", 
     xlab="Length (mm)", ylab="Number of prawns",ylim=c(0,70))


dev.off()





#Length NA Summary----
#Chi Square test comparing number of scavenged prawns in each treatment
no_length<-subset(survival,is.na(survival$length))
yes_length<-subset(survival,!is.na(survival$length))

no_length<-no_length[!is.na(no_length$treatment),]
yes_length<-yes_length[!is.na(yes_length$treatment),]

prop_length_NA<-barplot(c(hist(no_length$treatment, breaks =c(0,15,45,75,95,105,125), plot=FALSE)$counts/hist(survival$treatment, breaks =c(0,15,45,75,95,105,125), plot=FALSE)$counts),
                        names=c("0","30","60","90","100","120"))

chisq.test(t(data.frame(no_length=hist(no_length$treatment, breaks =c(0,15,45,75,95,105,125), plot=FALSE)$counts,
                        yes_length =hist(yes_length$treatment, breaks =c(0,15,45,75,95,105,125), plot=FALSE)$counts))
)

#Unbanded prawns----
#this gives two subsets with only banded prawns and only unbanded prawns,respectively
banded_survival<-survival[!is.na(survival$treatment),]
unbanded_survival<-survival[is.na(survival$treatment),]

#T test comparing banded and unbanded 
t.test(unbanded_survival$length,banded_survival$length)
var(unbanded_survival$length,na.rm=T)

#set working directory for figures
setwd(here("figures"))

#DELETE/ask about Violin----
#make upcoming figure into png
tiff(paste(Sys.Date(), "Banded_Unbanded_length_boxplot.tiff", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

#boxplot compari
boxplot(banded_survival$length,unbanded_survival$length, xlab= "Banded vs Unbanded", names = c("Banded", "Unbanded"))

#png complete
dev.off()

#Theoretical lost prawns experiment----

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


#Save upcoming figures as a tiff
setwd(here("figures"))
tiff(paste(Sys.Date(), "lost_bias_20.tiff", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

#Plot showing true surivival vs observed survival at different treatments (times out of water)
#assuming only dead prawns are lost
plot(NULL, xlim=c(-5,125),ylim= c(0,100),xlab="Treatment", ylab="Proportion Survived", main="Survival with different loss biases")
legend(x=80,y=95,c("No lost","Dead lost","Alive lost","Equal lost"), pch=c(1,2,3,4), cex=0.8)

#Plot true survival
points(treatments,true.alive)

#Plot showing true surivival vs observed survival at different treatments (times out of water)
#assuming only dead prawns are lost
points(treatments,dead.lost(total.prawns,true.alive,0.2), pch=2)

#Plot showing true surivival vs observed survival at different treatments (times out of water)
#assuming only living prawns are lost
points(treatments,alive.lost(total.prawns,true.alive,0.2), pch=3)

#Plot showing true surivival vs observed survival at different treatments (times out of water)
#assuming dead and alive prawns are lost at an equal frequency
points(treatments,equal.lost(total.prawns,true.alive,0.2), pch=4)

#Stop saving figures as a png
dev.off()


#ANALYSIS AND PLOTTING----

back_trans<-function(x){
  return(exp(x)/(1+exp(x)))
}
trans<-function(x){
  return(log(x/(1-x)))
}
back_trans_1<-function(x){
  return(exp(0.3-0.14*x)/(1+exp(0.3-0.14*x)))
}

##Survival curves with temp x treat averages----
library(viridis)
mean_length<-mean(model_df_2$length)
hist_temps<-hist(trial$exp_set_temp_air,plot=FALSE, breaks= c(9,13,17,21,26))
hist_temps$counts
hist_temps$breaks
n_temp_bins<-length(hist_temps$breaks)-1

model_6.1_1<-readRDS("mymodel.rds")


cols<-viridis(4)
cols1<-viridis(4,alpha=0.5)

#Figure 6, 3 panel model figure----

#This functon makes one of the three panels
conf_maker<-function(LENGTH_DATA, mid_length){
  
  #LENGTH_DATA is a subset of the survival data with only the specified length bin. 
  #It will be used to calculate per-treatment, temperature-binned, average survival.
  
  #mid_length is the middle of the length bin. It is used for model prediction. 
  n_temp_bins<-length(hist_temps$breaks)-1
  
  #create a 4 x 5 matrix 
  #4 rows for each temperature bin
  #5 rows for temperature, treatment, upper CI limit, mean, and lower CI limit
  
  CI_matrix<-matrix(0, n_temp_bins*5,5)
  
  #We are going to repeat the following procedure for each temperature bin 
  for (i in 1:n_temp_bins){
    
    #Start with Length data in given length bin:
    temp_mid<-hist_temps$mids[i]
    
    #Temperature boundaries for the temperature bin in this iteration of the 'for' loop.
    temp_bound<-hist_temps$breaks[c(i,i+1)]
    
    #Subset data to current temperature bin
    temp_bin_data<-LENGTH_DATA[between(LENGTH_DATA$temp,temp_bound[1],temp_bound[2]),]
    
    #We are going to repeat the following procedure for each Treatment, within this temperature bin
    for (x in 1:length(unique_treatments)){
      
      #
      current_treat<-unique_treatments[x]
      
      #Subset temp-binned data to current treatment
      treat_temp_bin_data<-temp_bin_data[which(temp_bin_data$treatment==current_treat),]
      
      #trial-trap levels in this temperature bin
      trial_traps<-unique(treat_temp_bin_data$trial_trap)
      
      #The number of trial-trap levels in this temperature bin
      n_trial_traps<-length(trial_traps)
      
      #Vector for storing survival proportions for each trial-trap, within this temp bin
      survival_props<-vector(length=n_trial_traps)
      
      
      #Within each trap-trial sample, calculate the survival proportion, 
      for (j in 1:n_trial_traps){
        
        #current_tt is the current trial-trap which we are calculating the survival proportion for.
        current_tt<-trial_traps[j]
        
        #Store survival proportion for current Trial trap, obviously only within this length bin
        survival_props[j]<-mean(treat_temp_bin_data[which(treat_temp_bin_data$trial_trap==current_tt),]$alive)
        
      }
      
      #ASK
      #select 123 samples (with replacement). 
      #ASK ABOUT 123 HERE, IT COULD ALSO BE TRIAL_TRAP Length, I.E is it the trial_trap levels in a particular temperature bin
      
      #A vector to store sampled survival proportions
      boot<-vector(length=1000)
      
      #Sample from calculated survival proportions, with replacement
      for (k in 1:1000){
        
        #survival_props contains one entry for for each trial-trap in the temperature bin.
        #There were 123 trial-trap levels total, some temperature bins may have over 60, some only 12.
        #the length of the survival_props vector equals the value of n_trial_traps
        
        #This line samples, with replacement, from the survival proportions calculated for each trial-trap level in the temperature bin.
        #It takes a number of samples equal to the nummber of trial-trap levels and stores it in the 'boot' vector
        boot[k]<-mean(sample(survival_props,replace=TRUE,size=n_trial_traps))
      }
      
      #
      row_number<-(i-1)*5+x
      
      #Store temperature bin, specifically the middle of the bin
      CI_matrix[row_number,1]<-temp_mid
      
      #Store 
      CI_matrix[row_number,2]<-current_treat
      
      #Store 97.5th survival percentile for upper CI limit
      CI_matrix[row_number,3]<-quantile(boot, probs=0.975)
      
      #ASK, mean or 50th percentile
      #Store 50th survival percentile for mean 
      CI_matrix[row_number,4]<-quantile(boot, probs=0.5)
      
      #Store 2.5th survival percentile for upper CI limit
      CI_matrix[row_number,5]<-quantile(boot, probs=0.025)
      
      #Store middle temperature value 
      CI_matrix[j,1]<-hist_temps$mids[i]
    }
    
  }
  #END OF FOR LOOP
  
  CInew1<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[1],120),length=rep(mid_length,120),treatment=c(1:120))) 
  CInew2<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[2],120),length=rep(mid_length,120),treatment=c(1:120))) 
  CInew3<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[3],120),length=rep(mid_length,120),treatment=c(1:120))) 
  CInew4<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[4],120),length=rep(mid_length,120),treatment=c(1:120))) 
  
  
  
  colors<-c("11\u00B0C"= cols[1],"15\u00B0C"=cols[2] ,"19\u00B0C"=cols[3], "23.5\u00B0C"= cols[4])
  cols<-viridis(4)
  cols1<-viridis(4,alpha=0.5)
  
  
  #DELETE
  temp_12<-LENGTH_DATA[which(LENGTH_DATA$temp<=hist_temps$breaks[2]),][,c("treatment","alive")]
  
  temp_16<-LENGTH_DATA[which(LENGTH_DATA$temp>hist_temps$breaks[2] & 
                               LENGTH_DATA$temp<=hist_temps$breaks[3]),][,c("treatment","alive")]
  
  temp_20<-LENGTH_DATA[which(LENGTH_DATA$temp>hist_temps$breaks[3] & 
                               LENGTH_DATA$temp<=hist_temps$breaks[4]),][,c("treatment","alive")]
  
  temp_24<-LENGTH_DATA[which(LENGTH_DATA$temp>hist_temps$breaks[4]),][,c("treatment","alive")]
  
  
  
  av<-rbind(confint(temp_12,12,unique(temp_12$treatment)), confint(temp_16,16,unique(temp_16$treatment)),confint(temp_20,20,unique(temp_20$treatment)),confint(temp_24,24,unique(temp_24$treatment)))
  
  
  return(ggplot(data = NULL, aes(x=x))+geom_line(aes(y=CInew1$mod.avg.pred, color="11\u00B0C"), lwd = 1)+  geom_line(aes(y=CInew2$mod.avg.pred, color="15\u00B0C"), lwd = 1) +
           geom_line(aes(y=CInew3$mod.avg.pred, color="19\u00B0C"), lwd = 1) +geom_line(aes(y=CInew4$mod.avg.pred, color="23.5\u00B0C"), lwd = 1)+
           geom_ribbon(aes(ymin=CInew1$lower.CL, ymax=CInew1$upper.CL), alpha=0.3, fill = cols[1],  color = cols[1], linetype = "dotted")+
           geom_ribbon(aes(ymin=CInew2$lower.CL, ymax=CInew2$upper.CL), alpha=0.3, fill = cols[2],  color = cols[2], linetype = "dotted")+
           geom_ribbon(aes(ymin=CInew3$lower.CL, ymax=CInew3$upper.CL), alpha=0.3, fill = cols[3],  color = cols[3], linetype = "dotted")+
           geom_ribbon(aes(ymin=CInew4$lower.CL, ymax=CInew4$upper.CL), alpha=0.3, fill = cols[4],  color = cols[4], linetype = "dotted")+
           geom_point(data=av[which(av$temp==12),], aes(x=treat,y=mean),color=cols1[1])+geom_point(data=av[which(av$temp==16),], aes(x=treat,y=mean),color=cols1[2])+
           geom_point(data=av[which(av$temp==20),], aes(x=treat,y=mean),color=cols1[3])+geom_point(data=av[which(av$temp==24),], aes(x=treat,y=mean),color=cols1[4])+
           geom_errorbar(data = av,aes(x=treat,ymin=lower, ymax=upper), width=.2, alpha=0.4)+
           labs(x="Time out of water",y="Probability of survival", color="Air temperature (\u00B0C)", title= paste("Survival for",mean_length,"mm prawn"))+scale_color_manual(values = cols1)
         #+theme(    legend.position = c(.95, .95),
         #legend.justification = c("right", "top"),
         # legend.box.just = "right",
         #legend.margin = margin(6, 6, 6, 6),plot.title = element_text(hjust = 0.5),panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", color = "grey50"))
  )
}
#END OF FUNCTION

length(unique(model_df_2[which(model_df_2$length<28 & model_df_2$temp>20),]$trial_trap))



shorties<-model_df_2[which(model_df_2$length<=27.5),]
normies<-model_df_2[which(model_df_2$length>=27.5 & model_df_2$length<=36.5),]
biggies<-model_df_2[which(model_df_2$length>=36.5),]
install.packages("gridExtra")

setwd(here("figures"))
tiff(paste(Sys.Date(), "survival_by_temp_three_panel.tiff", sep="_"),width=1000,height=1200)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

gridExtra::grid.arrange(conf_maker(shorties, round(mean(shorties$length))),
                        conf_maker(normies, round(mean(normies$length))),
                        conf_maker(biggies, round(mean(biggies$length))))
dev.off()