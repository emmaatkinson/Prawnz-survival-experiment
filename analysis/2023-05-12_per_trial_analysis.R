survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")
attach(survival)

#set up empty vectors 
trial_number<-vector(mode="numeric", length=23)
lost_prawnz<-vector(mode="numeric", length=23)
total_treatments<-vector(mode="numeric", length=23)
unbanded<-vector(mode="numeric", length=23)
scavenged<-vector(mode="numeric", length=23)
dead<-vector(mode="numeric", length=23)
alive<-vector(mode="numeric", length=23)
max_surv_sum<-vector(mode="numeric", length=23)
min_surv_sum<-vector(mode="numeric", length=23)
pulled<-vector(mode="numeric", length=23)

stage_0_per_trial<-vector(mode="numeric", length=23)
stage_1_per_trial<-vector(mode="numeric", length=23)
stage_2_per_trial<-vector(mode="numeric", length=23)
stage_3_per_trial<-vector(mode="numeric", length=23)
stage_NA_per_trial<-vector(mode="numeric", length=23)

for (i in 1:23){
  trial_number<-i
  
  #datasets of trial and survival for a trial number
  df<-subset(trial, trial_number==i)
  df1<-subset(survival, trial_number==i)
  
  #sum of treatment numbers to ascertain total prawns in each trial
  pulled[i]<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number), na.rm = TRUE)
  
  #total prawns left after soak including dead scavenged and alive
  #for each trial, as it changes with the for loop
  remain<-nrow(df1)
  
  #lost is the difference for each trial
  lost_prawnz[i]<-pulled[i]-remain
  
  # these vectors are filled with the maxima and minima of the sums of three 
  #survival values for each prawn for a trial. The sums should all be 1
  #because the three are mutually exclusive. Used for checking data was 
  #entered correctly
  max_surv_sum[i]<-max(df1$alive+df1$dead+df1$scavenged, na.rm=TRUE)
  min_surv_sum[i]<-min(df1$alive+df1$dead+df1$scavenged, na.rm=TRUE)
  
  #number of different treatments for each trial
  total_treatments[i]<-length(unique(df1$treatment))

  #unbanded prawns are entered as treatment=NA so the sum of these, per trial
  #is the number of unbanded per trial
  unbanded[i]<-sum(is.na(df1$treatment))

  scavenged[i]<-sum(df1$scavenged,na.rm=TRUE)
  dead[i]<-sum(df1$dead,na.rm=TRUE)
  alive[i]<-sum(df1$alive,na.rm=TRUE)

  #vectors of all the prawns lengths is created under the name length_x
  #where x is the trial number
  assign(paste0("lengths_",i),sort(df1$length))
  
  #this is wrong, there are NA's
  stage_0_per_trial[i]<-nrow(subset(df1, stage==0))
  stage_1_per_trial[i]<-nrow(subset(df1, stage==1))
  stage_2_per_trial[i]<-nrow(subset(df1, stage==2))
  stage_3_per_trial[i]<-nrow(subset(df1, stage==3))
  stage_NA_per_trial[i]<-sum(is.na(df1$stage))

}
max_surv_sum
trial_df<-data_frame(trial_number, total_treatments, lost_prawnz, unbanded, scavenged, dead, stage_0_per_trial,stage_1_per_trial,stage_2_per_trial,stage_3_per_trial)
dead
boxplot(lengths_1, lengths_2, lengths_3,lengths_4, lengths_5, lengths_6,lengths_7, lengths_8, lengths_9,lengths_10, lengths_11, lengths_12, lengths_13,lengths_14, lengths_15, lengths_16,lengths_17, lengths_18, lengths_19,lengths_20,lengths_21,lengths_22, lengths_23)
pulled
plot(1:23, stage_0_per_trial)
plot(1:23, stage_1_per_trial)
plot(1:23, stage_2_per_trial)
plot(1:23, stage_3_per_trial)

pulled-(stage_0_per_trial+stage_1_per_trial+stage_2_per_trial+stage_3_per_trial+stage_NA_per_trial)
lost_prawnz

df1<-subset(survival, trial_number==8)
nrow(df1)-sum(is.na(df1))


pulled<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number), na.rm = TRUE)



pulled<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number), na.rm = TRUE)
remain<-nrow(df1)
lost_prawnz[i]<-pulled-remain


#Alive percent by trial SALINITY
percent_survived<-alive/(scavenged+dead+alive)

lost_prawnz



df<-subset(trial, trial_number==8)
df1<-subset(survival, trial_number==8)
pulled<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number))
#remain<-(sum(df1$alive, na.rm = TRUE)+sum(df1$scavenged, na.rm = TRUE)+sum(df1$dead, na.rm = TRUE))
remain<-nrow(df1)
pulled-remain

df1


#this is wrong, there are NA's
df1<-subset(survival, trial_number==1)
stage_0_per_trial<-nrow(df1[df1$stage==0,])
df2<-subset(df1, stage==0)

stage_1_per_trial[i]<-nrow(df1[df1$stage==1,])
stage_2_per_trial[i]<-nrow(df1[df1$stage==2,])
stage_3_per_trial[i]<-nrow(df1[df1$stage==3,])
stage_4_per_trial[i]<-nrow(df1[df1$stage==4,])
stage_5_per_trial[i]<-nrow(df1[df1$stage==5,])



