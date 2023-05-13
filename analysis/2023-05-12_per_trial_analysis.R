survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")
attach(survival)

trial_number<-vector(mode="numeric", length=23)
lost_prawnz<-vector(mode="numeric", length=23)
total_treatments<-vector(mode="numeric", length=23)
unbanded<-vector(mode="numeric", length=23)
scavenged<-vector(mode="numeric", length=23)
dead<-vector(mode="numeric", length=23)

stage_0_per_trial<-vector(mode="numeric", length=23)
stage_1_per_trial<-vector(mode="numeric", length=23)
stage_2_per_trial<-vector(mode="numeric", length=23)
stage_3_per_trial<-vector(mode="numeric", length=23)
stage_4_per_trial<-vector(mode="numeric", length=23)
stage_5_per_trial<-vector(mode="numeric", length=23)

for (i in 1:23){
  trial_number<-i
  
  df<-subset(trial, trial_number==i)
  df1<-subset(survival, trial_number==i)
  
  pulled<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number), na.rm = TRUE)
  remain<-(sum(df1$alive, na.rm = TRUE)+sum(df1$scavenged, na.rm = TRUE)+sum(df1$dead, na.rm = TRUE))
  lost_prawnz[i]<-pulled-remain

  total_treatments[i]<-length(unique(df1$treatment))

  unbanded[i]<-sum(is.na(df1$treatment))

  scavenged[i]<-sum(df1$scavenged,na.rm=TRUE)
  dead[i]<-sum(df1$dead,na.rm=TRUE)

  assign(paste0("lengths_",i),sort(df1$length))
  
  #this is wrong, there are NA's
  stage_0_per_trial[i]<-nrow(df1[df1$stage==0,])
  stage_1_per_trial[i]<-nrow(df1[df1$stage==1,])
  stage_2_per_trial[i]<-nrow(df1[df1$stage==2,])
  stage_3_per_trial[i]<-nrow(df1[df1$stage==3,])
  stage_4_per_trial[i]<-nrow(df1[df1$stage==4,])
  stage_5_per_trial[i]<-nrow(df1[df1$stage==5,])
}

trial_df<-data_frame(trial_number, total_treatments, lost_prawnz, unbanded, scavenged, dead, stage_0,stage_1,stage_2,stage_3,stage_4,stage_5)

boxplot(lengths_1, lengths_2, lengths_3,lengths_4, lengths_5, lengths_6,lengths_7, lengths_8, lengths_9,lengths_10, lengths_11, lengths_12, lengths_13,lengths_14, lengths_15, lengths_16,lengths_17, lengths_18, lengths_19,lengths_20,lengths_21,lengths_22, lengths_23)

plot(1:23, stage_0_per_trial)
plot(1:23, stage_1_per_trial)
plot(1:23, stage_2_per_trial)
plot(1:23, stage_3_per_trial)
plot(1:23, stage_4_per_trial)
plot(1:23, stage_5_per_trial)





#for (i in 1:23){
#pulled<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number), na.rm = TRUE)
#remain<-((df1$alive, na.rm = TRUE)+sum(df1$scavenged, na.rm = TRUE)+sum(df1$dead, na.rm = TRUE))
#lost_prawnz[i]<-pulled-remain
#}
