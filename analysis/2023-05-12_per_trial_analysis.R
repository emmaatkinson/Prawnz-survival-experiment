survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")
attach(survival)

trial_number<-vector(mode="numeric", length=23)
lost_prawnz<-vector(mode="numeric", length=23)
total_treatments<-vector(mode="numeric", length=23)
unbanded<-vector(mode="numeric", length=23)
scavenged<-vector(mode="numeric", length=23)
dead<-vector(mode="numeric", length=23)

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

  
}
trial_df<-data_frame(trial_number, total_treatments, lost_prawnz, unbanded, scavenged, dead)
  

