survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")
attach(survival)

# Number of lost prawns
lost_per_trial<-matrix(nrow = 23, ncol = 2)
for (i in 1:23){
  lost_per_trial[i,1]<-i
  df<-subset(trial, trial_number==i)
  pulled<-sum(c(df$immediate_release_number,df$X30min_number,df$X1h_number,df$X1h30min_numner,df$X2h_number), na.rm = TRUE)
  df1<-subset(survival, trial_number==i)
  remain<-(sum(df1$alive, na.rm = TRUE)+sum(df1$scavenged, na.rm = TRUE)+sum(df1$dead, na.rm = TRUE))
  lost_per_trial[i,2]<-pulled-remain
}
plot(lost_per_trial[,1],lost_per_trial[,2])


#Number of treatments for each trial
treatments_per_trial<-matrix(nrow = 23, ncol = 2)
for (i in 1:23){
treatments_per_trial[i,1]<-i
df<-subset(survival, trial==i)
treatments_per_trial[i,2]<-length(unique(df$treatment))
}
graphics.off()
plot(treatments_per_trial[,1], treatments_per_trial[,2])

#Number of Unbanded prawns at end of trial
unbanded<-matrix(nrow = 23, ncol = 2)
for (i in 1:23){
  unbanded[i,1]<-i
  df<-subset(survival, trial_number==i)
  unbanded[i,2]<-sum(is.na(df$treatment))
}

#length distribution figures

#Scavenged vs Dead
scav_dead<-matrix(nrow = 23, ncol = 3)
for (i in 1:23){
  scav_dead[i,1]<-i
  df<-subset(survival, trial_number==i)
  scav_dead[i,2]<-sum(df$scavenged,na.rm=TRUE)
  scav_dead[i,3]<-sum(df$dead,na.rm=TRUE)
}
subset(survival, trial_number==3)
scav_dead

plot(scav_dead[,1], scav_dead[,3], col="red")
lines(scav_dead[,1], scav_dead[,2],col="blue")






