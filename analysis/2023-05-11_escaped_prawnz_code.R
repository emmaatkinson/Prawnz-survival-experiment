length(unique(c(1,1,2)))
trial<-read.csv("2023-05-09_prawn_combined_trial_data")
survival<-read.csv("2023-05-09_prawn_combined_survival_data")

escaped<-vector('numeric',23)
for(i in 1:23){
  e<-trial[trial$trial_number==i,]
  d<-survival[survival$trial_number==i,]
  escaped[i]<-e$immediate_release_number+e$X30min_number+e$X1h_number+e$X1h30min_numner+e$X2h_number-(sum(d$alive, na.rm = TRUE)+sum(d$dead, na.rm = TRUE)+sum(d$scavenged, na.rm = TRUE))
}

e<-trial[trial$trial_number==11,]
e$immediate_release_number+e$X30min_number+e$X1h_number+e$X1h30min_numner+e$X2h_number
sum(survival[survival$trial_number==11,]$alive)+sum(survival[survival$trial_number==11,]$dead)+sum(survival[survival$trial_number==11,]$scavenged, na.rm=TRUE)









