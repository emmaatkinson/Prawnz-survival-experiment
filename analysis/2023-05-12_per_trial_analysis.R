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
remain<-vector(mode="numeric", length=23)

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
  remain[i]<-nrow(df1)
  
  #lost is the difference for each trial
  lost_prawnz[i]<-pulled[i]-remain[i]
  
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

  #the number of scavenged,dead and alive prawns per trial 
  scavenged[i]<-sum(df1$scavenged,na.rm=TRUE)
  dead[i]<-sum(df1$dead,na.rm=TRUE)
  alive[i]<-sum(df1$alive,na.rm=TRUE)

  #vectors of all the prawns lengths is created under the name length_x
  #where x is the trial number
  assign(paste0("lengths_",i),sort(df1$length))
  
  #The number of prawns of each stage per trial 
  stage_0_per_trial[i]<-nrow(subset(df1, stage==0))
  stage_1_per_trial[i]<-nrow(subset(df1, stage==1))
  stage_2_per_trial[i]<-nrow(subset(df1, stage==2))
  stage_3_per_trial[i]<-nrow(subset(df1, stage==3))
  stage_NA_per_trial[i]<-sum(is.na(df1$stage))

}

#Dataframe of per trial information
trial_df<-data_frame(trial_number, total_treatments, pulled, remain, lost_prawnz, unbanded, scavenged, dead, alive, stage_0_per_trial,stage_1_per_trial,stage_2_per_trial,stage_3_per_trial)
trial_df$salinity<-exp_haul_sal_0m
trial_df$temperature<-rowSums(cbind(exp_set_temp_0m, exp_haul_temp_0m), na.rm=TRUE)/(rep(2, 23)-(is.na(exp_haul_temp_0m)+is.na(exp_set_temp_0m)))



# The maxima and minima of the sums of the alive, dead and scavenged per trial
max_surv_sum
min_surv_sum

#boxplot of the length distribution for each trial
boxplot(lengths_1, lengths_2, lengths_3,lengths_4, lengths_5, lengths_6,lengths_7, lengths_8, lengths_9,lengths_10, lengths_11, lengths_12, lengths_13,lengths_14, lengths_15, lengths_16,lengths_17, lengths_18, lengths_19,lengths_20,lengths_21,lengths_22, lengths_23)

#Number of each stage per trial
plot(1:23, stage_0_per_trial)
plot(1:23, stage_1_per_trial)
plot(1:23, stage_2_per_trial)
plot(1:23, stage_3_per_trial)

#This shows there is probably no error with the remaining part of the remain
#part of the lost prawns equation
remain-(stage_0_per_trial+stage_1_per_trial+stage_2_per_trial+stage_3_per_trial+stage_NA_per_trial)

#lost, dead, scavenged, alive prawns per trial
plot(lost_prawnz)

plot(1:23, 1:300)
plot(dead,type="l",col="red", ylab="Prawns", xlab="Trial")
lines(alive,col="green")
lines(scavenged, col="blue")

trial_repeated <- c(rep("1" , 3) , rep("2" , 3) , rep("3" , 3) , rep("4" , 3),rep("5" , 3) , rep("6" , 3) , rep("7" , 3) , rep("8" , 3),rep("9" , 3) , rep("10" , 3) , rep("11" , 3) , rep("12" , 3),rep("13" , 3) , rep("14" , 3) , rep("15" , 3) , rep("16" , 3),rep("17" , 3) , rep("18" , 3) , rep("19" , 3) , rep("20" , 3),rep("21" , 3) , rep("22" , 3) , rep("23" , 3))
condition <- rep(c("Alive" , "Dead" , "Scavenged") , 23)
prawns<-vector(mode="numeric", length=69)
for (i in 1:23){
prawns[3*i-2]<-alive[i]
prawns[3*i-1]<-dead[i]
prawns[3*i]<-scavenged[i]
}
rep_data<- data.frame(trial_repeated,condition,prawns)

ggplot(rep_data, aes(fill=condition, y=prawns, x=trial_repeated)) + 
  geom_bar(position="stack", stat="identity")
#Percent lost, dead, scavenged, alive prawns per trial
#NOTE: dead alive and scavenged are shown as proportions of remaining
#(not including lost) while lost is shown as a proportion of the total 
#at the start

trial_df$percent_lost<-(100*lost_prawnz/pulled)
trial_df$percent_dead<-100*trial_df$dead/trial_df$remain
trial_df$percent_alive<-100*trial_df$alive/trial_df$remain
trial_df$percent_scavenged<-100*trial_df$scavenged/trial_df$remain

dead/remain+alive/remain+scavenged/remain
trial_df$percent_dead+trial_df$percent_alive+trial_df$percent_scavenged

#Plots of Percent survival vs Temperature 
p1<-ggplot(data=trial_df, aes(x=salinity, y=percent_alive))+geom_point()
p2<-ggplot(data=trial_df, aes(x=temperature, y=percent_alive))+geom_point()
p1
p2

#SHOULD BE ALL ZEROES
lost_prawnz+dead+alive+scavenged-pulled

#SHOULD BE ALL 100
trial_df$percent_dead+trial_df$percent_alive+trial_df$percent_scavenged
