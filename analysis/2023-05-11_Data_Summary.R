reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data")
survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")

#Boxplots to double check outliers in lat lon data 
boxplot(exp_set_lat_1, exp_set_lat_2,exp_haul_lat_1,exp_haul_lat_2)
boxplot(exp_set_lon_1,exp_set_lon_2,exp_haul_lon_1,exp_haul_lon_2)

#Depth
boxplot(exp_set_depth_1-exp_haul_depth_1)
boxplot(exp_set_depth_2-exp_haul_depth_2)

#salinity
boxplot(exp_set_sal_0m-exp_haul_sal_0m)
boxplot(exp_set_sal_10m-exp_haul_sal_10m)
boxplot(exp_set_tote_sal-exp_haul_tote_sal)

unique(treatment)



boxplot(exp_haul_temp_air)
boxplot(exp_haul_temp_10m)


boxplot(survival$alive)
names(reflexes)

unique(survival$stage)
unique(survival$treatment)

boxplot(length~stage, data = survival)


hist(survival$treatment)
unique(survival$treatment)

nrow(subset(survival,treatment=="T"))

data.frame(survival$stage, survival$treatment)



stage_0<-subset(survival, stage==0)
stage_1<-subset(survival, stage==1)
stage_2<-subset(survival, stage==2)
stage_3<-subset(survival, stage==3)
boxplot(length~stage, data = survival)
boxplot(stage_0$length,stage_1$length,stage_2$length,stage_3$length)

which.min(stage_1$length)
stage_1[2203,]
