reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data")
survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")

boxplot(trial$exp_set_tote_temp)

boxplot(trial$total_end_process_time)

names(trial)

boxplot(exp_haul_temp_air)
boxplot(exp_haul_temp_10m)
boxplot(survival$alive)
names(reflexes)

unique(survival$stage)
unique(survival$treatment)

boxplot(length~stage, data = survival)

hist(stage~treatment, data = survival)
hist(survival$treatment)
unique(survival$treatment)

nrow(subset(survival,treatment=="T"))

data.frame(survival$stage, survival$treatment)

install.packages('Hmisc')
Hmisc::hist.data.frame(data.frame(survival$stage, survival$treatment))



stage_0<-subset(survival, stage==0)
stage_1<-subset(survival, stage==1)
stage_2<-subset(survival, stage==2)
stage_3<-subset(survival, stage==3)

boxplot(stage_0$length,stage_1$length,stage_2$length,stage_3$length)

