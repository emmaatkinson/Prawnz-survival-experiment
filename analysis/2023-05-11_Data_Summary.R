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




