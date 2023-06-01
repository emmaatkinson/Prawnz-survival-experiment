setwd("/Users/jacobhoutman/Documents/Git Hub/Prawnz-survival-experiment")
setwd(here("data-raw"))
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

reflexes<-reflexes[order(reflexes$trial_number,reflexes$trap_number),]
names(reflexes)
reflexes[which(is.na(reflexes$abdomen_turgor+reflexes$abdomen_retraction+reflexes$leg_movement+reflexes$leg_retraction+reflexes$maxilliped_movement+reflexes$maxilliped_retraction+reflexes$antenna+reflexes$eye_turgor+as.integer(reflexes$pleopods)+reflexes$mouth)),]






