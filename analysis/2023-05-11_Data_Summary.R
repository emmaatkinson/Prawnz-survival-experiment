setwd(here("data-clean"))
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data")
survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")


setwd(here("figures"))
png(paste(Sys.Date(), "Banded_Unbanded_length_boxplot.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
boxplot(survival[!is.na(survival$treatment),]$length,survival[is.na(survival$treatment),]$length, xlab= "Banded vs Unbanded", names = c("Banded", "Unbanded"))
dev.off()




