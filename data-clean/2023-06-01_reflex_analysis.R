setwd("/Users/jacobhoutman/Documents/Git Hub/Prawnz-survival-experiment")
library("here")
setwd(here("data-clean"))
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

reflexes<-reflexes[order(reflexes$trial_number,reflexes$trap_number),]
names(reflexes)
reflexes[which(is.na(reflexes$abdomen_turgor+reflexes$abdomen_retraction+reflexes$leg_movement+reflexes$leg_retraction+reflexes$maxilliped_movement+reflexes$maxilliped_retraction+reflexes$antenna+reflexes$eye_turgor+as.integer(reflexes$pleopods)+reflexes$mouth)),]

reflexes<-reflexes[order(reflexes$trial_number,reflexes$trap_number),]
reflexes$abdomen_turgor+reflexes$abdomen_retraction+reflexes$leg_movement+reflexes$leg_retraction+reflexes$maxilliped_movement+reflexes$maxilliped_retraction+reflexes$antenna+reflexes$eye_turgor+as.integer(reflexes$pleopods)+reflexes$mouth
reflexes$total<-reflexes$abdomen_turgor+reflexes$abdomen_retraction+reflexes$leg_movement+reflexes$leg_retraction+reflexes$maxilliped_movement+reflexes$maxilliped_retraction+reflexes$antenna+reflexes$eye_turgor+as.integer(reflexes$pleopods)+reflexes$mouth
setwd(here("figures"))
violin_reflex<-data.frame(total=c(reflexes[which(reflexes$treatment==0),]$total,reflexes[which(reflexes$treatment==30),]$total,reflexes[which(reflexes$treatment==60),]$total,reflexes[which(reflexes$treatment==90),]$total,reflexes[which(reflexes$treatment==120),]$total),treat=c(rep("0",nrow(reflexes[which(reflexes$treatment==0),])),rep("30",nrow(reflexes[which(reflexes$treatment==30),])),rep("60",nrow(reflexes[which(reflexes$treatment==60),])),rep("90",nrow(reflexes[which(reflexes$treatment==90),])),rep("120",nrow(reflexes[which(reflexes$treatment==120),]))))
violin_reflex$treat<-as.factor(violin_reflex$treat)
ggplot(violin_reflex, aes(x=treat, y=total)) + geom_violin()

nrow(reflexes[which(reflexes$treatment==120),])

boxplot(reflexes[which(reflexes$treatment==0),]$total,reflexes[which(reflexes$treatment==30),]$total,reflexes[which(reflexes$treatment==60),]$total,reflexes[which(reflexes$treatment==90),]$total,reflexes[which(reflexes$treatment==120),]$total)
graphics.off()

setwd(here("figures"))
pdf(paste(Sys.Date(), "reflex_distribution.pdf", sep="_"), width=7, height=7, pointsize=12)
par(mfrow=c(6,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
for (i in 0:4){
  treat_ref<-reflexes[which(reflexes$treatment==i*30),]$total
  plot(NULL, xlim=c(0,10),ylim=c(0,100), main=paste("Reflex distribution trial", i*30))
  points(0,sum(treat_ref==0)*100/length(treat_ref==0))
  points(1,sum(treat_ref==1)*100/length(treat_ref==1))
  points(2,sum(treat_ref==2)*100/length(treat_ref==2))
  points(3,sum(treat_ref==3)*100/length(treat_ref==3))
  points(4,sum(treat_ref==4)*100/length(treat_ref==4))
  points(5,sum(treat_ref==5)*100/length(treat_ref==5))
  points(6,sum(treat_ref==6)*100/length(treat_ref==6))
  points(7,sum(treat_ref==7)*100/length(treat_ref==7))
  points(8,sum(treat_ref==8)*100/length(treat_ref==8))
  points(9,sum(treat_ref==9)*100/length(treat_ref==9))
  points(10,sum(treat_ref==10)*100/length(treat_ref==10))
}
dev.off()






