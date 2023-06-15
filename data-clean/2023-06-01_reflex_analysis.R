setwd("/Users/jacobhoutman/Documents/Git Hub/Prawnz-survival-experiment")
library("here")
setwd(here("data-clean"))
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

#Ordering dataframe
reflexes<-reflexes[order(reflexes$trial_number,reflexes$trap_number),]

#New column for sum of reflexes 
reflexes$total<-reflexes$abdomen_turgor+reflexes$abdomen_retraction+reflexes$leg_movement+reflexes$leg_retraction+reflexes$maxilliped_movement+reflexes$maxilliped_retraction+reflexes$antenna+reflexes$eye_turgor+as.integer(reflexes$pleopods)+reflexes$mouth

#Violin plot sum of reflexes for each treatment
setwd(here("figures"))
violin_reflex<-data.frame(total=c(reflexes[which(reflexes$treatment==0),]$total,reflexes[which(reflexes$treatment==30),]$total,reflexes[which(reflexes$treatment==60),]$total,reflexes[which(reflexes$treatment==90),]$total,reflexes[which(reflexes$treatment==120),]$total),treat=c(rep("0",nrow(reflexes[which(reflexes$treatment==0),])),rep("30",nrow(reflexes[which(reflexes$treatment==30),])),rep("60",nrow(reflexes[which(reflexes$treatment==60),])),rep("90",nrow(reflexes[which(reflexes$treatment==90),])),rep("120",nrow(reflexes[which(reflexes$treatment==120),]))))
violin_reflex$treat<-as.factor(violin_reflex$treat)
ggplot(violin_reflex, aes(x=treat, y=total)) + geom_violin()


boxplot(reflexes[which(reflexes$treatment==0),]$total,reflexes[which(reflexes$treatment==30),]$total,reflexes[which(reflexes$treatment==60),]$total,reflexes[which(reflexes$treatment==90),]$total,reflexes[which(reflexes$treatment==120),]$total)
graphics.off()

#Plot for each treatment 
setwd(here("figures"))
pdf(paste(Sys.Date(), "reflex_distribution.pdf", sep="_"), width=7, height=7, pointsize=12)
par(mfrow=c(6,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
for (i in 0:4){
  treat_ref<-reflexes[which(reflexes$treatment==i*30),]$total
  plot(NULL, xlim=c(0,10),ylim=c(0,100), main=paste("Reflex distribution treatment", i*30))
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

quantile(x, seq(0.2,0.8,0.2))
plot(NULL, xlim=c(0,120),ylim=c(0,10), main="Reflex quantiles by treatment")
for (i in 0:4){
  treat_ref<-reflexes[which(reflexes$treatment==i*30),]$total
  points(rep(i*30, 4), quantile(treat_ref,seq(0.2,0.8,0.2)))
}

 quantile(reflexes[which(reflexes$treatment==90),]$total,seq(0.2,0.8,0.2))

table(c(0,2),c(0,2))

ref_mat<-matrix(data=NA,nrow=10,ncol=21)
trials<-unique(reflexes[order(reflexes$trial_number),]$trial_number)
for (i in 1:21){
  for(j in 1:10){
  ref_mat[j,i]<-length(which(reflexes[which(reflexes$trial_number==trials[i]),]$total==j))
  }
}
ref_mat[5:7,]

