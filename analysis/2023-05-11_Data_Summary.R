setwd(here("data-clean"))
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data")
survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")


setwd(here("figures"))
png(paste(Sys.Date(), "Banded_Unbanded_length_boxplot.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
boxplot(survival[!is.na(survival$treatment),]$length,survival[is.na(survival$treatment),]$length, xlab= "Banded vs Unbanded", names = c("Banded", "Unbanded"))
dev.off()


treatments
prop.alive<-function(totals, true.alive){
 return(true.alive/totals)
}

dead.lost<-function(totals,true.alive, percent){
  true.dead=totals-true.alive
  lost=true.dead*percent
  return(true.alive/(totals-lost))
}
alive.lost<-function(totals,true.alive, percent){
  lost=true.alive*percent
  return((true.alive-lost)/(totals-lost))
}
equal.lost<-function(totals,true.alive, percent){
  lost=totals*percent
  lost.alive=true.alive*percent
  return((true.alive-lost.alive)/(totals-lost))
}

setwd(here("figures"))
png(paste(Sys.Date(), "lost_bias_20.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(3,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(c(0,30,60,90,120), c(0.9,0.8,0.6,0.4,0.1), xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival when 20% of dead are lost")
legend(x=100,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(c(0,30,60,90,120),dead.lost(c(100,100,100,100,100),c(90,80,60,40,10),0.2), pch=2)
plot(c(0,30,60,90,120), c(0.9,0.8,0.6,0.4,0.1), xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival when 20% of alive are lost")
legend(x=100,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(c(0,30,60,90,120),alive.lost(c(100,100,100,100,100),c(90,80,60,40,10),0.2), pch=2)
plot(c(0,30,60,90,120), c(0.9,0.8,0.6,0.4,0.1), xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival when loss (20%) is unbiased")
legend(x=100,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(c(0,30,60,90,120),equal.lost(c(100,100,100,100,100),c(90,80,60,40,10),0.2), pch=2)
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "lost_bias_40.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(3,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(c(0,30,60,90,120), c(0.9,0.8,0.6,0.4,0.1), xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival when 40% of dead are lost")
legend(x=100,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(c(0,30,60,90,120),dead.lost(c(100,100,100,100,100),c(90,80,60,40,10),0.4), pch=2)
plot(c(0,30,60,90,120), c(0.9,0.8,0.6,0.4,0.1), xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival when 40% of alive are lost")
legend(x=100,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(c(0,30,60,90,120),alive.lost(c(100,100,100,100,100),c(90,80,60,40,10),0.4), pch=2)
plot(c(0,30,60,90,120), c(0.9,0.8,0.6,0.4,0.1), xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival when loss (40%) is unbiased")
legend(x=100,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(c(0,30,60,90,120),equal.lost(c(100,100,100,100,100),c(90,80,60,40,10),0.4), pch=2)
dev.off()



#plot(NULL, xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival with different loss biases")
#legend(x=100,y=1,c("True survival","Dead lost","Alive lost","Equal lost"), pch=c(1,2,3,4), cex=0.5)
#points(c(0, 60, 120), y=c(0.9, 0.6,0.1))
#points(c(0, 60, 120), y=c(0.92, 0.65,0.12), pch = 2)
#points(c(0, 60, 120), y=c(0.88, 0.56,0.082), pch=3)
#points(c(0, 60, 120), y=c(0.9, 0.6,0.1), pch=4)
