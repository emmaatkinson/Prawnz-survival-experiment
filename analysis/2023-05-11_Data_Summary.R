#set working directory
setwd(here("data-clean"))

#read in data
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

#unbanded prawns have an NA value for treatment.
#this gives two subsets with only banded prawns and only unbanded prawns,respectively
banded_survival<-survival[!is.na(survival$treatment),]
unbanded_survival<-survival[is.na(survival$treatment),]

#set working directory for figures
setwd(here("figures"))

#make upcoming figure into png
png(paste(Sys.Date(), "Banded_Unbanded_length_boxplot.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

#boxplot compari
boxplot(banded_survival$length,unbanded_survival$length, xlab= "Banded vs Unbanded", names = c("Banded", "Unbanded"))

#png complete
dev.off()


##Theoretical survival experiment----

## Survival was measured based on prawns remaining in the trap after the experiment.
## If either dead or alive prawns were more likely to be lost, our survival estimates 
## may have been biased.

## There are a range of possible bias scenarios. The two most extreme cases would be
## if only dead prawns were lost, and if only alive prawns were lost. The following code
## shows how different bias scenarios influence the difference between the observed 
## survival and the true survival.

#treatments are the possible times out of water a prawn might have experienced
treatments<-c(0,30,60,90,120)

#total.prawns represents the number of prawns in each trial.
total.prawns<-c(100,100,100,100,100)

#true.alive represents the number of living prawns after the trial.
true.alive<-c(90,80,60,40,10)

#dead.lost provides the percentage of observed prawns that lived, given the
#true survival (i.e. of lost and recovered prawns) and percent of dead prawns lost

dead.lost<-function(totals,true.alive, percent){
  
  #total prawns minus living prawns gives dead prawns
  true.dead=totals-true.alive
  lost=true.dead*percent
  observed.alive=100*true.alive/(totals-lost)
  return(100*true.alive/(totals-lost))
}

alive.lost<-function(totals,true.alive, percent){
  lost=true.alive*percent
  observed.alive=100*(true.alive-lost)/(totals-lost)
  return(observed.alive)
}

equal.lost<-function(totals,true.alive, percent){
  true.dead=totals-true.alive
  
  lost.alive=true.alive*percent
  lost.dead=true.dead*percent
  lost=lost.alive+lost.dead
  
  observed.alive=100*(true.alive-lost.alive)/(totals-lost)
  return(observed.alive)
}

setwd(here("figures"))
png(paste(Sys.Date(), "lost_bias_20.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(3,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when 20% of dead are lost")
legend(x=90,y=90,c("True survival","Observed survival"), pch=c(1,2), cex=0.7)
points(treatments,dead.lost(total.prawns,true.alive,0.2), pch=2)

plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when 20% of alive are lost")
legend(x=90,y=90,c("True survival","Observed survival"), pch=c(1,2), cex=0.7)
points(treatments,alive.lost(total.prawns,true.alive,0.2), pch=2)

plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when loss (20%) is unbiased")
legend(x=90,y=90,c("True survival","Observed survival"), pch=c(1,2), cex=0.7)
points(treatments,equal.lost(total.prawns,true.alive,0.2), pch=2)

dev.off()





png(paste(Sys.Date(), "lost_bias_40.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(3,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when 40% of dead are lost")
legend(x=90,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(treatments,dead.lost(total.prawns,true.alive,0.4), pch=2)

plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when 40% of alive are lost")
legend(x=90,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(treatments,alive.lost(total.prawns,true.alive,0.4), pch=2)

plot(treatments, true.alive, xlim=c(-5,125),ylim=c(0,100),xlab="Treatment", ylab="Percent Survived", main="Survival when loss (40%) is unbiased")
legend(x=90,y=1,c("True survival","Observed survival"), pch=c(1,2), cex=0.5)
points(treatments,equal.lost(total.prawns,true.alive,0.4), pch=2)
dev.off()



#plot(NULL, xlim=c(-5,125),ylim=c(0,1),xlab="Treatment", ylab="Proportion Survived", main="Survival with different loss biases")
#legend(x=100,y=1,c("True survival","Dead lost","Alive lost","Equal lost"), pch=c(1,2,3,4), cex=0.5)
#points(c(0, 60, 120), y=c(0.9, 0.6,0.1))
#points(c(0, 60, 120), y=c(0.92, 0.65,0.12), pch = 2)
#points(c(0, 60, 120), y=c(0.88, 0.56,0.082), pch=3)
#points(c(0, 60, 120), y=c(0.9, 0.6,0.1), pch=4)
