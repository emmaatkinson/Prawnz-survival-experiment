library(here())
library(scales)

rm(list=ls())

reflexes<-read.csv(here("data-clean","2023-05-09_prawn_combined_reflex_data.csv"))

reflexes$treatment = as.numeric(reflexes$treatment)
reflexes = reflexes[reflexes$treatment!=120,]

png(here("figures-EMA", "2025-mar-reflex-fig.png"), width=800, height=600, units="px", pointsize = 20)
plot(jitter(reflexes$treatment, amount=3), jitter(reflexes$score, amount=.5), 
     pch=20, cex=2, col=alpha("darkmagenta",.1),
     bty="n", xaxt="n", yaxt="n",
     xlab="Time out of water (min)",
     ylab="Post-experiment reflex score")
axis(side=1, at=c(0, 30, 60, 90), labels=c(0, 30, 60, 90))
axis(side=2, at=seq(0,10,2), labels=seq(0,10,2))

mean0 = mean(reflexes[reflexes$treatment==0,]$score)
sd0=sd(reflexes[reflexes$treatment==0,]$score)

mean30 = mean(reflexes[reflexes$treatment==30,]$score)
sd30=sd(reflexes[reflexes$treatment==30,]$score)

mean60 = mean(reflexes[reflexes$treatment==60,]$score)
sd60=sd(reflexes[reflexes$treatment==60,]$score)

mean90 = mean(reflexes[reflexes$treatment==90,]$score)
sd90=sd(reflexes[reflexes$treatment==90,]$score)

points(0, mean0, pch=20, cex=2.5, col="black")
segments(0, mean0-sd0, 0, mean0+sd0, lwd=3)

points(30, mean0, pch=20, cex=2.5, col="black")
segments(30, mean0-sd0, 30, mean0+sd0, lwd=3)

points(60, mean0, pch=20, cex=2.5, col="black")
segments(60, mean0-sd0, 60, mean0+sd0, lwd=3)

points(90, mean0, pch=20, cex=2.5, col="black")
segments(90, mean0-sd0, 90, mean0+sd0, lwd=3)
dev.off()
