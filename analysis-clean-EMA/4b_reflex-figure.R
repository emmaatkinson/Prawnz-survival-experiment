library(here)
library(scales)
library(PNWColors)

rm(list=ls())

reflexes<-read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_reflex_data.csv"))

reflexes$treatment = as.numeric(reflexes$treatment)
reflexes = reflexes[reflexes$treatment!=120,]

anemcols = pnw_palette(name="Anemone", n=7)

png(here("figures-EMA", "2025-aug-reflex-fig.png"), width=800, height=600, units="px", pointsize = 20)
plot(jitter(rep(0, nrow(reflexes[reflexes$treatment==0,])), amount=3), jitter(reflexes[reflexes$treatment==0,]$score, amount=.5), 
     pch=20, cex=2, col=alpha(anemcols[1],.1),
     bty="n", xaxt="n", yaxt="n", xlim=c(-5,110),
     xlab="Time out of water (min)",
     ylab="Post-experiment reflex score")
points(jitter(rep(30, nrow(reflexes[reflexes$treatment==30,])), amount=3), 
       jitter(reflexes[reflexes$treatment==30,]$score, amount=.5),
       pch=20, col=alpha(anemcols[1],0.1), cex=2)
points(jitter(rep(60, nrow(reflexes[reflexes$treatment==60,])), amount=3), 
       jitter(reflexes[reflexes$treatment==60,]$score, amount=.5),
       pch=20, col=alpha(anemcols[1],0.1), cex=2)
points(jitter(rep(90, nrow(reflexes[reflexes$treatment==90,])), amount=3), 
       jitter(reflexes[reflexes$treatment==90,]$score, amount=.5),
       pch=20, col=alpha(anemcols[1],0.1), cex=2)

axis(side=1, at=c(0, 30, 60, 90), labels=c(0, 30, 60, 90))
axis(side=2, at=seq(0,10,2), labels=seq(0,10,2))
box(bty="l")

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
