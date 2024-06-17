# Date created: 22-Nov-2023
# Last updated: 29-Apr-2024
# Authors: Jacob Houtman & Emma Atkinson
# Description: Prawn survival experiment (model figures)
# Notes: This code generates the model result figures associated with the paper "XXX" by 
#        co-authors Emma Atkinson, Jacob Houtman, Kyra Ford, and Mark Lewis. 
#
################################################################################

# Load packages #
library(qpdf)
library(viridis)
library(here)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(AICcmodavg)
library(rnaturalearth)
library(grid)
library(devtools)
library(purrr)
library(raster)
library(sf)
library(tolerance)
library(colorRamps)
library(colorspace)
library(Hmisc)

# Read in data #
reflexes<-read.csv(here("data-clean","2023-05-09_prawn_combined_reflex_data.csv"))
survival<-read.csv(here("data-clean","2023-05-09_prawn_combined_survival_data.csv"))
trial<-read.csv(here("data-clean","2023-05-09_prawn_combined_trial_data.csv"))

model.dat = read.csv(here("data-clean","2024-04-29_model_dataframe.csv"))

# Read in model
m1<-readRDS(here("model-outputs-EMA","treat-temp_temp-length_lme4.rds"))
m2<-readRDS(here("model-outputs-EMA","treat-temp_temp-length_TMB.rds"))

# Read in predictions
p1<-read.csv(here("model-outputs-EMA","2024-04-29-predictions-lme4.csv"))
p2<-read.csv(here("model-outputs-EMA","2024-04-29-predictions-TMB.csv"))

#### SET UP PLOTTING TOOLS ####

# Colors 
# cols<-viridis(5)
# cols1<-viridis(5,alpha=0.5)
cols<-diverge_hcl(5, h=c(246,40), c=96)
smoothcols<-diverge_hcl(30, h=c(246,40), c=96)

#heats<-blue2red(length(unique(model.dat$temp)))
heats<-rev(heat_hcl(length(unique(model.dat$temp)), c=c(80,30), l=c(30,90), power=c(1/5, 1.5)))

#Function for colour ramp
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  #dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

#Function for calculating binomial CIs
calc.bi = function(x){
  
  ci=bintol.int(x, n=length(x), alpha=0.05, side=2)
  return(c(ci$p.hat, ci$`2-sided.lower`/length(x), ci$`2-sided.upper`/length(x)))
  
}

################################################################################
# Model predictions relative to temperature (32 mm)
################################################################################

# Prep data
dat32=model.dat[which(model.dat$length>=31 & model.dat$length<=33),]

CI = do.call(data.frame, aggregate(x=dat32$alive, by=list(dat32$trial_number, dat32$treatment), FUN=calc.bi))
colnames(CI)=c("trial","treat","mean","lower","upper")
CI$temp = NA
CI$heatcol=NA
a=1

for (k in unique(dat32$trial_number)){
  CI[CI$trial==k,]$temp = dat32[dat32$trial_number==k,]$temp[1]
}

# assign heat-based colour
for (j in sort(unique(CI$temp))){
  a = a+1
  CI[CI$temp==j,]$heatcol = heats[a]
}


############## TWO-PANEL MODEL PLOT ############################################################################################
png(here("figures-EMA","model-temp-vs-survival.png"), width=800, height=800, units="px", pointsize=14)
par(mfrow=c(1,1), mar=c(5,4,4,0), oma=c(0,0,0,0))

### AGAINST TEMPERATURE
plot(p1$temp, p1$alive_prob, col="white", ylim=c(0,1), bty="n", xlab="Air temperature (deg C)", ylab="Probability of survival")  

# plot data points 

points(CI[CI$treat==0,]$temp, CI[CI$treat==0,]$mean, pch=20, cex=2.5, col=alpha(cols[1],.25))
points(CI[CI$treat==30,]$temp, CI[CI$treat==30,]$mean, pch=20, cex=2.5, col=alpha(cols[2],.25))
points(CI[CI$treat==60,]$temp, CI[CI$treat==60,]$mean, pch=20, cex=2.5, col=alpha(cols[3],.25))
points(CI[CI$treat==90,]$temp, CI[CI$treat==90,]$mean, pch=20, cex=2.5, col=alpha(cols[4],.25))
points(CI[CI$treat==120,]$temp, CI[CI$treat==120,]$mean, pch=20, cex=2.5, col=alpha(cols[5],.25))

polygon(c(p1[p1$treatment==0 & p1$length==32,]$temp, rev(p1[p1$treatment==0 & p1$length==32,]$temp)),c(p1[p1$treatment==0 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==0 & p1$length==32,]$plo_prob)), col=alpha(cols[1],0.25), border=NA)
lines(p1[p1$treatment==0 & p1$length==32,]$temp, p1[p1$treatment==0 & p1$length==32,]$alive_prob, col=cols[1],lwd=2)

polygon(c(p1[p1$treatment==30 & p1$length==32,]$temp, rev(p1[p1$treatment==30 & p1$length==32,]$temp)),c(p1[p1$treatment==30 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==30 & p1$length==32,]$plo_prob)), col=alpha(cols[2],0.25), border=NA)
lines(p1[p1$treatment==30 & p1$length==32,]$temp, p1[p1$treatment==30 & p1$length==32,]$alive_prob, col=cols[2],lwd=2)

polygon(c(p1[p1$treatment==60 & p1$length==32,]$temp, rev(p1[p1$treatment==60 & p1$length==32,]$temp)),c(p1[p1$treatment==60 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==60 & p1$length==32,]$plo_prob)), col=alpha(cols[3],0.25), border=NA)
lines(p1[p1$treatment==60 & p1$length==32,]$temp, p1[p1$treatment==60 & p1$length==32,]$alive_prob, col=cols[3],lwd=2)

polygon(c(p1[p1$treatment==90 & p1$length==32,]$temp, rev(p1[p1$treatment==90 & p1$length==32,]$temp)),c(p1[p1$treatment==90 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==90 & p1$length==32,]$plo_prob)), col=alpha(cols[4],0.25), border=NA)
lines(p1[p1$treatment==90 & p1$length==32,]$temp, p1[p1$treatment==90 & p1$length==32,]$alive_prob, col=cols[4],lwd=2)

polygon(c(p1[p1$treatment==120 & p1$length==32,]$temp, rev(p1[p1$treatment==120 & p1$length==32,]$temp)),c(p1[p1$treatment==120 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==120 & p1$length==32,]$plo_prob)), col=alpha(cols[5],0.25), border=NA)
lines(p1[p1$treatment==120 & p1$length==32,]$temp, p1[p1$treatment==120 & p1$length==32,]$alive_prob, col=cols[5],lwd=2)

subplot(color.bar(smoothcols, min=0, max=120, nticks=5, ticks=seq(0,120,30), title="Minutes \nout of water"), x=24, y=.9, size=c(.75,.75))

dev.off()

### AGAINST TREATMENT
png(here("figures-EMA","model-treatment-vs-survival.png"), width=800, height=800, units="px", pointsize=14)
par(mfrow=c(1,1), mar=c(5,4,4,0), oma=c(0,0,0,0))

plot(p1$treatment, p1$alive_prob, col="white", ylim=c(0,1), bty="n", xlab="Air exposure (minutes out of water)", ylab="Probability of survival")  

points(jitter(CI$treat), CI$mean, col=alpha(CI$heatcol,.25), pch=20, cex=2.5)
polygon(c(p1[p1$temp==10 & p1$length==32,]$treatment, rev(p1[p1$temp==10 & p1$length==32,]$treatment)),c(p1[p1$temp==10 & p1$length==32,]$phi_prob, rev(p1[p1$temp==10 & p1$length==32,]$plo_prob)), col=alpha(heats[1],0.2), border=NA)
lines(p1[p1$temp==10 & p1$length==32,]$treatment, p1[p1$temp==10 & p1$length==32,]$alive_prob, col=heats[1], lwd=2)

polygon(c(p1[p1$temp==18 & p1$length==32,]$treatment, rev(p1[p1$temp==18 & p1$length==32,]$treatment)),c(p1[p1$temp==18 & p1$length==32,]$phi_prob, rev(p1[p1$temp==18 & p1$length==32,]$plo_prob)), col=alpha(heats[10],0.2), border=NA)
lines(p1[p1$temp==18 & p1$length==32,]$treatment, p1[p1$temp==18 & p1$length==32,]$alive_prob, col=heats[10], lwd=2)

polygon(c(p1[p1$temp==26 & p1$length==32,]$treatment, rev(p1[p1$temp==26 & p1$length==32,]$treatment)),c(p1[p1$temp==26 & p1$length==32,]$phi_prob, rev(p1[p1$temp==26 & p1$length==32,]$plo_prob)), col=alpha(heats[19],0.2), border=NA)
lines(p1[p1$temp==26 & p1$length==32,]$treatment, p1[p1$temp==26 & p1$length==32,]$alive_prob, col=heats[19], lwd=2)

subplot(color.bar(heats, min=10, max=26, nticks=8, ticks=seq(0,26,2), title="Air temperature \n(deg C)"), x=110, y=0.9, size=c(.75, .75))
dev.off()