# Date created: 22-Nov-2023
# Last updated: [Current Date - Updated color schemes]
# Authors: Jacob Houtman & Emma Atkinson
# Description: Prawn survival experiment (model figures) - REVISED COLOR SCHEMES
# Notes: This code generates the model result figures with updated PNWColors palettes
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
library(PNWColors)

rm(list=ls())

# Read in data #
reflexes<-read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_reflex_data.csv"))
survival<-read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_survival_data.csv"))
trial<-read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_trial_data.csv"))

model.dat = read.csv(here("data-clean","2024-04-29_model_dataframe.csv"))

# Read in model
m1<-readRDS(here("model-outputs-EMA","treat-temp_temp-length_lme4.rds"))
m2<-readRDS(here("model-outputs-EMA","treat-temp_temp-length_TMB.rds"))

# Read in predictions
p1<-read.csv(here("model-outputs-EMA","2024-04-29-predictions-lme4.csv"))
p2<-read.csv(here("model-outputs-EMA","2024-04-29-predictions-TMB.csv"))

#### SET UP PLOTTING TOOLS ####

# Colors - UPDATED WITH PNWCOLORS PALETTES
cols_full <- pnw_palette("Anemone", 8)  # Get more colors
cols <- cols_full[c(1, 3, 5, 7, 8)]  # Pick non-adjacent colors for better contrast
smoothcols_full <- pnw_palette("Anemone", 40)
smoothcols <- smoothcols_full[seq(1, 40, length.out = 30)]

# For temperature gradient - using Sunset2 palette
n_temps <- length(unique(model.dat$temp))
heats <- pnw_palette("Sunset2", n_temps)

# Function for colour ramp - UPDATED FOR BETTER LEGEND
color.bar.external <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='', labels=NULL) {
  scale = (length(lut)-1)/(max-min)
  
  # Create a new plot region for the legend
  plot(c(0,2), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main='', cex.main=1.2)  # Remove main title here
  
  # If labels not provided, use ticks
  if(is.null(labels)) labels <- ticks
  
  # Add axis with labels - mgp controls distance from axis
  # mgp[1] = axis title distance, mgp[2] = axis labels distance, mgp[3] = axis line distance
  axis(2, at=ticks, labels=labels, las=1, cex.axis=1.1, mgp=c(3, 0.7, 0))
  
  # Draw color bar - adjusted positioning (moved left, closer to main plot)
  bar_left <- 0.2    # Closer to main plot
  bar_right <- 1.0   # Standard width
  
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(bar_left, y, bar_right, y+1/scale, col=lut[i], border=NA)
  }
  
  # Add border around color bar
  rect(bar_left, min, bar_right, max, border="black", lwd=1)
  
  # Add centered title above the color bar
  bar_center <- (bar_left + bar_right) / 2
  mtext(title, side=3, line=0.5, at=bar_center, cex=1.2)
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

for (k in unique(dat32$trial_number)){
  CI[CI$trial==k,]$temp = dat32[dat32$trial_number==k,]$temp[1]
}

# Assign heat-based colour - UPDATED INDEXING
temp_values <- sort(unique(CI$temp))
for (i in 1:length(temp_values)){
  CI[CI$temp==temp_values[i],]$heatcol = heats[i]
}

############## TEMPERATURE VS SURVIVAL - WITH EXTERNAL LEGEND ############################
png(here("figures-EMA","aug8-2025-model-temp-vs-survival.png"), width=900, height=600, units="px", pointsize=16)

# Set up layout with space for legend on the right
layout(matrix(c(1,2), nrow=1), widths=c(4,1))

# Main plot
par(mar=c(5,4,4,1))
plot(p1$temp, p1$alive_prob, col="white", ylim=c(0,1), bty="n", 
     xlab="Air temperature (°C)", ylab="Probability of survival", 
     cex.lab=1.2, cex.axis=1.2, bty="l")  

# Plot data points with updated colors
points(CI[CI$treat==0,]$temp, CI[CI$treat==0,]$mean, pch=21, cex=2, col=alpha("black", 0.4), bg=alpha(cols[1],.6))
points(CI[CI$treat==30,]$temp, CI[CI$treat==30,]$mean, pch=21, cex=2, col=alpha("black", 0.4), bg=alpha(cols[2],.6))
points(CI[CI$treat==60,]$temp, CI[CI$treat==60,]$mean, pch=21, cex=2, col=alpha("black", 0.4), bg=alpha(cols[3],.6))
points(CI[CI$treat==90,]$temp, CI[CI$treat==90,]$mean, pch=21, cex=2, col=alpha("black", 0.4), bg=alpha(cols[4],.6))
points(CI[CI$treat==120,]$temp, CI[CI$treat==120,]$mean, pch=21, cex=2, col=alpha("black", 0.4), bg=alpha(cols[5],.6))

# Plot model predictions with confidence intervals
polygon(c(p1[p1$treatment==0 & p1$length==32,]$temp, rev(p1[p1$treatment==0 & p1$length==32,]$temp)),
        c(p1[p1$treatment==0 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==0 & p1$length==32,]$plo_prob)), 
        col=alpha(cols[1],0.4), border=NA)
lines(p1[p1$treatment==0 & p1$length==32,]$temp, p1[p1$treatment==0 & p1$length==32,]$alive_prob, 
      col=cols[1], lwd=2)

polygon(c(p1[p1$treatment==30 & p1$length==32,]$temp, rev(p1[p1$treatment==30 & p1$length==32,]$temp)),
        c(p1[p1$treatment==30 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==30 & p1$length==32,]$plo_prob)), 
        col=alpha(cols[2],0.4), border=NA)
lines(p1[p1$treatment==30 & p1$length==32,]$temp, p1[p1$treatment==30 & p1$length==32,]$alive_prob, 
      col=cols[2], lwd=2)

polygon(c(p1[p1$treatment==60 & p1$length==32,]$temp, rev(p1[p1$treatment==60 & p1$length==32,]$temp)),
        c(p1[p1$treatment==60 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==60 & p1$length==32,]$plo_prob)), 
        col=alpha(cols[3],0.4), border=NA)
lines(p1[p1$treatment==60 & p1$length==32,]$temp, p1[p1$treatment==60 & p1$length==32,]$alive_prob, 
      col=cols[3], lwd=2)

polygon(c(p1[p1$treatment==90 & p1$length==32,]$temp, rev(p1[p1$treatment==90 & p1$length==32,]$temp)),
        c(p1[p1$treatment==90 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==90 & p1$length==32,]$plo_prob)), 
        col=alpha(cols[4],0.4), border=NA)
lines(p1[p1$treatment==90 & p1$length==32,]$temp, p1[p1$treatment==90 & p1$length==32,]$alive_prob, 
      col=cols[4], lwd=2)

polygon(c(p1[p1$treatment==120 & p1$length==32,]$temp, rev(p1[p1$treatment==120 & p1$length==32,]$temp)),
        c(p1[p1$treatment==120 & p1$length==32,]$phi_prob, rev(p1[p1$treatment==120 & p1$length==32,]$plo_prob)), 
        col=alpha(cols[5],0.4), border=NA)
lines(p1[p1$treatment==120 & p1$length==32,]$temp, p1[p1$treatment==120 & p1$length==32,]$alive_prob, 
      col=cols[5], lwd=2)

# Legend panel
par(mar=c(5,1,4,4))
color.bar.external(smoothcols, min=0, max=120, nticks=5, 
                   ticks=seq(0,120,30), 
                   title="Minutes out\nof water",
                   labels=c("0", "30", "60", "90", "120"))

dev.off()

############## TREATMENT VS SURVIVAL - WITH EXTERNAL LEGEND ############################
png(here("figures-EMA","aug8-2025-model-treatment-vs-survival.png"), width=900, height=600, units="px", pointsize=16)

# Set up layout with space for legend on the right
layout(matrix(c(1,2), nrow=1), widths=c(4,1))

# Main plot
par(mar=c(5,4,4,2))
plot(p1$treatment, p1$alive_prob, col="white", ylim=c(0,1), bty="n", xlim=c(-2,130), 
     xlab="Air exposure (minutes out of water)", ylab="Probability of survival",
     cex.lab=1.2, cex.axis=1.1,bty="l")  

# Plot data points with temperature-based colors
points(jitter(CI$treat,2), CI$mean, col=alpha("black",0.4), bg=alpha(CI$heatcol,.6), pch=21, cex=2)

# Find indices for specific temperatures in the heats palette
temp_10_idx <- which(temp_values == 10.7)
temp_18_idx <- which(temp_values == 17.8)
temp_26_idx <- which(temp_values == 25.7)

# Plot model predictions for different temperatures
polygon(c(p1[p1$temp==10 & p1$length==32,]$treatment, rev(p1[p1$temp==10 & p1$length==32,]$treatment)),
        c(p1[p1$temp==10 & p1$length==32,]$phi_prob, rev(p1[p1$temp==10 & p1$length==32,]$plo_prob)), 
        col=alpha(heats[temp_10_idx],0.2), border=NA)
lines(p1[p1$temp==10 & p1$length==32,]$treatment, p1[p1$temp==10 & p1$length==32,]$alive_prob, 
      col=heats[temp_10_idx], lwd=2)

polygon(c(p1[p1$temp==18 & p1$length==32,]$treatment, rev(p1[p1$temp==18 & p1$length==32,]$treatment)),
        c(p1[p1$temp==18 & p1$length==32,]$phi_prob, rev(p1[p1$temp==18 & p1$length==32,]$plo_prob)), 
        col=alpha(heats[temp_18_idx],0.2), border=NA)
lines(p1[p1$temp==18 & p1$length==32,]$treatment, p1[p1$temp==18 & p1$length==32,]$alive_prob, 
      col=heats[temp_18_idx], lwd=2)

polygon(c(p1[p1$temp==26 & p1$length==32,]$treatment, rev(p1[p1$temp==26 & p1$length==32,]$treatment)),
        c(p1[p1$temp==26 & p1$length==32,]$phi_prob, rev(p1[p1$temp==26 & p1$length==32,]$plo_prob)), 
        col=alpha(heats[temp_26_idx],0.2), border=NA)
lines(p1[p1$temp==26 & p1$length==32,]$treatment, p1[p1$temp==26 & p1$length==32,]$alive_prob, 
      col=heats[temp_26_idx], lwd=2)

# Legend panel
par(mar=c(5,1,4,4))
# Get min and max temperatures for the legend
min_temp <- min(model.dat$temp)
max_temp <- max(model.dat$temp)
color.bar.external(heats, min=min_temp, max=max_temp, 
                   nticks=9, 
                   ticks=seq(min_temp, max_temp, by=2),
                   title="Air temperature\n(°C)")

dev.off()
