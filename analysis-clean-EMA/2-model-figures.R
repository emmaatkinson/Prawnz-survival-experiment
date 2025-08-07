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

################################################################################
# Generate boot-strapped confidence intervals for raw survival data 
################################################################################

# Break temperature vector into 4 sections
hist_temps<-hist(trial$exp_set_temp_air,plot=FALSE, breaks= c(9,13,17,21,26))

# Colors 
cols<-viridis(4)
cols1<-viridis(4, alpha=0.5)

# This function makes one of the three panels. 
# It takes a survival data set based on length section and a particular length.
# The first part further cuts the data set by temperature and then treatment time and creates a bootstrap 95% Confidence Intervals. 
# The second part creates model predictions for the mid_length and for 4 different temperatures.
# The model prediction curves are displayed in a figure with the bootstrap CIs. 

boot_dat <-function(LENGTH_DATA, mid_length){
  
  # LENGTH_DATA is a subset of the survival data with only the specified length bin. 
  # It will be used to calculate per-treatment, temperature-binned, average survival.
  
  # mid_length is the middle of the length bin. It is used for model prediction. 
  n_temp_bins<-length(hist_temps$breaks)-1
  
  temp_list<-vector(length=0)
  treat_list<-vector(length=0)
  mu_list<-vector(length=0)
  upper_list<-vector(length=0)
  lower_list<-vector(length=0)
  n_list<-vector(length=0)
  
  #We are going to repeat the following procedure for each temperature bin 
  for (i in 1:n_temp_bins){
    
    #Start with Length data in given length bin:
    temp_mid<-hist_temps$mids[i]
    
    #Temperature boundaries for the temperature bin in this iteration of the 'for' loop.
    temp_bound<-hist_temps$breaks[c(i,i+1)]
    
    #Subset data to current temperature bin
    temp_bin_data<-LENGTH_DATA[between(LENGTH_DATA$temp,temp_bound[1],temp_bound[2]),]
    
    unique_treatments<-unique(temp_bin_data$treatment)
    
    #We are going to repeat the following procedure for each Treatment, within this temperature bin
    for (x in 1:length(unique_treatments)){
      
      #
      current_treat<-unique_treatments[x]
      
      #Subset temp-binned data to current treatment
      treat_temp_bin_data<-temp_bin_data[which(temp_bin_data$treatment==current_treat),]$alive
      
      #A vector to store sampled survival proportions
      boot<-vector(length=1000)
      
      #Sample from calculated survival proportions, with replacement
      for (k in 1:1000){
        
        #survival_props contains one entry for for each trial-trap in the temperature bin.
        #There were 123 trial-trap levels total, some temperature bins may have over 60, some only 12.
        #the length of the survival_props vector equals the value of n_trial_traps
        
        #This line samples, with replacement, from the survival proportions calculated for each trial-trap level in the temperature bin.
        #It takes a number of samples equal to the nummber of trial-trap levels and stores it in the 'boot' vector
        boot[k]<-mean(sample(treat_temp_bin_data,replace=TRUE,size=length(treat_temp_bin_data)))
      }
      
      #Store temperature bin, specifically the middle of the bin
      temp_list<-append(temp_list,temp_mid)
      
      #Store 
      treat_list<-append(treat_list,current_treat)
      
      #Store 97.5th survival percentile for upper CI limit
      upper_list<-append(upper_list,quantile(boot, probs=0.975))
      
      #Store 2.5th survival percentile for upper CI limit
      lower_list<-append(lower_list,quantile(boot, probs=0.025))
      
      #Store 2.5th survival percentile for upper CI limit
      mu_list<-append(mu_list,mean(treat_temp_bin_data))
      
      #Number of prawns in each bin (some have very few)
      n_list<-append(n_list, length(treat_temp_bin_data))
    }
    
  }
  #Add jitter to the x values of the points, so they don't overlap
  jitter<-runif(length(temp_list),min=-2,max=2)
  
  #Create data frame
  df<-data.frame(temp=temp_list,treat=treat_list+jitter,upper=upper_list,lower=lower_list,mu=mu_list,n=n_list)
  
}  
  
shorties<-model.dat[which(model.dat$length<=27.5),]
normies<-model.dat[which(model.dat$length>=27.5 & model.dat$length<=36.5),]
biggies<-model.dat[which(model.dat$length>=36.5),]

bootshort <- boot_dat(shorties,round(mean(shorties$length)))
bootnorm <- boot_dat(normies,round(mean(norties$length)))
bootbig <- boot_dat(biggies,round(mean(biggies$length)))

short<-round(mean(shorties$length))
norm<-round(mean(normies$length))
big<-round(mean(biggies$length))

plot(p1$treatment, p1$alive_prob, col="white", ylim=c(0,1), bty="n")  

points(bootshort[bootshort$temp==11,]$treat, bootshort[bootshort$temp==11,]$mu)  
segments(bootshort[bootshort$temp==11,]$treat, bootshort[bootshort$temp==11,]$lower,bootshort[bootshort$temp==11,]$treat, bootshort[bootshort$temp==11,]$upper)  
polygon(c(p1[p1$temp==11&p1$length==short,]$treatment,rev(p1[p1$temp==11&p1$length==short,]$treatment)), c(p1[p1$temp==11&p1$length==short,]$phi_prob, rev(p1[p1$temp==11&p1$length==short,]$plo_prob)), col=alpha("black",.25), border=NA)
lines(p1[p1$temp==11&p1$length==short,]$treatment, p1[p1$temp==11&p1$length==short,]$alive_prob, lwd=2)

points(bootshort[bootshort$temp==15,]$treat, bootshort[bootshort$temp==15,]$mu)  
segments(bootshort[bootshort$temp==15,]$treat, bootshort[bootshort$temp==15,]$lower,bootshort[bootshort$temp==15,]$treat, bootshort[bootshort$temp==15,]$upper)  
polygon(c(p1[p1$temp==15&p1$length==short,]$treatment,rev(p1[p1$temp==15&p1$length==short,]$treatment)), c(p1[p1$temp==15&p1$length==short,]$phi_prob, rev(p1[p1$temp==15&p1$length==short,]$plo_prob)), col=alpha("black",.25), border=NA)
lines(p1[p1$temp==15&p1$length==short,]$treatment, p1[p1$temp==15&p1$length==short,]$alive_prob, lwd=2)

points(bootshort[bootshort$temp==19,]$treat, bootshort[bootshort$temp==19,]$mu)  
segments(bootshort[bootshort$temp==19,]$treat, bootshort[bootshort$temp==19,]$lower,bootshort[bootshort$temp==19,]$treat, bootshort[bootshort$temp==19,]$upper)  
polygon(c(p1[p1$temp==19&p1$length==short,]$treatment,rev(p1[p1$temp==19&p1$length==short,]$treatment)), c(p1[p1$temp==19&p1$length==short,]$phi_prob, rev(p1[p1$temp==19&p1$length==short,]$plo_prob)), col=alpha("black",.25), border=NA)
lines(p1[p1$temp==19&p1$length==short,]$treatment, p1[p1$temp==19&p1$length==short,]$alive_prob, lwd=2)

points(bootshort[bootshort$temp==23.5,]$treat, bootshort[bootshort$temp==23.5,]$mu)  
segments(bootshort[bootshort$temp==23.5,]$treat, bootshort[bootshort$temp==23.5,]$lower,bootshort[bootshort$temp==23.5,]$treat, bootshort[bootshort$temp==23.5,]$upper)  
polygon(c(p1[p1$temp==24&p1$length==short,]$treatment,rev(p1[p1$temp==24&p1$length==short,]$treatment)), c(p1[p1$temp==24&p1$length==short,]$phi_prob, rev(p1[p1$temp==24&p1$length==short,]$plo_prob)), col=alpha("black",.25), border=NA)
lines(p1[p1$temp==24&p1$length==short,]$treatment, p1[p1$temp==24&p1$length==short,]$alive_prob, lwd=2)


















  
  #Calculate model predictions with 95% CI for prawns left out of water for 0-120 minutes
  CInew1<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[1],121),length=rep(mid_length,121),treatment=c(0:120))) 
  CInew2<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[2],121),length=rep(mid_length,121),treatment=c(0:120))) 
  CInew3<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[3],121),length=rep(mid_length,121),treatment=c(0:120))) 
  CInew4<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[4],121),length=rep(mid_length,121),treatment=c(0:120))) 
  
  #Store colours for plot
  cols<-viridis(4)
  cols1<-viridis(4,alpha=0.5)
  
  #Name colours for ggplot legend
  colors<-c("11\u00B0C"= cols[1],"15\u00B0C"=cols[2] ,"19\u00B0C"=cols[3], "23.5\u00B0C"= cols[4])
  
  #X values for the graph
  x=0:120
  
  #The geom_line arguments here give the model predictions for the given length and temperature, accross treatment times
  return(ggplot(data = NULL, aes(x=x))+geom_line(aes(y=CInew1$mod.avg.pred, color="11\u00B0C"), lwd = 1)+  geom_line(aes(y=CInew2$mod.avg.pred, color="15\u00B0C"), lwd = 1) +
           geom_line(aes(y=CInew3$mod.avg.pred, color="19\u00B0C"), lwd = 1) +geom_line(aes(y=CInew4$mod.avg.pred, color="23.5\u00B0C"), lwd = 1)+
           
           #The geom_ribbon adds the 95% confidence interval around the model predictions
           geom_ribbon(aes(ymin=CInew1$lower.CL, ymax=CInew1$upper.CL), alpha=0.3, fill = cols[1],  color = cols[1], linetype = "dotted")+
           geom_ribbon(aes(ymin=CInew2$lower.CL, ymax=CInew2$upper.CL), alpha=0.3, fill = cols[2],  color = cols[2], linetype = "dotted")+
           geom_ribbon(aes(ymin=CInew3$lower.CL, ymax=CInew3$upper.CL), alpha=0.3, fill = cols[3],  color = cols[3], linetype = "dotted")+
           geom_ribbon(aes(ymin=CInew4$lower.CL, ymax=CInew4$upper.CL), alpha=0.3, fill = cols[4],  color = cols[4], linetype = "dotted")+
           
           #The geom_point arguments add real survival averages for prawns in the temperature and treatment bin
           geom_point(data=df[which(df$temp==11),], aes(x=treat,y=mu),color=cols[1])+geom_point(data=df[which(df$temp==15),], aes(x=treat,y=mu),color=cols[2])+
           geom_point(data=df[which(df$temp==19),], aes(x=treat,y=mu),color=cols[3])+geom_point(data=df[which(df$temp==23.5),], aes(x=treat,y=mu),color=cols[4])+
           
           #geom_error bar adds error bars (no surprise) to the points, calculated by bootstrap above
           geom_errorbar(data = df,aes(x=treat,ymin=lower, ymax=upper), width=.2, alpha=0.4)+
           labs(x="Time out of water",y="Probability of survival", color="Air temperature (\u00B0C)", title= paste("Survival for",mid_length,"mm prawn"))+scale_color_manual(values = cols1)
         +theme(legend.position = c(.95, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6),plot.title = element_text(hjust = 0.5),panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", color = "grey50")))
}

#Cut up larger data frame by length
shorties<-model_df_2[which(model_df_2$length<=27.5),]
normies<-model_df_2[which(model_df_2$length>=27.5 & model_df_2$length<=36.5),]
biggies<-model_df_2[which(model_df_2$length>=36.5),]


setwd(here("New-figures"))
tiff(paste(Sys.Date(), "survival_by_temp_three_panel.tiff", sep="_"),width=1000,height=1200)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))

#Arrange three panels
gridExtra::grid.arrange(conf_maker(shorties, round(mean(shorties$length))),
                        conf_maker(normies, round(mean(normies$length))),
                        conf_maker(biggies, round(mean(biggies$length))))
dev.off()
