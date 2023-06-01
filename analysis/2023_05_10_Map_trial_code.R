setwd("/Users/jacobhoutman/Documents/Git Hub/Prawnz-survival-experiment")
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")


library("tidyverse")
library("googlesheets4")
library("lubridate")
library("openintro")
library("maps")
library("ggmap")

#Define Largest and smallest latitude and longitude to set edges of the map
n_edge<-max(max(append(trial$exp_set_lat_1,trial$exp_set_lat_2),na.rm=TRUE), max(append(trial$exp_haul_lat_1,trial$exp_haul_lat_2),na.rm=TRUE)) #select largest latitude, find a northern bound for edge of map
s_edge<-min(min(append(trial$exp_set_lat_1,trial$exp_set_lat_2),na.rm=TRUE), min(append(trial$exp_haul_lat_1,trial$exp_haul_lat_2),na.rm=TRUE))#smallest latitude, find a southern bound for edge of map
w_edge<-(-1)*max(max(append(trial$exp_set_lon_1,trial$exp_set_lon_2),na.rm=TRUE), max(append(trial$exp_haul_lon_1,trial$exp_haul_lon_2),na.rm=TRUE))#select largest longitude, find a western bound for edge of map
e_edge<-(-1)*min(min(append(trial$exp_set_lon_1,trial$exp_set_lon_2),na.rm=TRUE), min(append(trial$exp_haul_lon_1,trial$exp_haul_lon_2),na.rm=TRUE))#smallest longitude, find a eastern bound for edge of map

#Round up for aesthetics
n_edge<-51
s_edge<-50.7
w_edge<--126.7
e_edge<--126.4

#Vectors without NAs, set 1,2 and haul 1,2
s_lat_1<-trial$exp_set_lat_1[!is.na(trial$exp_set_lat_1)]
s_lon_1<--1*trial$exp_set_lon_1[!is.na(trial$exp_set_lon_1)]

h_lat_1<-trial$exp_haul_lat_1[!is.na(trial$exp_haul_lat_1)]
h_lon_1<--1*trial$exp_haul_lon_1[!is.na(trial$exp_haul_lon_1)]

s_lat_2<-trial$exp_set_lat_2[!is.na(trial$exp_set_lat_2)]
s_lon_2<--1*trial$exp_set_lon_2[!is.na(trial$exp_set_lon_2)]

h_lat_2<-trial$exp_haul_lat_2[!is.na(trial$exp_haul_lat_2)]
h_lon_2<--1*trial$exp_haul_lon_2[!is.na(trial$exp_haul_lon_2)]

# Dataframes without NAs, set 1,2 and haul 1,2
dfs1<-data.frame(s_lat_1,s_lon_1)
dfh1<-data.frame(h_lat_1,h_lon_1)
dfs2<-data.frame(s_lat_2,s_lon_2)
dfh2<-data.frame(h_lat_2,h_lon_2)

#map of survey area 
broughton_map<-ggmap::get_stamenmap(bbox=c(left=w_edge,bottom=s_edge,right=e_edge,top=n_edge),maptype="terrain-background",zoom=12)
ggmap::ggmap(broughton_map)

#Apply trial locations to map
setwd(here("figures"))
png(filename=paste(Sys.Date(), "broughton_map.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggmap::ggmap(broughton_map) + ggplot2::geom_point(data=dfs1, ggplot2::aes(x=s_lon_1, y=s_lat_1), size=1)
dev.off() 
getwd()





