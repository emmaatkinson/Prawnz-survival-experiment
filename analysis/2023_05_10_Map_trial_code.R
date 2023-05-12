reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data")
survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")

attach(reflexes)
attach(trial)
attach(survival)

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

#
s_lat_1<-trial$exp_set_lat_1[!is.na(trial$exp_set_lat_1)]
s_lon_1<--1*trial$exp_set_lon_1[!is.na(trial$exp_set_lon_1)]

h_lat_1<-trial$exp_haul_lat_1[!is.na(trial$exp_haul_lat_1)]
h_lon_1<--1*trial$exp_haul_lon_1[!is.na(trial$exp_haul_lon_1)]

dfs<-data.frame(s_lat_1,s_lon_1)
dfh<-data.frame(h_lat_1,h_lon_1)

broughton_map<-ggmap::get_stamenmap(bbox=c(left=w_edge,bottom=s_edge,right=e_edge,top=n_edge),maptype="terrain-background",zoom=12)
ggmap::ggmap(broughton_map)

#OR

broughton_map<-ggmap::get_stamenmap(bbox=c(left=-126.64,bottom=50.3788,right=-125.799,top=50.976),maptype="terrain-background",zoom=9)
ggmap::ggmap(broughton_map)

ggmap::ggmap(broughton_map) + ggplot2::geom_point(data=dfs, ggplot2::aes(x=s_lon_1, y=s_lat_1), size=1)+ ggplot2::geom_point(data=dfh, ggplot2::aes(x=h_lon_1, y=h_lat_1), size=1, col="red")



#SOMETHINGS WRONG
eb<-subset(trial, site_name="Echo_Bay")

#real echo bay is 50.7508 and -126.4975
real_lat<-50.7508 
real_lon<--126.4975

70*(real_lon+mean(eb$exp_haul_lon_1))
mean(eb$exp_set_lon_1)
mean(eb$exp_haul_lon_2)
mean(eb$exp_set_lon_2)
111*(real_lat-mean(eb$exp_haul_lat_1))
mean(eb$exp_set_lat_1)
mean(eb$exp_haul_lat_2)
mean(eb$exp_set_lat_2)




