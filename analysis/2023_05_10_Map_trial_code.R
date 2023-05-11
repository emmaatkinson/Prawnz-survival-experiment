reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data")
survival<-read.csv("2023-05-09_prawn_combined_survival_data")
trial<-read.csv("2023-05-09_prawn_combined_trial_data")

attach(reflexes)
attach(trial)
attach(survival)

library("tidyverse")
install.packages("googlesheets4")
install.packages("lubridate")
install.packages("openintro")
install.packages("maps")
install.packages("ggmap")


n_edge<-max(max(append(trial$exp_set_lat_1,trial$exp_set_lat_2),na.rm=TRUE), max(append(trial$exp_haul_lat_1,trial$exp_haul_lat_2),na.rm=TRUE)) #select largest latitude, find a northern bound for edge of map
s_edge<-min(min(append(trial$exp_set_lat_1,trial$exp_set_lat_2),na.rm=TRUE), min(append(trial$exp_haul_lat_1,trial$exp_haul_lat_2),na.rm=TRUE))#smallest latitude, find a southern bound for edge of map
w_edge<-(-1)*max(max(append(trial$exp_set_lon_1,trial$exp_set_lon_2),na.rm=TRUE), max(append(trial$exp_haul_lon_1,trial$exp_haul_lon_2),na.rm=TRUE))#select largest longitude, find a western bound for edge of map
e_edge<-(-1)*min(min(append(trial$exp_set_lon_1,trial$exp_set_lon_2),na.rm=TRUE), min(append(trial$exp_haul_lon_1,trial$exp_haul_lon_2),na.rm=TRUE))#smallest longitude, find a eastern bound for edge of map


broughton_map<-ggmap::get_stamenmap(bbox=c(left=w_edge,bottom=s_edge,right=e_edge,top=n_edge),maptype="terrain-background",zoom=12)
ggmap::ggmap(broughton_map)

ggmap::ggmap(broughton_map) + ggplot2::geom_point(data=df1, ggplot2::aes(x=s_lat_1, y=s_lon_1), size=10)

df1<-data.frame(s_lat_1,s_lon_1)

s_lat_1<-trial$exp_set_lat_1[!is.na(trial$exp_set_lat_1)]
s_lon_1<-trial$exp_set_lon_1[!is.na(trial$exp_set_lon_1)]



