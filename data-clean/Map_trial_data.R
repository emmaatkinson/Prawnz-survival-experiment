setwd(here("data-clean"))
getwd()

#Read in dataset
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")


library("tidyverse")
library("googlesheets4")
library("lubridate")
library("openintro")
library("maps")
library("ggmap")


n_edge<-max(max(append(trial$exp_set_lat_1,trial$exp_set_lat_2),na.rm=TRUE), max(append(trial$exp_haul_lat_1,trial$exp_haul_lat_2),na.rm=TRUE)) #select largest latitude, find a northern bound for edge of map
s_edge<-min(min(append(trial$exp_set_lat_1,trial$exp_set_lat_2),na.rm=TRUE), min(append(trial$exp_haul_lat_1,trial$exp_haul_lat_2),na.rm=TRUE))#smallest latitude, find a southern bound for edge of map
w_edge<-(-1)*max(max(append(trial$exp_set_lon_1,trial$exp_set_lon_2),na.rm=TRUE), max(append(trial$exp_haul_lon_1,trial$exp_haul_lon_2),na.rm=TRUE))#select largest longitude, find a western bound for edge of map
e_edge<-(-1)*min(min(append(trial$exp_set_lon_1,trial$exp_set_lon_2),na.rm=TRUE), min(append(trial$exp_haul_lon_1,trial$exp_haul_lon_2),na.rm=TRUE))#smallest longitude, find a eastern bound for edge of map


broughton_map<-ggmap::get_stamenmap(bbox=c(left=w_edge,bottom=s_edge,right=e_edge,top=n_edge),maptype="terrain",zoom=12)
ggmap::ggmap(broughton_map)
ggmap::ggmap(broughton_map) + ggplot2::geom_point(data=df1, ggplot2::aes(y=s_lat_1, x=s_lon_1), size=1)

map_df1<-data.frame(s_lat_1,s_lon_1=-s_lon_1)

s_lat_1<-trial$exp_set_lat_1[!is.na(trial$exp_set_lat_1)]
s_lon_1<-trial$exp_set_lon_1[!is.na(trial$exp_set_lon_1)]


# Load required libraries
library('rnaturalearth')
library(ggplot2)
library(grid)
install.packages("rnaturalearthdata")
# Download world polygon data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define region of interest for the inset map (e.g. Europe)
xmin <- w_edge
xmax <- e_edge
ymin <- s_edge
ymax <- n_edge

bcn<-52
bcs<-48
bcw<--130
bce<--120

install.packages("ggthemes")

# Create main plot
main_plot <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(bcw, bce), ylim = c(bcs, bcn)) +
  ggtitle("World Map") +
  theme_dark()+
  # Add a rectangle to represent the inset area on the main map
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = "black", fill = NA, data = data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE)

# Create an inset map
inset_map <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme_void() +
  theme(plot.background = element_rect(color = "black", size = 1))

# Add the inset map to the main plot
vp <- viewport(width = 0.3, height = 0.3, x = 0.8, y = 0.2)
print(main_plot)
print(inset_map, vp = vp)



# Create main plot
main_plot<-ggmap::get_stamenmap(bbox=c(left=-130,bottom=48,right=-122,top=52),zoom=8)
ggmap(main_plot)
#INSET MAP
broughton_map<-ggmap::get_stamenmap(bbox=c(left=w_edge,bottom=s_edge,right=e_edge,top=n_edge),zoom=12)
inset_plot<-ggmap::ggmap(broughton_map) + ggplot2::geom_point(data=map_df1, ggplot2::aes(y=s_lat_1, x=s_lon_1), size=1)
ggmap(broughton_map)
# Add the inset map to the main plot
vp <- v  iewport(width = 0.3, height = 0.3, x = 0.8, y = 0.2)
ggmap(main_plot)
ggmap(broughton_map, vp = vp)
