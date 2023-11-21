setwd(here("data-clean"))
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")


library("tidyverse")
library("googlesheets4")
library("lubridate")
library("openintro")
library("maps")
library("ggmap")
library(rgdal)
library(maptools)

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

##MAP 2----

# Load required libraries
library(rnaturalearth)
library(ggplot2)
library(grid)
library(devtools)
devtools::install_github("ropensci/rnaturalearthhires")

# Download world polygon data
world <- ne_countries(scale = "large", returnclass = "sf")
world1<- ne_countries(scale = "large", returnclass = "sf")

lakes110 <- ne_download(scale = 10, type = "admin_1_states_provinces", category = "cultural")
sp::plot(lakes110)

library(sf)
states <- st_as_sf(map("province", plot = FALSE, fill = TRUE))
head(states)

# Define region of interest for the inset map (e.g. Europe)
n_edge<-58
s_edge<-45
w_edge<--140
e_edge<--122

n_edge1<-51.3
s_edge1<-50.3
w_edge1<--127.5
e_edge1<--125.5

#Data frame for map
world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")

#Labels and coordinates
long<-c(-126, -124)
lat<-c(49.5,53)
names<-c("VI","BC")


##Data frame for BC boundary
xx<-PROV[1,]$geometry[[1]][1]
bc_boundary<-data.frame(xx[[1]][1])
#remove the coast since that is included in the world data,
bc_boundary1<-bc_boundary[4200:nrow(bc_boundary),]


# Create main plot
main_plot <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(w_edge, e_edge), ylim = c(s_edge, n_edge)) +
  xlab("Longitude")+ylab("Latitude")+
  theme_minimal() +
  # Add a rectangle to represent the inset area on the main map
  geom_rect(aes(xmin = w_edge1, xmax = e_edge1, ymin = s_edge1, ymax = n_edge1),
            color = "black", fill = NA, data = data.frame(xmin = w_edge1, xmax = e_edge1, ymin = s_edge1, ymax = n_edge1),
            inherit.aes = FALSE)+
  #Add labels for BC and VI
  geom_text(label=names[1], 
            x=long[1],
            y=lat[1]
            , size = 3, hjust=0, vjust=-1)+
  geom_text(label=names[2], 
            x=long[2],
            y=lat[2]
            , size = 3, hjust=0, vjust=-1) +
  geom_path(data=bc_boundary1, aes(X1,X2), colour = "darkgrey")
main_plot

# Create an inset map
inset_map <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(w_edge1, e_edge1), ylim = c(s_edge1, n_edge1)) +
  geom_point(data=dfs1,aes(x=s_lon_1, y=s_lat_1), size=1, alpha=0.5)+
  theme_void()+
  theme(panel.border=element_rect(color = "black", linewidth = 1, fill=NA))
    #theme(panel.background=element_blank(),panel.ontop = TRUE, = ggplot2::element_rect(color = "black", linewidth = 3))


# Add the inset map to the main plot
vp <- viewport(width = 0.37, height = 0.34, x = 0.48, y = 0.3)


#Save as PNG file
setwd(here("New-figures"))
pdf(paste(Sys.Date(), "broughton_inset_map.pdf", sep="_"), width=7, height=11)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
print(main_plot)
print(inset_map, vp = vp)
dev.off() 






