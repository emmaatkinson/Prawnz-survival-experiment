# generating study map figure for prawn experiment paper

#install.packages("rnaturalearthdata")
#install.packages("rnaturalearth")
#install.packages("ggspatial")
#install.packages("remotes")
#remotes::install_github("MichaelMalick/r-chroma")
#install.packages("ggrepel")
#remotes::install_github("seananderson/ggsidekick")

rm(list=ls())

library(here)
library(sf)
library(tidyverse)
library(PBSmapping)
library(rnaturalearth)
library(scales)
library(ggspatial)
library(chroma)
library(ggrepel)
library(ggsidekick)

source(here("analysis-clean-EMA","coordinate_conversion_function.R"))

#set up colours for plotting
cols=rev(chroma::qpal(7, luminance=40))
cols.dk=rev(chroma::qpal(7, luminance=20))

# read in collated site data  
site = read.csv(here("data-clean","2024-04-19-EMA_prawn_combined_trial_data.csv"))
site = site[,c("trial_number","exp_haul_lon_1", "exp_haul_lat_1")]
site$exp_haul_lon_1 = paste("-",site$exp_haul_lon_1,sep="")
  
# read in sub-area shape-file
subdat = st_read(here("data-raw","shapefiles","DFO_BC_PFMA_SUBAREAS_CHS_V3_G.shp"))
# read in coastline shape-file
coastdat = st_read(here("data-raw","shapefiles","250_CST_LN_line.shp"))

# Subset area 11 and 12, some overlap on map
d11_12 <- filter(subdat, MGNT_AREA==11 | MGNT_AREA==12 | MGNT_AREA==13)

#Create sf points from lat and lon values
sitepoints <- site %>%  
  st_as_sf(coords= c("exp_haul_lon_1", "exp_haul_lat_1"), crs=4326) %>% 
  st_transform(st_crs(d11_12))

lons = c("-127", "-126", "-126", "-127")
lats = c(50.6, 50.6, 51, 51)
testxy = data.frame(lons,lats)

test = testxy %>% 
       st_as_sf(coords=c("lons", "lats"), crs=4326) %>% 
       st_transform(st_crs(d11_12))

test2

# project bc polygon to the crs of the subarea data
coast <- coastdat %>% st_transform(st_crs(d11_12))

# set bounding box
bb2 <- st_bbox(test)

#make map
map <- coast %>% 
       ggplot() +
       theme_bw() +
       theme(panel.border=element_rect(colour=cols[1], fill=NA, linewidth=1),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text = element_blank(),
             axis.ticks = element_blank()) +
       geom_sf(colour="white") +
       geom_sf(data=d11_12, fill="lightblue", col="lightblue") +
       geom_sf(data=sitepoints, shape=21, col=cols[1], fill=cols.dk[1], size=.5, stroke=1.25, alpha=0.6) +
       annotation_scale() +
       coord_sf(xlim = c(bb2[["xmin"]], bb2[["xmax"]]), 
                ylim = c(bb2[["ymin"]], bb2[["ymax"]])) 
map

#########################################################################################
cl <- rnaturalearth::ne_states(country = c("United States of America", "Canada"))
na_map <- sf::st_as_sf(cl)

test2 = testxy %>% 
  st_as_sf(coords=c("lons", "lats"), crs=4326) %>% 
  st_transform(st_crs(na_map))

bb = st_bbox(test2)

axes <- list( xlims=c(-140, -121), 
              ylims=c(47, 58),
              xbreaks=seq(-140,-120,10), 
              xlabels=as.character(seq(-140,-120,10)),
              seq(45, 60, 5), 
              ybreaks=seq(50, 60, 5),
              ylabels=as.character(seq(50,60,5)))


big_map <- ggplot(na_map) +
           geom_sf(data=na_map, colour="lightblue", fill="white", linewidth=.5) +
           labs(x="Longitude (°E)", y="Latitude (°N)") +
           geom_rect(
             xmin=bb[["xmin"]],
             ymin=bb[["ymin"]],
             xmax=bb[["xmax"]],
             ymax=bb[["ymax"]],
             fill=NA,
             colour=cols[1],
             size=.5
           ) +
          geom_text(data=na_map[na_map$name=="British Columbia",], aes(longitude, latitude, label=name),size=3) +
          annotation_scale(location="br") +
          annotation_north_arrow(location="tr") +
          coord_sf(xlim=axes$xlims, ylim=axes$ylims) +
          theme_sleek() 

big_map


library(cowplot)

setwd(here("figures-EMA"))
jpeg("prawnz_experiment_map.jpg", width=5, height=5, units="in",res=600, pointsize=10)

ggdraw(big_map) +
  draw_plot(
    {
      map +
        theme(legend.position="none")
    },
    x=0.15,
    y=0.08,
    width=0.4,
    height=0.4)

dev.off()
  
