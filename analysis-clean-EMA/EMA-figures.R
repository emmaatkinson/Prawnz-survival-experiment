#Read in packages ----
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

#set working directory
setwd(here("data-clean"))

#Read in data----
reflexes<-read.csv("2023-05-09_prawn_combined_reflex_data.csv")
survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

#Make new column to store temperature 
survival$temp<-rep(0, nrow(survival))

#Read in model
model_6.1_1<-readRDS("mymodel.rds")
