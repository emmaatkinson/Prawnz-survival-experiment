## Example script set-up ##

# Date created: 3-May-2023
# Last updated: 3-May-2023
# Author: Emma Atkinson
# Description: Example script set-up
# Notes: 

# --- Prepping environment --- #

rm(list=ls())
graphics.off()

# Installing packages #
# install.packages("tidyverse")

# Loading packages #
library(tidyverse)
library(here) # this package allows you to reproducibly set your directory

# See what here() does (if you open the R Project, it should already be set to
# the correct root folder)
here()

# Use here() to set working directory 
setwd(here("data-raw"))

# Check working directory
getwd()