# Figure 1 Map

#............................................................
# Load packages ----
#............................................................

#data, visualization
library(readr)
library(ggplot2)
library(khroma)          #colour blind friendly colour palette
library(dplyr)           #data wrangling
library(tidyr)           #data wrangling
library(tibble)          #data wrangling
library(lubridate)       #round_date() for corrMove
library(geobr)           #shape files for Brazil
library(gridExtra)       #arrangement of plots for multi-panel figures
library(scales)          #scaling axis in plots
library(sf)
#analysis
library(devtools)
#devtools::install_github("ctmm-initiative/ctmm", force = TRUE) #if package needs to be updated
#devtools::install_github("jmcalabrese/corrMove", force = TRUE) #if installing for the first time
library(ctmm)            #continuous-time movement models
library(lme4)            #pairwise sex test to see if differences are significant using glmer()
library(glmmTMB)         #beta distribution
library(mgcv)            #gam() for encounters
library(corrMove)        #correlative movement

#............................................................
# Data ----
#............................................................

# Set working directory
setwd("C:/Users/achhen/Documents/GitHub/Giant_Anteater")

#import data, cleaned GPS giant anteater data
DATA_GPS <- readRDS("RDS/DATA_GPS.RDS")
DATA_TELEMETRY <- readRDS("RDS/DATA_TELEMETRY.RDS")
DATA_META <- readRDS("RDS/DATA_META.RDS")
DATA_BIO <- readRDS("RDS/DATA_BIO.RDS")


#............................................................
# Map ----
#............................................................

#Find the range of the GPS coordinates
ctmm::extent(DATA_TELEMETRY)
#min/max
#latitude (EW, y values) -53.79925/-53.47427
#longitude (NS, x values) -21.77205/-21.08363

# Convert GPS data into an sf object
#Assuming your data is in WGS 84 coordinate reference system (EPSG:4326)
gps_sf <- st_as_sf(DATA_GPS, coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326)

#............................................................
# Raster data
#............................................................

#steps
# spatial points -> ext -> crop
# EXT <- ext(spatial points object from gps locations)
# crop(native_forest_sp, EXT)

library(terra)
library(tidyterra)

#import raster data
pasture <- terra::rast("map/pasture.TIF")
pasture_ext <- ext(pasture) 
#-54.27486, -52.99557, -22.0169, -20.36328  (xmin, xmax, ymin, ymax)
pasture_crop <- crop(pasture, pasture_ext)

native_forest <- terra::rast("map/native_forest.TIF")
native_forest_ext <- ext(native_forest)
native_forest_crop <- crop(native_forest, native_forest_ext)

planted_forest <- terra::rast("map/planted_forest.TIF")
planted_forest_ext <- ext(planted_forest)
planted_forest_crop <- crop(planted_forest, planted_forest_ext)

#downloaded from Mapbiomas, https://storage.googleapis.com/mapbiomas-public/brasil/collection-71/lclu/coverage/brasil_coverage_2017.tif
# brazil_coverage <- terra::rast("map/brasil_coverage_2017.TIF")
# brazil_coverage_ext <- ext(brazil_coverage)
# brazil_coverage_crop <- crop(brazil_coverage, brazil_coverage_ext)

#test plot for all sites
ggplot() +
  geom_spatraster(data = native_forest, aes(fill = native_forest)) +
  geom_spatraster(data = planted_forest, aes(fill = planted_forest), alpha = 0.5) +
  geom_sf(data = gps_sf, aes(color = Sex),
          size = 1) 

#............................................................
# Site specific #
#............................................................
#add site number to GPS dataset
DATA_GPS$site <- NA
DATA_GPS$site[DATA_GPS$ID %in% c("Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
                                 "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
                                 "Puji", "Rodolfo")] <- 1
DATA_GPS$site[DATA_GPS$ID %in% c("Annie", "Beto", "Hannah", "Jane", "Larry",
                                 "Luigi", "Margaret", "Maria", "Reid", "Sheron",
                                 "Thomas")] <- 2

#subset site 1 GPS data and create a new dataframe 
GPS_site1 <- DATA_GPS[DATA_GPS$site == 1,]
GPS_site1 <- left_join(GPS_site1, DATA_BIO, by = "ID")
# Convert dataframe into an sf object
gps_sf1 <- st_as_sf(GPS_site1, coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326)

#obtain range (extent) of spatial object
ext(gps_sf1)

# TO DO: SET COLOUR PALETTE FOR EACH LAYER
col_natforest <- "darkgreen"
col_plantforest <- "purple"
col_pasture <- "yellow"

#https://rstudio.github.io/leaflet/raster.html
#Raster images
#import raster data
native_forest <- terra::rast("map/native_forest.TIF")
#range of raster for site 1 size
native_forest_ext <- ext(gps_sf1)
native_forest_crop <- crop(native_forest, native_forest_ext)

planted_forest <- terra::rast("map/planted_forest.TIF")
#range of raster for site 1 size
planted_forest_ext <- ext(gps_sf1)
planted_forest_crop <- crop(planted_forest, planted_forest_ext)

pasture <- terra::rast("map/pasture.TIF")
#range of raster for site 1 size
pasture_ext <- ext(gps_sf1)
pasture_crop <- crop(pasture, pasture_ext)

#site 1
ggplot() +
  geom_spatraster(data = native_forest_crop, aes(fill = "green"), alpha = 0.5, maxcell = 5000) +
  geom_spatraster(data = planted_forest_crop, aes(fill = "pink"), alpha = 0.5, maxcell = 5000) +
  geom_spatraster(data=pasture_crop, aes(fill="yellow"), alpha=0.5, maxcell = 5000) +
  geom_sf(data = gps_sf1, aes(color = Sex),
          size = 1) +
  scale_color_manual(values = c('#004488', '#A50026'), breaks = c('Male', 'Female')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_sf(xlim = c(-53.66, -53.8), #lat
           ylim = c(-21.2, -21.08))  #long

