
# map of brazil, study site

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
#analysis
library(devtools)
#devtools::install_github("ctmm-initiative/ctmm", force = TRUE) #if package needs to be updated
#devtools::install_github("jmcalabrese/corrMove", force = TRUE) #if installing for the first time
library(ctmm)            #continuous-time movement models
library(lme4)            #pairwise sex test to see if differences are significant using glmer()
library(glmmTMB)         #beta distribution
library(corrMove)        #correlative movement

# Set working directory
setwd("C:/Users/achhen/Documents/GitHub/Giant_Anteater")

#............................................................
# Map of Brazil ----
#............................................................

#Brazil with meso regions outlined
BR_meso_region <- read_meso_region(code_meso = "all", year = 2017, simplified = TRUE, showProgress = TRUE)

#state of Mato Grosso du Sul
MS_state_micro <- read_micro_region(code_micro= "MS", year = 2017, simplified = FALSE, showProgress = TRUE)

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

plot_BR <-
  ggplot() +
  geom_sf(data=BR_meso_region, fill="white", color="black", size=.15, show.legend = FALSE) +
  labs(subtitle="Brazil", size=8) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") +
  no_axis
plot_BR

ggsave(plot = last_plot(), filename = "figures/map/BR_meso_region.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

plot_MS <-
  ggplot() +
  geom_sf(data=MS_state_micro, fill="white", color="black", size=.15, show.legend = FALSE) +
  labs(subtitle="Mato Grosso du Sul", size=8) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") +
  no_axis
plot_MS

ggsave(plot = last_plot(), filename = "figures/map/MS_state_micro.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

plot_BR_MS <- grid.arrange(plot_BR, plot_MS, 
                           ncol=2, widths = c(6,5.2))
# Save the figure
ggsave(plot_BR_MS,
       width = 10, height = 5, units = "in",
       dpi = 600,
       bg = "white",
       file="figures/map/plot_BR_MS.png")


#MAPBIOMAS
library(sf)
map_states <- read_sf("data/map/dashboard_states-static-layer.shp")

plot(map_states)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(readr)
library(sf)         # For handling spatial data (Simple Features)
library(geobr)           #shape files for Brazil
library(ggplot2)    # For creating plots


# Set working directory
setwd("C:/Users/achhen/Documents/GitHub/Giant_Anteater")

#import data, cleaned GPS giant anteater data
DATA_GPS <- readRDS("RDS/DATA_GPS.RDS")
# DATA_TELEMETRY <- readRDS("RDS/DATA_TELEMETRY.RDS")
# DATA_META <- readRDS("RDS/DATA_META.RDS")
# DATA_BIO <- readRDS("RDS/DATA_BIO.RDS")

#~~~~~~~~~~~~~~~~~~~~~~~~~
#MAPBIOMAS
library(sf)
map_states <- read_sf("data/mapbiomas/dashboard_states-static-layer/dashboard_states-static-layer.shp")

mato <- map_states[map_states$name == "Mato Grosso do Sul",]
#~~~~~~~~~~~~~~~~~~~~~~~~~

# Assuming your data is in WGS 84 coordinate reference system (EPSG:4326)
gps_sf <- st_as_sf(DATA_GPS, coords = c("GPS.Longitude", "GPS.Latitude"), crs = 4326)

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# South America level
# Load a basemap, for example, a world map using the 'rnaturalearth' package
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter out South America by continent code (continent = "South America")
south_america <- world[world$continent == "South America", ]

# Filter out Brazil by country name (name = "Brazil")
brazil <- world[world$name == "Brazil", ]

#Brazil with meso regions outlined
library(geobr)
BR_meso_region <- read_meso_region(code_meso = "all", year = 2017, simplified = TRUE, showProgress = TRUE)

ggplot() +
  geom_sf(data = south_america, fill = "darkgrey") +
  geom_sf(data = BR_meso_region, fill="white", color="black", size=.15, show.legend = FALSE) +
  geom_sf(data = gps_sf, color = "red", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# country level
#state of Mato Grosso du Sul
MS_state <- read_meso_region(code_meso= "MS", year = 2017, simplified = FALSE, showProgress = TRUE)
#state of Mato Grosso du Sul
MS_state_micro <- read_micro_region(code_micro= "MS", year = 2017, simplified = FALSE, showProgress = TRUE)


ggplot() +
  geom_sf(data = BR_meso_region, fill = "darkgrey") +
  geom_sf(data = MS_state_micro, fill="white", color="black", size=.15, show.legend = FALSE) +
  #geom_sf(data = gps_sf, color = "red", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# state level

#region of Mato Grosso du Sul
MS_region <- read_region(year = 2017, simplified = FALSE, showProgress = TRUE)

ctmm::extent(DATA_TELEMETRY)

library(raster)
brazil_coverage <- raster("data/map/brasil_coverage_2017.TIF")
pasture <- raster("data/map/pasture.TIF")
native_forest <- raster("data/map/native_forest.TIF")
planted_forest <- raster("data/map/planted_forest.TIF")

DATA_GPS$site <- NA

DATA_GPS$site[DATA_GPS$ID %in% c("Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
                                 "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
                                 "Puji", "Rodolfo")] <- 1

DATA_GPS$site[DATA_GPS$ID %in% c("Annie", "Beto", "Hannah", "Jane", "Larry",
                                 "Luigi", "Margaret", "Maria", "Reid", "Sheron",
                                 "Thomas")] <- 2

#site 1
ggplot() +
  # geom_sf(data = MS_state_micro, fill = "darkgrey") +
  # geom_sf(data = MS_region, fill="white", color="black", size=.15, show.legend = FALSE) +
  geom_sf(data = gps_sf, color = "red", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_sf(xlim = c(-53.6, -53.8),
           ylim = c(-21, -21.25))

#site 2
ggplot() +
  geom_sf(data = gps_sf, color = "red", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_sf(xlim = c(-53.4, -53.7),
           ylim = c(-21.65, -21.85))


