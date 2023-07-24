
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
ggsave(plot = last_plot(), filename = "figures/BR_meso_region.png", device = NULL,
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
ggsave(plot = last_plot(), filename = "figures/MS_state_micro.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

plot_BR_MS <- grid.arrange(plot_BR, plot_MS, 
                           ncol=2, widths = c(6,5.2))
# Save the figure
ggsave(plot_BR_MS,
       width = 10, height = 5, units = "in",
       dpi = 600,
       bg = "white",
       file="figures/plot_BR_MS.png")

