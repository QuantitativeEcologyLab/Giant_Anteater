


# Case study of Pair 11
#Margaret and Thomas


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
# Data ----
#............................................................

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant anteater")

#import data, cleaned GPS giant anteater data
DATA_GPS <- read_csv("data/Anteaters_NoOutliers.csv")

#correct a mismatch entry for 'Larry' to 'Larry 267'
DATA_GPS$ID[DATA_GPS$ID == "Larry 267"] <- "Larry"
DATA_GPS$ID[DATA_GPS$ID == "Little Rick"] <- "Little_Rick"
#subset to 23 range-resident individuals
DATA_GPS <- DATA_GPS[which(DATA_GPS$ID %in% c("Alexander", "Annie", "Anthony", "Beto", "Bumpus",
                                              "Cate", "Christoffer","Elaine", "Hannah","Jackson",
                                              "Jane","Kyle", "Larry", "Little_Rick", "Luigi",
                                              "Makao", "Margaret", "Maria", "Puji", "Reid", 
                                              "Rodolfo", "Sheron", "Thomas")),]

# Convert dataset to a telemetry object
DATA_TELEMETRY <- as.telemetry("data/Anteaters_NoOutliers.csv")

#correct a mismatch entry for 'Larry' to 'Larry 267'
names(DATA_TELEMETRY)[25] <- "Larry"
names(DATA_TELEMETRY)[27] <- "Little_Rick"

#subset to 23 range-resident individuals
DATA_TELEMETRY  <- DATA_TELEMETRY[c("Alexander", "Annie", "Anthony", "Beto", "Bumpus",
                                    "Cate", "Christoffer","Elaine", "Hannah","Jackson",
                                    "Jane","Kyle", "Larry", "Little_Rick", "Luigi",
                                    "Makao", "Margaret", "Maria", "Puji", "Reid", 
                                    "Rodolfo", "Sheron", "Thomas")]

# Import supplementary data containing biological information
DATA_META <- read_csv("data/Anteater_Results_Final.csv")
#subset to 23 range-resident individuals
DATA_META <- DATA_META[c(1:3,8:10,12,14,17,19,20,22,23,25:29,33:35,37,38),]
DATA_META$ID[DATA_META$ID == "Little Rick"] <- "Little_Rick"
DATA_BIO <- DATA_META[,1:3]

#............................................................
# Home range ----
#............................................................

FIT <- readRDS("RDS/FIT.RDS")
AKDE <- readRDS("RDS/AKDE.RDS")

#............................................................
## Home range size results ----
#............................................................

#home-range size
AKDE_thomas <- AKDE["Thomas"]
AKDE_margaret <- AKDE["Margaret"]

#calculate mean home-range sizes for Thomas
meta(AKDE_thomas)

#calculate mean home-range sizes for Margaret
meta(AKDE_margaret)

#............................................................
## Home range overlap  ----
#............................................................

round(proximity_identified_pairs_df[11,]$overlap_low, 2)
round(proximity_identified_pairs_df[11,]$overlap_est, 2)
round(proximity_identified_pairs_df[11,]$overlap_high, 2)

#............................................................
## Proximity Ratio  ----
#............................................................

#proximity ratio
round(proximity_identified_pairs_df$proximity_low[proximity_identified_pairs_df$pair_ID_number == 11], 2)
round(proximity_identified_pairs_df$proximity_est[proximity_identified_pairs_df$pair_ID_number == 11], 2)
round(proximity_identified_pairs_df$proximity_high[proximity_identified_pairs_df$pair_ID_number == 11], 2)

#............................................................
## Distance ----
#............................................................

#distances are in meters, convert to km
round(mean(distance_pair11$est)/1000, 2)
round(min(distance_pair11$est)/1000, 2)
round(max(distance_pair11$est)/1000, 2)

#............................................................
## Correlative movement ----
#............................................................

#mean 95% CI correlative movement
round(mean(cm_pair11$etaTot.CI.Low), 2)
round(mean(cm_pair11$etaTot.MLE), 2)
round(mean(cm_pair11$etaTot.CI.Upp), 2)
