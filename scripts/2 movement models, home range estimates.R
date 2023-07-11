
# Movement models

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
# Home range movement models ----
#............................................................

# Fit movement models
GUESS <- lapply(DATA_TELEMETRY[1:23], function(b) ctmm.guess(b,interactive=FALSE) )
FIT <- lapply(1:23, function(i) ctmm.select(DATA_TELEMETRY[[i]],GUESS[[i]]) )
names(FIT) <- names(DATA_TELEMETRY[1:23])
overlap(FIT)
#calculate AKDE overlap, create aligned UDs
AKDE <- akde(DATA_TELEMETRY[1:23],FIT)
overlap(AKDE)

#save movement models
saveRDS(FIT, file = "RDS/FIT.RDS")
#save AKDE overlap
saveRDS(AKDE, file = "RDS/AKDE.RDS")



