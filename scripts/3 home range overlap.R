
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
# Home range overlap dataframe ----
#............................................................

#load movement models and AKDE home range estimates
FIT <- readRDS("RDS/FIT.RDS")
AKDE <- readRDS("RDS/AKDE.RDS")

#subset home range overlap based on site location
AKDE_1 <- AKDE[c("Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
                 "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
                 "Puji", "Rodolfo")]
AKDE_2 <- AKDE[c("Annie", "Beto", "Hannah", "Jane", "Larry",
                 "Luigi", "Margaret", "Maria", "Reid", "Sheron",
                 "Thomas")]

#Create a pairwise data frame pairing up every individual for site 1
#calculate 95% AKDE overlap for pairwise comparison
overlap_1 <- overlap(AKDE_1, level = 0.95)

#extract CI 'est' matrix from array
overlap_1_est <- overlap_1$CI[ , , 2]
#remove duplicate values of the matrix
overlap_1_est[upper.tri(overlap_1_est, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_1_df <- as.data.frame(overlap_1_est)
overlap_1_df$anteater_A <- rownames(overlap_1_df)
overlap_1_df <- pivot_longer(overlap_1_df, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_est', values_drop_na = TRUE)

#extract CI 'low' matrix from array
overlap_1_low <- overlap_1$CI[ , , 1]
#remove duplicate values of the matrix
overlap_1_low[upper.tri(overlap_1_low, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_1_low <- as.data.frame(overlap_1_low)
overlap_1_low$anteater_A <- rownames(overlap_1_low)
overlap_1_low <- pivot_longer(overlap_1_low, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_low', values_drop_na = TRUE)
overlap_1_df <- left_join(overlap_1_df, overlap_1_low, by = c("anteater_A", "anteater_B"))
overlap_1_df <- relocate(overlap_1_df, overlap_low, .before = overlap_est)

#extract CI 'high' matrix from array
overlap_1_high <- overlap_1$CI[ , , 3]
#remove duplicate values of the matrix
overlap_1_high[upper.tri(overlap_1_high, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_1_high <- as.data.frame(overlap_1_high)
overlap_1_high$anteater_A <- rownames(overlap_1_high)
overlap_1_high <- pivot_longer(overlap_1_high, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_high', values_drop_na = TRUE)
overlap_1_df <- left_join(overlap_1_df, overlap_1_high, by = c("anteater_A", "anteater_B"))

#add biological data to dataframe
overlap_1_df <- left_join(overlap_1_df, rename(DATA_BIO, anteater_A = ID), by = "anteater_A")
overlap_1_df <- left_join(overlap_1_df, rename(DATA_BIO, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
#add column to indicate which sexes that are being compared
overlap_1_df <- mutate(overlap_1_df,
                       sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                  paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                  paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                  paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))
#assign site
overlap_1_df$site <- 1
overlap_1_df <- relocate(overlap_1_df, c("overlap_low", "overlap_est", "overlap_high"), .after = site)

#Create a pairwise data frame pairing up every individual for site  ...................
#calculate 95% AKDE overlap for pairwise comparison
overlap_2 <- overlap(AKDE_2, level = 0.95)

#extract CI 'est' matrix from array
overlap_2_est <- overlap_2$CI[ , , 2]
#remove duplicate values of the matrix
overlap_2_est[upper.tri(overlap_2_est, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_2_df <- as.data.frame(overlap_2_est)
overlap_2_df$anteater_A <- rownames(overlap_2_df)
overlap_2_df <- pivot_longer(overlap_2_df, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_est', values_drop_na = TRUE)

#extract CI 'low' matrix from array
overlap_2_low <- overlap_2$CI[ , , 1]
#remove duplicate values of the matrix
overlap_2_low[upper.tri(overlap_2_low, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_2_low <- as.data.frame(overlap_2_low)
overlap_2_low$anteater_A <- rownames(overlap_2_low)
overlap_2_low <- pivot_longer(overlap_2_low, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_low', values_drop_na = TRUE)
overlap_2_df <- left_join(overlap_2_df, overlap_2_low, by = c("anteater_A", "anteater_B"))
overlap_2_df <- relocate(overlap_2_df, overlap_low, .before = overlap_est)

#extract CI 'high' matrix from array
overlap_2_high <- overlap_2$CI[ , , 3]
#remove duplicate values of the matrix
overlap_2_high[upper.tri(overlap_2_high, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
overlap_2_high <- as.data.frame(overlap_2_high)
overlap_2_high$anteater_A <- rownames(overlap_2_high)
overlap_2_high <- pivot_longer(overlap_2_high, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap_high', values_drop_na = TRUE)
overlap_2_df <- left_join(overlap_2_df, overlap_2_high, by = c("anteater_A", "anteater_B"))

#add biological data to dataframe
overlap_2_df <- left_join(overlap_2_df, rename(DATA_BIO, anteater_A = ID), by = "anteater_A")
overlap_2_df <- left_join(overlap_2_df, rename(DATA_BIO, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
#add column to indicate which sexes that are being compared
overlap_2_df <- mutate(overlap_2_df,
                       sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                  paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                  paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                  paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))
#assign site
overlap_2_df$site <- 2
overlap_2_df <- relocate(overlap_2_df, c("overlap_low", "overlap_est", "overlap_high"), .after = site)
## HRO pairwise results
overlap_df <- rbind(overlap_1_df, overlap_2_df)

overlap_df$pair_ID <- paste(overlap_df$anteater_A, overlap_df$anteater_B, sep = "_")
overlap_df <- relocate(overlap_df, pair_ID, .before = anteater_A)

saveRDS(object = overlap_df, file = "RDS/overlap_df.RDS")