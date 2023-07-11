
# Distances

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
# Distance ----
#............................................................

FIT <- readRDS("RDS/FIT.RDS")
AKDE <- readRDS("RDS/AKDE.RDS")
overlap_df <- readRDS("RDS/overlap_df.RDS")
proximity_df <- readRDS("RDS/proximity_df.RDS")
proximity_identified_pairs_df <- readRDS("RDS/proximity_identified_pairs_df.RDS")
#add proximity ratio data to home-range overlap dataframe
overlap_df <- left_join(overlap_df, proximity_df, by = c("anteater_A", "anteater_B",
                                                         "Sex.A", "Sex.B",
                                                         "Age.A", "Age.B",
                                                         "sex_comparison",
                                                         "site"))


#Calculate the distance statistics
overlap_df$distance_low <- NA
overlap_df$distance_est <- NA
overlap_df$distance_high <- NA

RES <- list()

for (i in 1:nrow(overlap_df)) {
  ANIMAL_A <- as.character(overlap_df[i, 'anteater_A']) 
  ANIMAL_B <- as.character(overlap_df[i, 'anteater_B'])
  TRACKING_DATA <- DATA_TELEMETRY[c(ANIMAL_A, ANIMAL_B)]
  MODELS <- list(FIT[[ANIMAL_A]], FIT[[ANIMAL_B]])
  
  DISTANCES_RES <- tryCatch({
    distances_result <- distances(data = TRACKING_DATA, CTMM = MODELS, GUESS = ctmm(error = FALSE))
    data.frame(pair_ID = paste(ANIMAL_A, ANIMAL_B, sep = "_"),
               distance_low = distances_result$low, 
               distance_est = distances_result$est, 
               distance_high = distances_result$high,
               t = distances_result$t,
               timestamp = distances_result$timestamp)
  }, error = function(err) {
    data.frame(pair_ID = paste(ANIMAL_A, ANIMAL_B, sep = "_"),
               distance_low = NA,
               distance_est = NA,
               distance_high = NA,
               t = NA, 
               timestamp = NA)
  })
  
  RES[[i]] <- DISTANCES_RES
  
  #write.csv(RES, "data/DATA_distance.csv", row.names = FALSE)
  cat("finished index", i, "\n")
}

#Turn the list of list into a data frame
distance_df <- do.call(rbind, RES)
saveRDS(distance_df, file = "RDS/distance_df.RDS")


#............................................................
# Proximity ratio identified pairs distances ----
#............................................................

#Calculate the instantaneous Euclidean distance between each proximity ratio identified pairs
pair1 <- DATA_TELEMETRY[c("Kyle","Christoffer")]
FIT_pair1 <- FIT[c("Kyle","Christoffer")]
distance_pair1 <- distances(pair1, FIT_pair1) # distance measurement and time between the pair
distance_pair1$pair_ID_number <- 1
distance_pair1$anteater_A <- "Kyle"
distance_pair1$anteater_B <- "Christoffer"
distance_pair1$pair_ID <- paste(distance_pair1$anteater_A, distance_pair1$anteater_B, sep = "_")
distance_pair1 <- relocate(distance_pair1, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair2 <- DATA_TELEMETRY[c("Elaine","Christoffer")]
FIT_pair2 <- FIT[c("Elaine","Christoffer")]
distance_pair2 <- distances(pair2, FIT_pair2) # distance measurement and time between the pair
distance_pair2$pair_ID_number <- 2
distance_pair2$anteater_A <- "Elaine"
distance_pair2$anteater_B <- "Christoffer"
distance_pair2$pair_ID <- paste(distance_pair2$anteater_A, distance_pair2$anteater_B, sep = "_")
distance_pair2 <- relocate(distance_pair2, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair3 <- DATA_TELEMETRY[c("Kyle","Bumpus")]
FIT_pair3 <- FIT[c("Kyle","Bumpus")]
distance_pair3 <- distances(pair3, FIT_pair3) # distance measurement and time between the pair
distance_pair3$pair_ID_number <- 3
distance_pair3$anteater_A <- "Kyle"
distance_pair3$anteater_B <- "Bumpus"
distance_pair3$pair_ID <- paste(distance_pair3$anteater_A, distance_pair3$anteater_B, sep = "_")
distance_pair3 <- relocate(distance_pair3, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair4 <- DATA_TELEMETRY[c("Little_Rick","Elaine")]
FIT_pair4 <- FIT[c("Little_Rick","Elaine")]
distance_pair4 <- distances(pair4, FIT_pair4) # distance measurement and time between the pair
distance_pair4$pair_ID_number <- 4
distance_pair4$anteater_A <- "Little_Rick"
distance_pair4$anteater_B <- "Elaine"
distance_pair4$pair_ID <- paste(distance_pair4$anteater_A, distance_pair4$anteater_B, sep = "_")
distance_pair4 <- relocate(distance_pair4, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair5 <- DATA_TELEMETRY[c("Makao","Bumpus")]
FIT_pair5 <- FIT[c("Makao","Bumpus")]
distance_pair5 <- distances(pair5, FIT_pair5) # distance measurement and time between the pair
distance_pair5$pair_ID_number <- 5
distance_pair5$anteater_A <- "Makao"
distance_pair5$anteater_B <- "Bumpus"
distance_pair5$pair_ID <- paste(distance_pair5$anteater_A, distance_pair5$anteater_B, sep = "_")
distance_pair5 <- relocate(distance_pair5, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair6 <- DATA_TELEMETRY[c("Puji","Bumpus")]
FIT_pair6 <- FIT[c("Puji","Bumpus")]
distance_pair6 <- distances(pair5, FIT_pair5) # distance measurement and time between the pair
distance_pair6$pair_ID_number <- 6
distance_pair6$anteater_A <- "Puji"
distance_pair6$anteater_B <- "Bumpus"
distance_pair6$pair_ID <- paste(distance_pair6$anteater_A, distance_pair6$anteater_B, sep = "_")
distance_pair6 <- relocate(distance_pair6, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair7 <- DATA_TELEMETRY[c("Rodolfo", "Elaine")]
FIT_pair7 <- FIT[c("Rodolfo", "Elaine")]
distance_pair7 <- distances(pair7, FIT_pair7) # distance measurement and time between the pair
distance_pair7$pair_ID_number <- 7
distance_pair7$anteater_A <- "Rodolfo"
distance_pair7$anteater_B <- "Elaine"
distance_pair7$pair_ID <- paste(distance_pair7$anteater_A, distance_pair7$anteater_B, sep = "_")
distance_pair7 <- relocate(distance_pair7, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair8 <- DATA_TELEMETRY[c("Larry","Annie")]
FIT_pair8 <- FIT[c("Larry","Annie")]
distance_pair8 <- distances(pair8, FIT_pair8) # distance measurement and time between the pair
distance_pair8$pair_ID_number <- 8
distance_pair8$anteater_A <- "Larry"
distance_pair8$anteater_B <- "Annie"
distance_pair8$pair_ID <- paste(distance_pair8$anteater_A, distance_pair8$anteater_B, sep = "_")
distance_pair8 <- relocate(distance_pair8, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair9 <- DATA_TELEMETRY[c("Reid","Larry")]
FIT_pair9 <- FIT[c("Reid","Larry")]
distance_pair9 <- distances(pair9, FIT_pair9) # distance measurement and time between the pair
distance_pair9$pair_ID_number <- 9
distance_pair9$anteater_A <- "Reid"
distance_pair9$anteater_B <- "Larry"
distance_pair9$pair_ID <- paste(distance_pair9$anteater_A, distance_pair9$anteater_B, sep = "_")
distance_pair9 <- relocate(distance_pair9, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair10 <- DATA_TELEMETRY[c("Sheron","Maria")]
FIT_pair10 <- FIT[c("Sheron","Maria")]
distance_pair10 <- distances(pair10, FIT_pair10) # distance measurement and time between the pair
distance_pair10$pair_ID_number <- 10
distance_pair10$anteater_A <- "Sheron"
distance_pair10$anteater_B <- "Maria"
distance_pair10$pair_ID <- paste(distance_pair10$anteater_A, distance_pair10$anteater_B, sep = "_")
distance_pair10 <- relocate(distance_pair10, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair11 <- DATA_TELEMETRY[c("Thomas","Margaret")]
FIT_pair11 <- FIT[c("Thomas","Margaret")]
distance_pair11 <- distances(pair11, FIT_pair11) # distance measurement and time between the pair
distance_pair11$pair_ID_number <- 11
distance_pair11$anteater_A <- "Thomas"
distance_pair11$anteater_B <- "Margaret"
distance_pair11$pair_ID <- paste(distance_pair11$anteater_A, distance_pair11$anteater_B, sep = "_")
distance_pair11 <- relocate(distance_pair11, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

pair12 <- DATA_TELEMETRY[c("Thomas","Reid")]
FIT_pair12 <- FIT[c("Thomas","Reid")]
distance_pair12 <- distances(pair12, FIT_pair12) # distance measurement and time between the pair
distance_pair12$pair_ID_number <- 12
distance_pair12$anteater_A <- "Thomas"
distance_pair12$anteater_B <- "Reid"
distance_pair12$pair_ID <- paste(distance_pair12$anteater_A, distance_pair12$anteater_B, sep = "_")
distance_pair12 <- relocate(distance_pair12, c(pair_ID_number, pair_ID, anteater_A, anteater_B), .before = low)

saveRDS(object = distance_pair1, file = "RDS/distance_pair1.RDS")
saveRDS(object = distance_pair2, file = "RDS/distance_pair2.RDS")
saveRDS(object = distance_pair3, file = "RDS/distance_pair3.RDS")
saveRDS(object = distance_pair4, file = "RDS/distance_pair4.RDS")
saveRDS(object = distance_pair5, file = "RDS/distance_pair5.RDS")
saveRDS(object = distance_pair6, file = "RDS/distance_pair6.RDS")
saveRDS(object = distance_pair7, file = "RDS/distance_pair7.RDS")
saveRDS(object = distance_pair8, file = "RDS/distance_pair8.RDS")
saveRDS(object = distance_pair9, file = "RDS/distance_pair9.RDS")
saveRDS(object = distance_pair10, file = "RDS/distance_pair10.RDS")
saveRDS(object = distance_pair11, file = "RDS/distance_pair11.RDS")
saveRDS(object = distance_pair12, file = "RDS/distance_pair12.RDS")

#combine into a dataframe
distance_pair_df <- rbind(distance_pair1, distance_pair2)
distance_pair_df <- rbind(distance_pair_df, distance_pair3)
distance_pair_df <- rbind(distance_pair_df, distance_pair4)
distance_pair_df <- rbind(distance_pair_df, distance_pair5)
distance_pair_df <- rbind(distance_pair_df, distance_pair6)
distance_pair_df <- rbind(distance_pair_df, distance_pair7)
distance_pair_df <- rbind(distance_pair_df, distance_pair8)
distance_pair_df <- rbind(distance_pair_df, distance_pair9)
distance_pair_df <- rbind(distance_pair_df, distance_pair10)
distance_pair_df <- rbind(distance_pair_df, distance_pair11)
distance_pair_df <- rbind(distance_pair_df, distance_pair12)

saveRDS(object = distance_pair_df, file = "RDS/distance_pair_df.RDS")
