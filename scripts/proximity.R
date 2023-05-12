#Proximity ratio

library(readr)
library(ctmm)

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant anteater")

# Convert dataset to a telemetry object
DATA_TELEMETRY <- as.telemetry("data/Anteaters_NoOutliers.csv")
#correct a mismatch entry for 'Larry 267' and 'Larry'
names(DATA_TELEMETRY)[25] <- "Larry"

#load RDS files
#movement models
FIT_1 <- readRDS("RDS/movement_model/FIT_1.RDS")
FIT_2 <- readRDS("RDS/movement_model/FIT_2.RDS")
DATA_overlap_pairwise <- readRDS("RDS/overlap/overlap_pairwise.RDS")
DATA_overlap_pairwise_1 <- readRDS("RDS/overlap/overlap_pairwise_1.RDS")
DATA_overlap_pairwise_2 <- readRDS("RDS/overlap/overlap_pairwise_2.RDS")

#create empty columns for results to be saved to
DATA_overlap_pairwise_1$proximity_low <- NA
DATA_overlap_pairwise_1$proximity_est <- NA
DATA_overlap_pairwise_1$proximity_high <- NA

#Calculate the proximity statistics
for(i in 1:nrow(overlap_1)){
  ANIMAL_A <- as.character(DATA_overlap_pairwise_1[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(DATA_overlap_pairwise_1[i, 'anteater_B'])
  TRACKING_DATA.1 <- DATA_TELEMETRY[c(ANIMAL_A, ANIMAL_B)] # extract anteater by name, has extra layers .-. it doesnt work, that is why
  # line above is using as.character removes all the fluff because you just want the text string
  MODELS.1 <- list(FIT_1[ANIMAL_A][[1]], FIT_1[ANIMAL_B][[1]])
  PROXIMITY1 <- tryCatch(
    {
      PROXIMITY_1 <- proximity(data = TRACKING_DATA.1, CTMM = MODELS.1, GUESS=ctmm(error=FALSE))},
    error=function(err){
      PROXIMITY_1 <- c(NA,NA,NA)
      return(PROXIMITY_1)
    }
  )
  DATA_overlap_pairwise_1[i, c("proximity_low")] <- PROXIMITY_1[1]
  DATA_overlap_pairwise_1[i, c("proximity_est")] <- PROXIMITY_1[2]
  DATA_overlap_pairwise_1[i, c("proximity_high")] <- PROXIMITY_1[3]
  write.csv(DATA_overlap_pairwise_1, "data/DATA_proximity_1.csv", row.names = FALSE)
  Sys.sleep(0.5) #this staggers each output by 0.5 seconds, a way to see the loop happening in real time
}

#create empty columns for results to be saved to
DATA_overlap_pairwise_2$proximity_low <- NA
DATA_overlap_pairwise_2$proximity_est <- NA
DATA_overlap_pairwise_2$proximity_high <- NA

#Calculate the proximity statistics
for(i in 1:nrow(DATA_overlap_pairwise_2)){
  ANIMAL_A <- as.character(DATA_overlap_pairwise_2[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(DATA_overlap_pairwise_2[i, 'anteater_B'])
  TRACKING_DATA_2 <- DATA_TELEMETRY[c(ANIMAL_A, ANIMAL_B)] # extract anteater by name, has extra layers .-. it doesnt work, that is why
  # line above is using as.character removes all the fluff because you just want the text string
  MODELS_2 <- list(FIT_2[ANIMAL_A][[1]], FIT_2[ANIMAL_B][[1]])
  PROXIMITY_2 <- tryCatch(
    {
      #calculate the proximity statistic
      PROXIMITY_2 <- proximity(data = TRACKING_DATA_2, CTMM = MODELS_2, GUESS=ctmm(error=FALSE))},
    error=function(err){
      PROXIMITY_2 <- c(NA,NA,NA)
      return(PROXIMITY_2)
    }
  )
  DATA_overlap_pairwise_2[i, c("proximity_low")] <- PROXIMITY_2[1]
  DATA_overlap_pairwise_2[i, c("proximity_est")] <- PROXIMITY_2[2]
  DATA_overlap_pairwise_2[i, c("proximity_high")] <- PROXIMITY_2[3]
  write.csv(DATA_overlap_pairwise_2, "data/DATA_proximity_2.csv", row.names = FALSE)
  Sys.sleep(0.5) #this staggers each output by 0.5 seconds, a way to see the loop happening in real time
}

DATA_proximity_1 <- read_csv("data/DATA_proximity_1.csv")
DATA_proximity_2 <- read_csv("data/DATA_proximity_2.csv")
DATA_proximity <- bind_rows(DATA_proximity_1, DATA_proximity_2)

saveRDS(DATA_proximity_1, file = "RDS/proximity/DATA_proximity_1.RDS")
saveRDS(DATA_proximity_2, file = "RDS/proximity/DATA_proximity_2.RDS")
saveRDS(DATA_proximity, file = "RDS/proximity/DATA_proximity.RDS")