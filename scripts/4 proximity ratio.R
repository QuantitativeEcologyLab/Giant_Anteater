
# Proximity ratio statistics

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
# Proximity ratio ----
#............................................................

FIT <- readRDS("RDS/FIT.RDS")
AKDE <- readRDS("RDS/AKDE.RDS")
overlap_df <- readRDS("RDS/overlap_df.RDS")

#subset individual movement models based on their site location
FIT_1 <- FIT[c("Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
               "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
               "Puji", "Rodolfo")]
FIT_2 <- FIT[c("Annie", "Beto", "Hannah", "Jane", "Larry",
               "Luigi", "Margaret", "Maria", "Reid", "Sheron",
               "Thomas")]

#create empty columns for results to be saved to
overlap_1_df$proximity_low <- NA
overlap_1_df$proximity_est <- NA
overlap_1_df$proximity_high <- NA
#Calculate the proximity statistics
for(i in 1:nrow(overlap_1)){
  ANIMAL_A <- as.character(overlap_1_df[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(overlap_1_df[i, 'anteater_B'])
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
  overlap_1_df[i, c("proximity_low")] <- PROXIMITY_1[1]
  overlap_1_df[i, c("proximity_est")] <- PROXIMITY_1[2]
  overlap_1_df[i, c("proximity_high")] <- PROXIMITY_1[3]
  write.csv(overlap_1_df, "data/DATA_proximity_1.csv", row.names = FALSE)
  cat("finished index", i, "\n") # see the loop happening in real time
}

#create empty columns for results to be saved to
overlap_2_df$proximity_low <- NA
overlap_2_df$proximity_est <- NA
overlap_2_df$proximity_high <- NA
#Calculate the proximity statistics
for(i in 1:nrow(overlap_2_df)){
  ANIMAL_A <- as.character(overlap_2_df[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(overlap_2_df[i, 'anteater_B'])
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
  overlap_2_df[i, c("proximity_low")] <- PROXIMITY_2[1]
  overlap_2_df[i, c("proximity_est")] <- PROXIMITY_2[2]
  overlap_2_df[i, c("proximity_high")] <- PROXIMITY_2[3]
  write.csv(overlap_2_df, "data/DATA_proximity_2.csv", row.names = FALSE)
  cat("finished index", i, "\n") # see the loop happening in real time
}

#import proximity data
proximity_1_df <- read_csv("data/DATA_proximity_1.csv")
proximity_2_df <- read_csv("data/DATA_proximity_2.csv")

#create proximity dataframe
proximity_1_df$site <- 1
proximity_2_df$site <- 2
proximity_2_df <- relocate(proximity_2_df, site, .before = proximity_low)
proximity_1_df$anteater_A[proximity_1_df$anteater_A == "Little Rick"] <- "Little_Rick"
proximity_1_df$anteater_B[proximity_1_df$anteater_B == "Little Rick"] <- "Little_Rick"
#correct a mismatch entry for 'Larry 267' to 'Larry'
proximity_2_df$anteater_A[proximity_2_df$anteater_A == "Larry 267"] <- "Larry"
proximity_2_df$anteater_B[proximity_2_df$anteater_B == "Larry 267"] <- "Larry"

proximity_df <- bind_rows(proximity_1_df, proximity_2_df)
proximity_df <- proximity_df[,-3]
proximity_df <- mutate(proximity_df,
                       sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                  paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                  paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                  paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))

saveRDS(proximity_df, file = "RDS/proximity_df.RDS")

#............................................................
# Proximity ratio identified pairs ----
#............................................................

#add proximity ratio data to home-range overlap dataframe
overlap_df <- left_join(overlap_df, proximity_df, by = c("anteater_A", "anteater_B",
                                                         "Sex.A", "Sex.B",
                                                         "Age.A", "Age.B",
                                                         "sex_comparison",
                                                         "site"))

#identify pairs that did not have a proximity ratio of 1
proximity_above1 <- overlap_df[overlap_df$proximity_low > 1,]
proximity_below1 <- overlap_df[overlap_df$proximity_high < 1,]
#exclude pairs with a HR overlap of 0
proximity_below1[proximity_below1$overlap_est == 0,]
proximity_below1 <- proximity_below1[proximity_below1$overlap_est != 0,]

# dataframe of identified proximity pairs
proximity_identified_pairs_df <- rbind(proximity_above1, proximity_below1)
proximity_identified_pairs_df$pair_ID_number <- seq(from = 1, to = 12, by = 1)
proximity_identified_pairs_df <- relocate(proximity_identified_pairs_df, pair_ID_number, .before = anteater_A)

#correct the sex_comparison output to female-male
proximity_identified_pairs_df <- mutate(proximity_identified_pairs_df,
                                        sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                                   paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                                   paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                                   paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))

saveRDS(proximity_identified_pairs_df, file = "RDS/proximity_identified_pairs_df.RDS")

