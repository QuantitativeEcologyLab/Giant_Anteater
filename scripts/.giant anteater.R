

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
## Home range size ----
#............................................................

# Initialize an empty data frame to store the results
HR_size <- data.frame()

# Loop through each object in the AKDE list
for (i in 1:length(AKDE)) {
  # Access the AKDE object and extract the summary
  summary <- summary(AKDE[[i]])$CI
  
  # Bind the summary to the HR_size data frame
  HR_size <- rbind(HR_size, as.data.frame(summary))
}

row.names(HR_size) <- NULL
HR_size <- cbind(HR_size, DATA_META[,c(1:3,5)])
HR_size$site <- NA
HR_size$site[HR_size$Road == "MS-040"] <- 1
HR_size$site[HR_size$Road == "BR_267"] <- 2
HR_size$Road <- NULL
HR_size <- relocate(HR_size, c(low, est, high), .after = site)
names(HR_size)[5] <- "HR_low"
names(HR_size)[6] <- "HR_est"
names(HR_size)[7] <- "HR_high"

#............................................................
## Home range size results ----
#............................................................

#calculate home-range size & compare sex
AKDE_male <- AKDE[c("Alexander", "Anthony", "Beto","Christoffer","Jackson",
                    "Kyle", "Larry", "Little_Rick", "Luigi", "Reid", 
                    "Rodolfo", "Thomas")]
AKDE_female <- AKDE[c("Annie", "Bumpus", "Cate", "Elaine", "Hannah",
                      "Jane","Makao", "Margaret", "Maria", "Puji",
                      "Sheron")]

#calculate mean home-range sizes for male
meta(AKDE_male)

#calculate mean home-range sizes for female
meta(AKDE_female)

#test to see significance of sex on home-range
AKDE_sex_compare <- list(male = AKDE_male,
                         female = AKDE_female)
COL_sex <- c("#004488", "#A50026")
meta(AKDE_sex_compare, col = COL_sex, sort = TRUE)


#............................................................
## Home range overlap results ----
#............................................................

overlap_df <- readRDS("RDS/overlap_df.RDS")

# Total home range overlap & range
#calculate mean total home range overlap 
round(mean(overlap_df$overlap_est), 2)
round(min(overlap_df$overlap_est), 2)
round(max(overlap_df$overlap_est), 2)

#............................................................
### Home-range overlap sex analysis ----
#............................................................

##due to the nature of a beta distribution, transformation/squeezing is required, based on the equation https://stats.stackexchange.com/questions/31300/dealing-with-0-1-values-in-a-beta-regression
min_val <- min(overlap_df$overlap_est)
max_val <- max(overlap_df$overlap_est)
squeeze_min <- 0.001
squeeze_max <- 0.999
overlap_df$overlap_est_squeezed <- ((overlap_df$overlap_est - min_val) / (max_val - min_val)) * (squeeze_max - squeeze_min) + squeeze_min
overlap_df <- relocate(overlap_df, overlap_est_squeezed, .after = overlap_high)

#test for significance in sex, compare model with and without sex as a variable
HRO_test <- glmmTMB(overlap_est_squeezed ~ sex_comparison + (1|site), family = beta_family(link = "logit"), data = overlap_df)
HRO_test2 <- glmmTMB(overlap_est_squeezed ~ 1 + (1|site), family = beta_family(link = "logit"), data = overlap_df)
HRO_test_results <- anova(HRO_test, HRO_test2)
HRO_test_pvalue <- round(HRO_test_results$`Pr(>Chisq)`[2], 2)

#number of home range overlap in each sex comparison category
table(overlap_df$sex_comparison)

# Home range overlap based on sex comparison categories
#calculate mean home range overlap & range based on sex comparison categories
round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)

round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)

round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "female-male"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "female-male"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "female-male"]), 2)

#............................................................
# Interactions ----
#............................................................

## Proximity ratio ----
proximity_df <- readRDS("RDS/proximity_df.RDS")
proximity_identified_pairs_df <- readRDS("RDS/proximity_identified_pairs_df.RDS")

#add proximity ratio data to home-range overlap dataframe
overlap_df <- left_join(overlap_df, proximity_df, by = c("anteater_A", "anteater_B",
                                                         "Sex.A", "Sex.B",
                                                         "Age.A", "Age.B",
                                                         "sex_comparison",
                                                         "site"))

#Identify pairs that did not have a proximity ratio of 1 based on sex comparison category
table(proximity_identified_pairs_df$sex_comparison)

### Proximity ratio sex analysis ----
#test for significance in sex, compare model with and without sex as a variable across all 121 dyads
proximity_test <- glmer(proximity_est ~ sex_comparison + (1|site), family = Gamma(link = "log"), data = overlap_df)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = overlap_df)
proximity_test_results <- anova(proximity_test, proximity_test2)
proximity_test_pvalue <- round(proximity_test_results$`Pr(>Chisq)`[2], 2)
#p = 0.13

#PROX AND OVERLAP
proximity_test <- glmer(proximity_est ~ overlap_est + (1|site), family = Gamma(link = "log"), data = overlap_df)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = overlap_df)
proximity_test_results <- anova(proximity_test, proximity_test2)
proximity_test_results

#............................................................
## Distances ----
#............................................................

distance_df <- readRDS("RDS/distance_df.RDS")

# #locate NA values within the dataframe
distance_df[!complete.cases(distance_df), ] #3,502,701 observations
#drop the 3 fixes that had no distance values 
distance_df <- na.omit(distance_df) #3,502,698 observations

#add supplementary info to distance data from the home range overlap dataframe
distance_df <- merge(distance_df, overlap_df, by = "pair_ID")
distance_df <- relocate(distance_df, c(distance_low, distance_est, distance_high,
                                       t, timestamp), .after = proximity_high)

#............................................................
## Encounters ----
#............................................................

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~NEED TO DO
### Sensitivity Analysis ----

#calculate the distance threshold to be used as an encounter event



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END

#calculate total encounters of all individuals based on sex comparison type
overlap_df$encounter_count <- NA
identified_pairs <- unique(overlap_df$pair_ID)
for (i in identified_pairs){
  subset_A <- distance_df[distance_df$pair_ID == i,]
  
  # Count the number of times "distance_est" is below 15
  encounter_count <- sum(subset_A$distance_est < 15)
  
  #save results
  overlap_df[overlap_df$pair_ID == i, "encounter_count"] <- encounter_count
  
}

#number of pairs that had 0 encounters
overlap_df[overlap_df$encounter_count == 0,] #78
#number of pairs that had at least 1 encounter
overlap_df[overlap_df$encounter_count != 0,] #43

#calculate the number of encounters based on threshold
sum(overlap_df$encounter_count)
sum(overlap_df$encounter_count[overlap_df$sex_comparison == "male-male"])
sum(overlap_df$encounter_count[overlap_df$sex_comparison == "female-female"])
sum(overlap_df$encounter_count[overlap_df$sex_comparison == "female-male"])

#............................................................
### Encounter sex analysis ----
#............................................................

#effect of sex and overlap on encounter rates (model that does not include 0 encounter counts)
encounter_test <- glmer(encounter_count ~ overlap_est + sex_comparison + (1|site), family = poisson(link = "log"), data = overlap_df, subset = encounter_count > 0)
encounter_test2 <- glmer(encounter_count ~ 1 + (1|site), family = poisson(link = "log"), data = overlap_df, subset = encounter_count > 0)
encounter_test_results <- anova(encounter_test, encounter_test2)
encounter_test_pvalue <- round(encounter_test_results$`Pr(>Chisq)`[2], 2)
encounter_test_pvalue
summary(encounter_test)

# amount of home-range overlap and the number of observed encounters (β = 4.86 ± 0.148, p = 0.00)
summary(encounter_test)

#............................................................
### Encounters of identified pairs ----
#............................................................

distance_pair_df <- readRDS("RDS/distance_pair_df.RDS")

#calculate the number of encounters based on a instantaneous Euclidean distance between the individuals with a threshold of 15meters using telemetry data (threshold obtained from the sensitivity analysis)
proximity_identified_pairs_df$encounter_count <- NA

for (i in 1:12) {
  # Calculate the encounter_count for the current pair
  encounter_count <- sum(distance_pair_df$pair_ID_number == i & distance_pair_df$est < 15)
  
  # Assign the calculated encounter_count to the corresponding rows in proximity_identified_pairs_df
  proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == i] <- encounter_count
}

sum(proximity_identified_pairs_df$encounter_count)
sum(proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$sex_comparison == "male-male"])
sum(proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$sex_comparison == "female-female"])
sum(proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$sex_comparison == "female-male"])

#............................................................
## Correlative movement of identified pairs----
#............................................................
cm_pair1 <- readRDS("RDS/cm_pair1.RDS")
cm_pair2 <- readRDS("RDS/cm_pair2.RDS")
cm_pair3 <- readRDS("RDS/cm_pair3.RDS")
cm_pair4 <- readRDS("RDS/cm_pair4.RDS")
cm_pair5 <- readRDS("RDS/cm_pair5.RDS")
cm_pair6 <- readRDS("RDS/cm_pair6.RDS")
cm_pair7 <- readRDS("RDS/cm_pair7.RDS")
cm_pair8 <- readRDS("RDS/cm_pair8.RDS")
cm_pair9 <- readRDS("RDS/cm_pair9.RDS")
cm_pair10 <- readRDS("RDS/cm_pair10.RDS")
cm_pair11 <- readRDS("RDS/cm_pair11.RDS")
cm_pair12 <- readRDS("RDS/cm_pair12.RDS")

#calculate mean amount of correlative movement for each identified pair
# initialize columns
proximity_identified_pairs_df$mean_etaTot.CI.Low <- NA
proximity_identified_pairs_df$mean_etaTot.MLE <- NA
proximity_identified_pairs_df$mean_etaTot.CI.Upp <- NA

# iterate over pair_ID_number values
for (i in 1:12) {
  # get the corresponding cm_pair
  cm_pair <- get(paste0("cm_pair", i))
  
  # calculate means
  mean_etaTot_CI_Low <- mean(cm_pair$etaTot.CI.Low)
  mean_etaTot_MLE <- mean(cm_pair$etaTot.MLE)
  mean_etaTot_CI_Upp <- mean(cm_pair$etaTot.CI.Upp)
  
  # update proximity_identified_pairs_df with mean values
  proximity_identified_pairs_df$mean_etaTot.CI.Low[proximity_identified_pairs_df$pair_ID_number == i] <- mean_etaTot_CI_Low
  proximity_identified_pairs_df$mean_etaTot.MLE[proximity_identified_pairs_df$pair_ID_number == i] <- mean_etaTot_MLE
  proximity_identified_pairs_df$mean_etaTot.CI.Upp[proximity_identified_pairs_df$pair_ID_number == i] <- mean_etaTot_CI_Upp
}

#mean amount of total correlation in all identified pairs movement
round(mean(proximity_identified_pairs_df$mean_etaTot.CI.Low[-1]), 2)
round(mean(proximity_identified_pairs_df$mean_etaTot.MLE[-1]), 2)
round(mean(proximity_identified_pairs_df$mean_etaTot.CI.Upp[-1]), 2)

#calculate mean total drift and mean total diffusion correlative movement for the 12 pairs
# initialize columns
proximity_identified_pairs_df$mean_etaDif.MLE <- NA
proximity_identified_pairs_df$mean_etaDft.MLE <- NA

# iterate over pair_ID_number values
for (i in 1:12) {
  # get the corresponding cm_pair
  cm_pair <- get(paste0("cm_pair", i))
  
  # calculate means
  mean_etaDif_MLE <- mean(cm_pair$etaDif.MLE)
  mean_etaDft_MLE <- mean(cm_pair$etaDft.MLE)
  
  # update proximity_identified_pairs_df with mean values
  proximity_identified_pairs_df$mean_etaDif.MLE[proximity_identified_pairs_df$pair_ID_number == i] <- mean_etaDif_MLE
  proximity_identified_pairs_df$mean_etaDft.MLE[proximity_identified_pairs_df$pair_ID_number == i] <- mean_etaDft_MLE
}






