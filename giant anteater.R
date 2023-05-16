
#load packages
#data, visualization
library(readr)
library(ggplot2)
library(khroma)          #colour blind friendly colour palette
library(dplyr)           #data wrangling
library(tidyr)           #data wrangling
library(tibble)          #data wrangling
library(lubridate)       #round_date() for corrMove
#analysis
#devtools::install_github("ctmm-initiative/ctmm", force = TRUE) if package needs to be updated
library(ctmm)            #continuous-time movement models
library(lme4)            #pairwise sex test to see if differences are significant using glmer()
#devtools::install_github("jmcalabrese/corrMove", force = TRUE)
library(corrMove)        #correlative movement

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant anteater")

#Import cleaned giant anteater data
DATA_GPS <- read_csv("data/Anteaters_NoOutliers.csv")


# Convert dataset to a telemetry object
DATA <- as.telemetry("data/Anteaters_NoOutliers.csv")

# Import supplementary data containing biological information
DATA_META <- read_csv("data/Anteater_Results_Final.csv")
#subset to specific individuals
DATA_META <- DATA_META[c(1:3,8:10,12,14,17,19,20,22,23,25:29,33:35,37,38),]
DATA_BIO <- DATA_META[,c(1:3)]
#correct a mismatch entry for 'Larry' to 'Larry 267'
DATA_BIO$ID[DATA_BIO$ID == "Larry"] <- "Larry 267"

#data subsetting and carpentry
SITE_1 <- DATA[c(1,3,9,10,12,15,21,24,27,29,36,38)]
SITE_1_male <- DATA[c(1,3,12,21,24,27,38)]
SITE_1_female <- DATA[c(9,10,15,29,36)]
SITE_2 <- DATA[c(2,8,19,22,25,30,31,37,41,42)]
SITE_2_male <- DATA[c(8,25,37,42,28)]
SITE_2_female <- DATA[c(2,22,30,19,31,41)]

#................. to do
# Home Range
#calculate home range size

# Home range: compare sex
#does sex determine the home range size?

#.................. end of home range size

# Home range overlap ----
#refer to 'movement model and AKDE overlap' script
#movement models
FIT_1 <- readRDS("RDS/movement_model/FIT_1.RDS")
FIT_1_male <- readRDS("RDS/movement_model/FIT_1_male.RDS")
FIT_1_female <- readRDS("RDS/movement_model/FIT_1_female.RDS")
FIT_2 <- readRDS("RDS/movement_model/FIT_2.RDS")
FIT_2_male <- readRDS("RDS/movement_model/FIT_2_male.RDS")
FIT_2_female <- readRDS("RDS/movement_model/FIT_2_female.RDS")

#calculate AKDE overlap, create aligned UDs
AKDE_1 <- readRDS("RDS/overlap/AKDE_1.RDS")
AKDE_1_male <- readRDS("RDS/overlap/AKDE_1_male.RDS")
AKDE_1_female <- readRDS("RDS/overlap/AKDE_1_female.RDS")
AKDE_2 <- readRDS("RDS/overlap/AKDE_2.RDS")
AKDE_2_male <- readRDS("RDS/overlap/AKDE_2_male.RDS")
AKDE_2_female <- readRDS("RDS/overlap/AKDE_2_female.RDS")

#fit movement models
GUESS_1 <- lapply(SITE_1[1:12], function(b) ctmm.guess(b,interactive=FALSE) )
FIT_1 <- lapply(1:12, function(i) ctmm.select(SITE_1[[i]],GUESS_1[[i]]) )
names(FIT_1) <- names(SITE_1[1:12])
overlap(FIT_1)
#calculate AKDE overlap, create aligned UDs
AKDE_1 <- akde(SITE_1[1:12],FIT_1)
overlap(AKDE_1)

#fit movement models
GUESS_1_male <- lapply(SITE_1_male[1:7], function(b) ctmm.guess(b,interactive=FALSE) )
FIT_1_male <- lapply(1:7, function(i) ctmm.select(SITE_1_male[[i]],GUESS_1_male[[i]]) )
names(FIT_1_male) <- names(SITE_1_male[1:7])
overlap(FIT_1_male)
#calculate AKDE overlap, create aligned UDs
AKDE_1_male <- akde(SITE_1[1:7],FIT_1_male)
overlap(AKDE_1_male)

#fit movement models
GUESS_1_female <- lapply(SITE_1_female[1:5], function(b) ctmm.guess(b,interactive=FALSE) )
FIT_1_female <- lapply(1:5, function(i) ctmm.select(SITE_1_female[[i]],GUESS_1_female[[i]]) )
names(FIT_1_female) <- names(SITE_1_female[1:5])
overlap(FIT_1_female)
#calculate AKDE overlap, create aligned UDs
AKDE_1_female <- akde(SITE_1_female[1:5],FIT_1_female)
overlap(AKDE_1_female)

#fit movement models
GUESS_2 <- lapply(SITE_2[1:11], function(b) ctmm.guess(b,interactive=FALSE) )
FIT_2 <- lapply(1:11, function(i) ctmm.select(SITE_2[[i]],GUESS_2[[i]]) )
names(FIT_2) <- names(SITE_2[1:11])
overlap(FIT_2)
#calculate AKDE overlap, create aligned UDs
AKDE_2 <- akde(SITE_2[1:11],FIT_2)
overlap(AKDE_2)

#fit movement models
GUESS_2_male <- lapply(SITE_2_male[1:5], function(b) ctmm.guess(b,interactive=FALSE) )
FIT_2_male <- lapply(1:5, function(i) ctmm.select(SITE_2_male[[i]],GUESS_2_male[[i]]) )
names(FIT_2_male) <- names(SITE_2_male[1:5])
overlap(FIT_2_male)
#calculate AKDE overlap, create aligned UDs
AKDE_2_male <- akde(SITE_2[1:5],FIT_2_male)
overlap(AKDE_2_male)

#fit movement models
GUESS_2_female <- lapply(SITE_2_female[1:6], function(b) ctmm.guess(b,interactive=FALSE) )
FIT_2_female <- lapply(1:6, function(i) ctmm.select(SITE_2_female[[i]],GUESS_2_female[[i]]) )
names(FIT_2_female) <- names(SITE_2_female[1:6])
overlap(FIT_2_female)
#calculate AKDE overlap, create aligned UDs
AKDE_2_female <- akde(SITE_2_female[1:6],FIT_2_female)
overlap(AKDE_2_female)

#save movement models
saveRDS(FIT_1, file = "RDS/movement_model/FIT_1.RDS")
saveRDS(FIT_1_male, file = "RDS/movement_model/FIT_1_male.RDS")
saveRDS(FIT_1_female, file = "RDS/movement_model/FIT_1_female.RDS")
saveRDS(FIT_2, file = "RDS/movement_model/FIT_2.RDS")
saveRDS(FIT_2_male, file = "RDS/movement_model/FIT_2_male.RDS")
saveRDS(FIT_2_female, file = "RDS/movement_model/FIT_2_female.RDS")
#save AKDE overlaps
saveRDS(AKDE_1, file = "RDS/overlap/AKDE_1.RDS")
saveRDS(AKDE_1_male, file = "RDS/overlap/AKDE_1_male.RDS")
saveRDS(AKDE_1_female, file = "RDS/overlap/AKDE_1_female.RDS")
saveRDS(AKDE_2, file = "RDS/overlap/AKDE_2.RDS")
saveRDS(AKDE_2_male, file = "RDS/overlap/AKDE_2_male.RDS")
saveRDS(AKDE_2_female, file = "RDS/overlap/AKDE_2_female.RDS")

#plot home range overlap (coloured based on sex; red = female, blue = male)
COL_1 <- c("#004488", "#004488", "#A50026", "#A50026", "#004488", "#A50026", "#004488", "#004488", "#004488", "#A50026", "#A50026", "#004488") 
COL_2 <- c("#A50026", "#004488", "#A50026", "#A50026", "#004488", "#004488", "#A50026", "#A50026", "#004488", "#A50026", "#004488") 
par(mfrow = c(1,2))
plot(AKDE_1, col.DF = COL_1, col.level = COL_1, col.grid = NA, level = NA)
title("A)", adj = 0)
title("Site 1")
plot(AKDE_2, col.DF = COL_2, col.level = COL_2, col.grid = NA, level = NA) 
title("B)", adj = 0)
title("Site 2")

## Compare sex (pairwise) ----
#refer to 'AKDE overlap pairwise' script
#AKDE overlap pairwise (at CI 95%)
DATA_overlap_pairwise <- readRDS("RDS/overlap/DATA_overlap_pairwise.RDS")
DATA_overlap_pairwise_1 <- readRDS("RDS/overlap/DATA_overlap_pairwise_1.RDS")
DATA_overlap_pairwise_2 <- readRDS("RDS/overlap/DATA_overlap_pairwise_2.RDS")

#calculate 95% AKDE overlap for pairwise comparison
overlap_pairwise_1 <- overlap(AKDE_1, level = 0.95)
#extract CI 'est' matrix from array
overlap_pairwise_1 <- overlap_pairwise_1$CI[ , , 2]
#remove duplicate values of the matrix
overlap_pairwise_1[upper.tri(overlap_pairwise_1, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
DATA_overlap_pairwise_1 <- as.data.frame(overlap_pairwise_1)
DATA_overlap_pairwise_1$anteater_A <- rownames(DATA_overlap_pairwise_1) 
DATA_overlap_pairwise_1 <- pivot_longer(DATA_overlap_pairwise_1, cols = -anteater_A, names_to = 'anteater_B', values_to = 'Overlap', values_drop_na = TRUE)
#add biological data to dataframe
DATA_overlap_pairwise_1 <- left_join(DATA_overlap_pairwise_1, rename(DATA_BIO, anteater_A = ID), by = "anteater_A")
DATA_overlap_pairwise_1 <- left_join(DATA_overlap_pairwise_1, rename(DATA_BIO, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
#add column to indicate which sexes that are being compared
DATA_overlap_pairwise_1 <- mutate(DATA_overlap_pairwise_1,
                                  sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                             paste(Sex.A, Sex.B) == "Female Female" ~ "female-female", 
                                                             paste(Sex.A, Sex.B) == "Male Female" ~ "male-female",
                                                             paste(Sex.A, Sex.B) == "Female Male" ~ "male-female"))
DATA_overlap_pairwise_1$site <- "1"

#calculate 95% overlap 
#overlap() applied to aligned UD objects + movement models = overlap of their (autocorrelated) kernel density estimates
overlap_pairwise_2 <- overlap(AKDE_2, level = 0.95)
#extract CI 'est' matrix from array
overlap_pairwise_2 <- overlap_pairwise_2$CI[ , , 2]
#remove duplicate values of the matrix
overlap_pairwise_2[upper.tri(overlap_pairwise_2, diag = TRUE)] <- NA
#Create a new data frame based on the overlap values
DATA_overlap_pairwise_2 <- as.data.frame(overlap_pairwise_2)
DATA_overlap_pairwise_2$anteater_A <- rownames(DATA_overlap_pairwise_2) 
DATA_overlap_pairwise_2 <- pivot_longer(DATA_overlap_pairwise_2, cols = -anteater_A, names_to = 'anteater_B', values_to = 'Overlap', values_drop_na = TRUE)
#add biological data to dataframe
DATA_overlap_pairwise_2 <- left_join(DATA_overlap_pairwise_2, rename(DATA_BIO, anteater_A = ID), by = "anteater_A")

DATA_overlap_pairwise_2 <- left_join(DATA_overlap_pairwise_2, rename(DATA_BIO, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
#add column to indicate which sexes that are being compared
DATA_overlap_pairwise_2 <- mutate(DATA_overlap_pairwise_2,
                                  sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                             paste(Sex.A, Sex.B) == "Female Female" ~ "female-female", 
                                                             paste(Sex.A, Sex.B) == "Male Female" ~ "male-female",
                                                             paste(Sex.A, Sex.B) == "Female Male" ~ "male-female"))
DATA_overlap_pairwise_2$site <- "2"
DATA_overlap_pairwise <- rbind(DATA_overlap_pairwise_1, DATA_overlap_pairwise_2)

saveRDS(object = DATA_overlap_pairwise_1, file = "RDS/overlap/DATA_overlap_pairwise_1.RDS")
saveRDS(object = DATA_overlap_pairwise_2, file = "RDS/overlap/DATA_overlap_pairwise_2.RDS")
saveRDS(object = DATA_overlap_pairwise, file = "RDS/overlap/DATA_overlap_pairwise.RDS")

## plot home range overlap sex comparision  ----
ggplot(data = DATA_overlap_pairwise, mapping = aes(x = sex_comparison, y = Overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Home range overlap") +
  xlab("Sex") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position="none") +
  scale_fill_manual(values = c("#A50026", "#9970AB", "#004488"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))

## Sex comparison analysis ----
#determine if sex is a significant factor in the occurrence home range overlap?

#test for significance in sex, compare model with and without sex as a variable
test <- glmer(Overlap ~ sex_comparison + (1|site), family = binomial(link = "logit"), data = DATA_overlap_pairwise)
test2 <- glmer(Overlap ~ 1 + (1|site), family = binomial, data = DATA_overlap_pairwise)
test_results <- anova(test, test2)
test_pvalue <- round(test_results$`Pr(>Chisq)`[2], 2)

HRO_sex_counts <- table(DATA_overlap_pairwise$sex_comparison)

## calculate median HR overlap value ----
MM_median_HRO <- round(median(DATA_overlap_pairwise$Overlap[DATA_overlap_pairwise$sex_comparison == "male-male"]), 2)
FF_median_HRO <- round(median(DATA_overlap_pairwise$Overlap[DATA_overlap_pairwise$sex_comparison == "female-female"]), 2)
MF_median_HRO <- round(median(DATA_overlap_pairwise$Overlap[DATA_overlap_pairwise$sex_comparison == "male-female"]), 2)

# Proximity ----
#Calculate the proximity statistics
DATA_proximity_1 <- readRDS("RDS/proximity/DATA_proximity_1.RDS")
DATA_proximity_2 <- readRDS("RDS/proximity/DATA_proximity_2.RDS")
DATA_proximity <- readRDS("RDS/proximity/DATA_proximity.RDS")

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

## plot proximity ratio ----
ggplot(data = DATA_proximity, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 2) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), linewidth = 0.5) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home range overlap") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.85, 0.15),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

## Proximity Ratio ----
#identify the pairs with ratio above/below 1
DATA_proximity$ratio <- NA
DATA_proximity$ratio[DATA_proximity$proximity_low > 1] <- "above 1"
DATA_proximity$ratio[DATA_proximity$proximity_high < 1] <- "below 1"

proximity_above1 <- DATA_proximity[which(DATA_proximity$proximity_low > 1),]
table(proximity_above1$sex_comparison)
proximity_below1 <- DATA_proximity[which(DATA_proximity$proximity_high < 1),]
#exclude dyads with a HR overlap of 0
proximity_below1[proximity_below1$overlap == 0,]
proximity_below1 <- proximity_below1[proximity_below1$overlap != 0,]
table(proximity_below1$sex_comparison)
unique(c(proximity_below1$anteater_A, proximity_below1$anteater_B))

proximity_ratio <- rbind(proximity_above1, proximity_below1)
proximity_ratio$pair_ID <- seq(from = 1, to = 12, by = 1)






## Sex analysis ----
#determine if sex is a significant factor in the occurrence Proximity Ratio?

#test for significance in sex, compare model with and without sex as a variable
proximity_test <- glmer(proximity_est ~ sex_comparison + (1|site), family = Gamma(link = "inverse"), data = DATA_proximity)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "inverse"), data = DATA_proximity)
proximity_test_results <- anova(proximity_test, proximity_test2)
proximity_test_pvalue <- round(proximity_test_results$`Pr(>Chisq)`[2], 2)

# Distance measurement ----
#distance
distance_pair1 <- readRDS("RDS/distance/distance_pair1.RDS")
distance_pair2 <- readRDS("RDS/distance/distance_pair2.RDS")
distance_pair3 <- readRDS("RDS/distance/distance_pair3.RDS")
distance_pair4 <- readRDS("RDS/distance/distance_pair4.RDS")
distance_pair5 <- readRDS("RDS/distance/distance_pair5.RDS")
distance_pair6 <- readRDS("RDS/distance/distance_pair6.RDS")
distance_pair7 <- readRDS("RDS/distance/distance_pair7.RDS")
distance_pair8 <- readRDS("RDS/distance/distance_pair8.RDS")
distance_pair9 <- readRDS("RDS/distance/distance_pair9.RDS")
distance_pair10 <- readRDS("RDS/distance/distance_pair10.RDS")
distance_pair11 <- readRDS("RDS/distance/distance_pair11.RDS")
distance_pair12 <- readRDS("RDS/distance/distance_pair12.RDS")

#Calculated the instantaneous Euclidean distance between the individuals in the dyad using telemetry data
# Christoffer/Kyle
pair1 <- DATA[c("Christoffer","Kyle")]
FIT_pair1 <- FIT_1[c("Christoffer","Kyle")]
distance_pair1 <- distances(pair1, FIT_pair1) # distance measurement and time between the pair
distance_pair1$pair_ID <- "1"

# Christoffer/Elaine
pair2 <- DATA[c("Christoffer","Elaine")]
FIT_pair2 <- FIT_1[c("Christoffer","Elaine")]
distance_pair2 <- distances(pair2, FIT_pair2) # distance measurement and time between the pair
distance_pair2$pair_ID <- "2"

# Bumpus/Kyle
pair3 <- DATA[c("Bumpus","Kyle")]
FIT_pair3 <- FIT_1[c("Bumpus","Kyle")]
distance_pair3 <- distances(pair3, FIT_pair3) # distance measurement and time between the pair
distance_pair3$pair_ID <- "3"

# Elaine/Little Rick
pair4 <- DATA[c("Elaine","Little Rick")]
FIT_pair4 <- FIT_1[c("Elaine","Little Rick")]
distance_pair4 <- distances(pair4, FIT_pair4) # distance measurement and time between the pair
distance_pair4$pair_ID <- "4"

# Bumpus/Makao
pair5 <- DATA[c("Bumpus","Makao")]
FIT_pair5 <- FIT_1[c("Bumpus","Makao")]
distance_pair5 <- distances(pair5, FIT_pair5) # distance measurement and time between the pair
distance_pair5$pair_ID <- "5"

# Bumpus/Puji
pair6 <- DATA[c("Bumpus","Puji")]
FIT_pair6 <- FIT_1[c("Bumpus","Puji")]
distance_pair6 <- distances(pair5, FIT_pair5) # distance measurement and time between the pair
distance_pair6$pair_ID <- "6"

# Elaine/Rodolfo
pair7 <- DATA[c("Elaine","Rodolfo")]
FIT_pair7 <- FIT_1[c("Elaine","Rodolfo")]
distance_pair7 <- distances(pair7, FIT_pair7) # distance measurement and time between the pair
distance_pair7$pair_ID <- "7"

# Annie/Larry
pair8 <- DATA[c("Annie","Larry 267")]
FIT_pair8 <- FIT_2[c("Annie","Larry 267")]
distance_pair8 <- distances(pair8, FIT_pair8) # distance measurement and time between the pair
distance_pair8$pair_ID <- "8"

# Larry/Reid
pair9 <- DATA[c("Larry 267","Reid")]
FIT_pair9 <- FIT_2[c("Larry 267","Reid")]
distance_pair9 <- distances(pair9, FIT_pair9) # distance measurement and time between the pair
distance_pair9$pair_ID <- "9"

# Margaret/Thomas
pair10 <- DATA[c("Margaret","Thomas")]
FIT_pair10 <- FIT_2[c("Margaret","Thomas")]
distance_pair10 <- distances(pair10, FIT_pair10) # distance measurement and time between the pair
distance_pair10$pair_ID <- "10"

# Reid/Thomas
pair11 <- DATA[c("Reid","Thomas")]
FIT_pair11 <- FIT_2[c("Reid","Thomas")]
distance_pair11 <- distances(pair11, FIT_pair11) # distance measurement and time between the pair
distance_pair11$pair_ID <- "11"

# Maria/Sheron
pair12 <- DATA[c("Maria","Sheron")]
FIT_pair12 <- FIT_2[c("Maria","Sheron")]
distance_pair12 <- distances(pair12, FIT_pair12) # distance measurement and time between the pair
distance_pair12$pair_ID <- "12"

DATA_DISTANCE <- rbind(distance_pair1, distance_pair2, distance_pair3, distance_pair4, distance_pair5,
                  distance_pair6, distance_pair7, distance_pair8, distance_pair9, distance_pair10,
                  distance_pair11, distance_pair12)

saveRDS(object = distance_pair1, file = "RDS/distance/distance_pair1.RDS")
saveRDS(object = distance_pair2, file = "RDS/distance/distance_pair2.RDS")
saveRDS(object = distance_pair3, file = "RDS/distance/distance_pair3.RDS")
saveRDS(object = distance_pair4, file = "RDS/distance/distance_pair4.RDS")
saveRDS(object = distance_pair5, file = "RDS/distance/distance_pair5.RDS")
saveRDS(object = distance_pair6, file = "RDS/distance/distance_pair6.RDS")
saveRDS(object = distance_pair7, file = "RDS/distance/distance_pair7.RDS")
saveRDS(object = distance_pair8, file = "RDS/distance/distance_pair8.RDS")
saveRDS(object = distance_pair9, file = "RDS/distance/distance_pair9.RDS")
saveRDS(object = distance_pair10, file = "RDS/distance/distance_pair10.RDS")
saveRDS(object = distance_pair11, file = "RDS/distance/distance_pair11.RDS")
saveRDS(object = distance_pair12, file = "RDS/distance/distance_pair12.RDS")
saveRDS(object = DATA_DISTANCE, file = "RDS/distance/DATA_DISTANCE.RDS")

#Encounters
#number of encounters based on a distance threshold of 15meters
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "1"] <- sum(distance_pair1$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "2"] <- sum(distance_pair2$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "3"] <- sum(distance_pair3$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "4"] <- sum(distance_pair4$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "5"] <- sum(distance_pair5$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "6"] <- sum(distance_pair6$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "7"] <- sum(distance_pair7$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "8"] <- sum(distance_pair8$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "9"] <- sum(distance_pair9$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "10"] <- sum(distance_pair10$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "11"] <- sum(distance_pair11$est < 15)
proximity_ratio$encounter_count[proximity_ratio$pair_ID == "12"] <- sum(distance_pair12$est < 15)

#plot
plot(x = overlap, y = encounter_count, data = proximity_ratio)

# Correlative Movement ----
#correlative movement
cmAnteater_pair1 <- readRDS("RDS/correlative movement/cmAnteater_pair1.RDS")
cmAnteater_pair2 <- readRDS("RDS/correlative movement/cmAnteater_pair2.RDS")
cmAnteater_pair3 <- readRDS("RDS/correlative movement/cmAnteater_pair3.RDS")
cmAnteater_pair4 <- readRDS("RDS/correlative movement/cmAnteater_pair4.RDS")
cmAnteater_pair5 <- readRDS("RDS/correlative movement/cmAnteater_pair5.RDS")
cmAnteater_pair6 <- readRDS("RDS/correlative movement/cmAnteater_pair6.RDS")
cmAnteater_pair7 <- readRDS("RDS/correlative movement/cmAnteater_pair7.RDS")
cmAnteater_pair8 <- readRDS("RDS/correlative movement/cmAnteater_pair8.RDS")
cmAnteater_pair9 <- readRDS("RDS/correlative movement/cmAnteater_pair9.RDS")
cmAnteater_pair10 <- readRDS("RDS/correlative movement/cmAnteater_pair10.RDS")
cmAnteater_pair11 <- readRDS("RDS/correlative movement/cmAnteater_pair11.RDS")
cmAnteater_pair12 <- readRDS("RDS/correlative movement/cmAnteater_pair12.RDS")

#data carpentry
Bumpus <- DATA$Bumpus
Christoffer <- DATA$Christoffer
Elaine <- DATA$Elaine
Kyle <- DATA$Kyle
Little_rick <- DATA$`Little Rick`
Makao <- DATA$Makao
Puji <- DATA$Puji
Rodolfo <- DATA$Rodolfo
Annie <- DATA$Annie
Larry <- DATA$`Larry 267`
Margaret <- DATA$Margaret
Reid <- DATA$Reid
Thomas <- DATA$Thomas
Maria <- DATA$Maria
Sheron <- DATA$Sheron

#create a dataframe of an individuals GPS coordinates
Bumpus_GPS <- data.frame(timestamp = round_date(Bumpus$timestamp, "20 minutes"),
                         Bumpus.x = Bumpus$longitude,
                         Bumpus.y = Bumpus$latitude)
Christoffer_GPS <- data.frame(timestamp = round_date(Christoffer$timestamp, "20 minutes"),
                              Christoffer.x = Christoffer$longitude,
                              Christoffer.y = Christoffer$latitude)
Elaine_GPS <- data.frame(timestamp = round_date(Elaine$timestamp, "20 minutes"),
                         Elaine.x = Elaine$longitude,
                         Elaine.y = Elaine$latitude)
Kyle_GPS <- data.frame(timestamp = round_date(Kyle$timestamp, "20 minutes"),
                       Kyle.x = Kyle$longitude,
                       Kyle.y = Kyle$latitude)
Little_rick_GPS <- data.frame(timestamp = round_date(Little_rick$timestamp, "20 minutes"),
                              Little_rick.x = Little_rick$longitude,
                              Little_rick.y = Little_rick$latitude)
Makao_GPS <- data.frame(timestamp = round_date(Makao$timestamp, "20 minutes"),
                        Makao.x = Makao$longitude,
                        Makao.y = Makao$latitude)
Puji_GPS <- data.frame(timestamp = round_date(Puji$timestamp, "20 minutes"),
                       Puji.x = Puji$longitude,
                       Puji.y = Puji$latitude)
Rodolfo_GPS <- data.frame(timestamp = round_date(Rodolfo$timestamp, "20 minutes"),
                          Rodolfo.x = Rodolfo$longitude,
                          Rodolfo.y = Rodolfo$latitude)
Annie_GPS <- data.frame(timestamp = round_date(Annie$timestamp, "20 minutes"),
                        Annie.x = Annie$longitude,
                        Annie.y = Annie$latitude)
Larry_GPS <- data.frame(timestamp = round_date(Larry$timestamp, "20 minutes"),
                        Larry.x = Larry$longitude,
                        Larry.y = Larry$latitude)
Margaret_GPS <- data.frame(timestamp = round_date(Margaret$timestamp, "20 minutes"),
                           Margaret.x = Margaret$longitude,
                           Margaret.y = Margaret$latitude)
Reid_GPS <- data.frame(timestamp = round_date(Reid$timestamp, "20 minutes"),
                       Reid.x = Reid$longitude,
                       Reid.y = Reid$latitude)
Thomas_GPS <- data.frame(timestamp = round_date(Thomas$timestamp, "20 minutes"),
                         Thomas.x = Thomas$longitude,
                         Thomas.y = Thomas$latitude)
Maria_GPS <- data.frame(timestamp = round_date(Maria$timestamp, "20 minutes"),
                        Maria.x = Maria$longitude,
                        Maria.y = Maria$latitude)
Sheron_GPS <- data.frame(timestamp = round_date(Sheron$timestamp, "20 minutes"),
                         Sheron.x = Sheron$longitude,
                         Sheron.y = Sheron$latitude)

#combine the GPS coordinates of pairs together
pair1_GPS <- merge(Christoffer_GPS, Kyle_GPS)
pair1_GPS <- pair1_GPS[, c(1,2,4,3,5)]
pair1_GPS <- pair1_GPS[!duplicated(pair1_GPS$timestamp),]

pair2_GPS <- merge(Christoffer_GPS, Elaine_GPS)
pair2_GPS <- pair2_GPS[, c(1,2,4,3,5)]
pair2_GPS <- pair2_GPS[!duplicated(pair2_GPS$timestamp),]

pair3_GPS <- merge(Bumpus_GPS, Kyle_GPS)
pair3_GPS <- pair3_GPS[, c(1,2,4,3,5)]
pair3_GPS <- pair3_GPS[!duplicated(pair3_GPS$timestamp),]

pair4_GPS <- merge(Elaine_GPS, Little_rick_GPS)
pair4_GPS <- pair4_GPS[, c(1,2,4,3,5)]
pair4_GPS <- pair4_GPS[!duplicated(pair4_GPS$timestamp),]

pair5_GPS <- merge(Bumpus_GPS, Makao_GPS)
pair5_GPS <- pair5_GPS[, c(1,2,4,3,5)]
pair5_GPS <- pair5_GPS[!duplicated(pair5_GPS$timestamp),]

pair6_GPS <- merge(Bumpus_GPS, Puji_GPS)
pair6_GPS <- pair6_GPS[, c(1,2,4,3,5)]
pair6_GPS <- pair6_GPS[!duplicated(pair6_GPS$timestamp),]

pair7_GPS <- merge(Elaine_GPS, Rodolfo_GPS)
pair7_GPS <- pair7_GPS[, c(1,2,4,3,5)]
pair7_GPS <- pair7_GPS[!duplicated(pair7_GPS$timestamp),]

pair8_GPS <- merge(Annie_GPS, Larry_GPS)
pair8_GPS <- pair8_GPS[, c(1,2,4,3,5)]
pair8_GPS <- pair8_GPS[!duplicated(pair8_GPS$timestamp),]

pair9_GPS <- merge(Larry_GPS, Reid_GPS)
pair9_GPS <- pair9_GPS[, c(1,2,4,3,5)]
pair9_GPS <- pair9_GPS[!duplicated(pair9_GPS$timestamp),]

pair10_GPS <- merge(Margaret_GPS, Thomas_GPS)
pair10_GPS <- pair10_GPS[, c(1,2,4,3,5)]
pair10_GPS <- pair10_GPS[!duplicated(pair10_GPS$timestamp),]

pair11_GPS <- merge(Reid_GPS, Thomas_GPS)
pair11_GPS <- pair11_GPS[, c(1,2,4,3,5)]
pair11_GPS <- pair11_GPS[!duplicated(pair11_GPS$timestamp),]

pair12_GPS <- merge(Maria_GPS, Sheron_GPS)
pair12_GPS <- pair12_GPS[, c(1,2,4,3,5)]
pair12_GPS <- pair12_GPS[!duplicated(pair12_GPS$timestamp),]

## Correlative Movement analysis ----

#Create corrData object.
cdAnteater_pair1 <- as.corrData(pair1_GPS)
#Estimate the partition points
prtsAnteater_pair1 <- findPrts(cdAnteater_pair1, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair1 <- corrMove(cdAnteater_pair1, prtsAnteater_pair1)

#Create corrData object.
cdAnteater_pair2 <- as.corrData(pair2_GPS)
#Estimate the partition points
prtsAnteater_pair2 <- findPrts(cdAnteater_pair2, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair2 <- corrMove(cdAnteater_pair2, prtsAnteater_pair2)

#Create corrData object.
cdAnteater_pair3 <- as.corrData(pair3_GPS)
#Estimate the partition points
prtsAnteater_pair3 <- findPrts(cdAnteater_pair3, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair3 <- corrMove(cdAnteater_pair3, prtsAnteater_pair3)

#Create corrData object.
cdAnteater_pair4 <- as.corrData(pair4_GPS)
#Estimate the partition points
prtsAnteater_pair4 <- findPrts(cdAnteater_pair4, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair4 <- corrMove(cdAnteater_pair4, prtsAnteater_pair4)

#Create corrData object.
cdAnteater_pair5 <- as.corrData(pair5_GPS)
#Estimate the partition points
prtsAnteater_pair5 <- findPrts(cdAnteater_pair5, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair5 <- corrMove(cdAnteater_pair5, prtsAnteater_pair5)

#Create corrData object.
cdAnteater_pair6 <- as.corrData(pair6_GPS)
#Estimate the partition points
prtsAnteater_pair6 <- findPrts(cdAnteater_pair6, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair6 <- corrMove(cdAnteater_pair6, prtsAnteater_pair6)

#Create corrData object.
cdAnteater_pair7 <- as.corrData(pair7_GPS)
#Estimate the partition points 
prtsAnteater_pair7 <- findPrts(cdAnteater_pair7, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair7 <- corrMove(cdAnteater_pair7, prtsAnteater_pair7)

#Create corrData object.
cdAnteater_pair8 <- as.corrData(pair8_GPS)
#Estimate the partition points
prtsAnteater_pair8 <- findPrts(cdAnteater_pair8, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair8 <- corrMove(cdAnteater_pair8, prtsAnteater_pair8)

#Create corrData object.
cdAnteater_pair9 <- as.corrData(pair9_GPS)
#Estimate the partition points
prtsAnteater_pair9 <- findPrts(cdAnteater_pair9, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair9 <- corrMove(cdAnteater_pair9, prtsAnteater_pair9)

#Create corrData object.
cdAnteater_pair10 <- as.corrData(pair10_GPS)
#Estimate the partition points
prtsAnteater_pair10 <- findPrts(cdAnteater_pair10, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair10 <- corrMove(cdAnteater_pair10, prtsAnteater_pair10)

#Create corrData object.
cdAnteater_pair11 <- as.corrData(pair11_GPS)
#Estimate the partition points
prtsAnteater_pair11 <- findPrts(cdAnteater_pair11, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair11 <- corrMove(cdAnteater_pair11, prtsAnteater_pair11)

#Create corrData object.
cdAnteater_pair12 <- as.corrData(pair12_GPS)
#Estimate the partition points
prtsAnteater_pair12 <- findPrts(cdAnteater_pair12, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater_pair12 <- corrMove(cdAnteater_pair12, prtsAnteater_pair12)

#RDS of correlative movement have been saved to "RDS/correlative_movement" folder














