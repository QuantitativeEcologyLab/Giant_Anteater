#Movement model and AKDE overlap

library(readr)
library(ctmm)            #continuous-time movement models

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant anteater")

#Import cleaned giant anteater data
DATA_GPS <- read_csv("data/Anteaters_NoOutliers.csv")
#correct a mismatch entry for 'Larry 267' and 'Larry'
#DATA_GPS$ID[DATA_GPS$ID == "Larry 267"] <- "Larry"

# Convert dataset to a telemetry object
DATA_TELEMETRY <- as.telemetry("data/Anteaters_NoOutliers.csv")
#correct a mismatch entry for 'Larry 267' and 'Larry'
names(DATA_TELEMETRY)[25] <- "Larry"

#data subsetting and carpentry
SITE_1 <- DATA_TELEMETRY[c(1,3,9,10,12,15,21,24,27,29,36,38)]
SITE_1_male <- DATA_TELEMETRY[c(1,3,12,21,24,27,38)]
SITE_1_female <- DATA_TELEMETRY[c(9,10,15,29,36)]

SITE_2 <- DATA_TELEMETRY[c(2,8,19,22,25,30,31,37,41,42)]
SITE_2_male <- DATA_TELEMETRY[c(8,25,37,42,28)]
SITE_2_female <- DATA_TELEMETRY[c(2,22,30,19,31,41)]

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