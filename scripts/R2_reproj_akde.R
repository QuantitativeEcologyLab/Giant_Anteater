
#............................................................
# Data ----
#............................................................

#import data, cleaned giant anteater GPS tracking data, containing no outliers
DATA_GPS <- read.csv("data/anteater/Anteaters_NoOutliers.csv")

#import supplementary data containing biological information
DATA_META <- read.csv("data/anteater/Anteater_Results_Final.csv")

#correct mismatch ID entries
DATA_GPS$ID[DATA_GPS$ID == "Larry 267"] <- "Larry"
DATA_GPS$ID[DATA_GPS$ID == "Little Rick"] <- "Little_Rick"

#correct mismatch ID entries
DATA_META$ID[DATA_META$ID == "Little Rick"] <- "Little_Rick"

#subset to the 23 range-resident individuals
anteater_data <- DATA_GPS[which(DATA_GPS$ID %in% c("Alexander", "Annie", "Anthony", "Beto", "Bumpus",
                                                   "Cate", "Christoffer","Elaine", "Hannah","Jackson",
                                                   "Jane","Kyle", "Larry", "Little_Rick", "Luigi",
                                                   "Makao", "Margaret", "Maria", "Puji", "Reid", 
                                                   "Rodolfo", "Sheron", "Thomas")),]

#subset to the 23 range-resident individuals
bio_data <- DATA_META[c(1:3,8:10,12,14,17,19,20,22,23,25:29,33:35,37,38),]
#subset the biological data
bio_data <- bio_data[,c(1:3,5)]

#add site location to the dataframe
bio_data$Site <- NA
bio_data$Site[bio_data$Road == "MS-040"] <- 1
bio_data$Site[bio_data$Road == "BR_267"] <- 2



#_____________________________________________________
# Script description: fit movement models

#convert dataset to a telemetry object
DATA_TELEMETRY <- as.telemetry(anteater_data)

projection(DATA_TELEMETRY) <- median(DATA_TELEMETRY)

#summary of the dataset
summary(DATA_TELEMETRY)

#visualisation of the data
plot(DATA_TELEMETRY)

save(DATA_TELEMETRY, file = "data/anteater/telemetry_data_reproj.rda")
# load("data/anteater/telemetry_data_reproj.rda")

#............................................................
# Movement models ----
#............................................................

#fit movement models
GUESS <- lapply(DATA_TELEMETRY[1:23], function(b) ctmm.guess(b,interactive=FALSE) )
FIT <- lapply(1:23, function(i) ctmm.select(DATA_TELEMETRY[[i]],GUESS[[i]]) )
names(FIT) <- names(DATA_TELEMETRY[1:23])
overlap(FIT)

#summary of the fitted model
summary(FIT)

#save movement models
save(FIT, file = "data/anteater_fit_reproj.rda")
# load(file = "data/anteater_fit_reproj.rda")



#............................................................
# Estimating home range areas ----
#............................................................

#calculate AKDE home range estimates based on the best fit model, create aligned UDs
AKDE <- akde(DATA_TELEMETRY[1:23],FIT)
overlap(AKDE)

#save AKDE home range estimations
save(AKDE, file = "data/anteater_akdes_reproj.rda")
# load("data/anteater_akdes_reproj.rda")



#subset home range overlap based on site location
AKDE_1_reproj <- AKDE[c("Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
                 "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
                 "Puji", "Rodolfo")]

AKDE_2_reproj <- AKDE[c("Annie", "Beto", "Hannah", "Jane", "Larry",
                 "Luigi", "Margaret", "Maria", "Reid", "Sheron",
                 "Thomas")]

#save home range estimates for each site
# save(AKDE_1_reproj, file = "data/home_range/AKDE_1_reproj.rda")
# save(AKDE_2_reproj, file = "data/home_range/AKDE_2_reproj.rda")
load("data/home_range/AKDE_1_reproj.rda")
load("data/home_range/AKDE_2_reproj.rda")