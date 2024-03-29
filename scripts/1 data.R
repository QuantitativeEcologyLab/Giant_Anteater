
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


#save GPS dataframe
# save(anteater_data, file = "data/anteater/anteater_data.rda")
load("data/anteater/anteater_data.rda")
#save biological information dataframe
# save(bio_data, file = "data/anteater/bio_data.rda")
load("data/anteater/bio_data.rda")

