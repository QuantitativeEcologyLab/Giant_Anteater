# correlative movement

library(readr)
#devtools::install_github("ctmm-initiative/ctmm", force = TRUE) #if package needs to be updated
library(ctmm)            #continuous-time movement models
library(lubridate)       #round_date() for corrMove
library(dplyr)
devtools::install_github("jmcalabrese/corrMove", force = TRUE)
library(corrMove)        #correlative movement

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant anteater")

# Import data
#import cleaned GPS giant anteater data
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

#load RDS files
overlap_df  <- readRDS("RDS/overlap_df.RDS")
proximity_df <- readRDS("RDS/proximity_df.RDS")
overlap_df <- left_join(overlap_df, proximity_df, by = c("anteater_A", "anteater_B",
                                                         "Sex.A", "Sex.B",
                                                         "Age.A", "Age.B",
                                                         "sex_comparison",
                                                         "site"))

## Proximity Ratio pairs ----
proximity_identified_pairs_df <- readRDS("RDS/proximity_identified_pairs_df.RDS")

# Data preparation for correlative movement analysis ----
#data carpentry
Bumpus <- DATA_TELEMETRY$Bumpus
Christoffer <- DATA_TELEMETRY$Christoffer
Elaine <- DATA_TELEMETRY$Elaine
Kyle <- DATA_TELEMETRY$Kyle
Little_rick <- DATA_TELEMETRY$Little_Rick
Makao <- DATA_TELEMETRY$Makao
Puji <- DATA_TELEMETRY$Puji
Rodolfo <- DATA_TELEMETRY$Rodolfo
Annie <- DATA_TELEMETRY$Annie
Larry <- DATA_TELEMETRY$Larry
Margaret <- DATA_TELEMETRY$Margaret
Maria <- DATA_TELEMETRY$Maria
Sheron <- DATA_TELEMETRY$Sheron
Reid <- DATA_TELEMETRY$Reid
Thomas <- DATA_TELEMETRY$Thomas

#create a dataframe of an individuals GPS coordinates, format dataset for corrMove analysis
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
Maria_GPS <- data.frame(timestamp = round_date(Maria$timestamp, "20 minutes"),
                        Maria.x = Maria$longitude,
                        Maria.y = Maria$latitude)
Sheron_GPS <- data.frame(timestamp = round_date(Sheron$timestamp, "20 minutes"),
                         Sheron.x = Sheron$longitude,
                         Sheron.y = Sheron$latitude)
Reid_GPS <- data.frame(timestamp = round_date(Reid$timestamp, "20 minutes"),
                       Reid.x = Reid$longitude,
                       Reid.y = Reid$latitude)
Thomas_GPS <- data.frame(timestamp = round_date(Thomas$timestamp, "20 minutes"),
                         Thomas.x = Thomas$longitude,
                         Thomas.y = Thomas$latitude)

#combine the GPS coordinates of pairs together
#reorganize dataframe needed for corrMove into format required: timestamp, ID1 x coord, ID2 x coord, ID1 y coord, ID2 y coord
#remove duplicate timestamps

#Create corrData object, manually not using as.corrData()
cd_pair1 <- merge(Kyle_GPS, Christoffer_GPS)
cd_pair1 <- cd_pair1[, c(1,2,4,3,5)]
cd_pair1 <- cd_pair1[!duplicated(cd_pair1$timestamp),]
#Estimate the partition points
prts_pair1 <- findPrts(cd_pair1, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair1 <- corrMove(cd_pair1, prts_pair1)

#Create corrData object, manually not using as.corrData()
cd_pair2 <- merge(Elaine_GPS, Christoffer_GPS)
cd_pair2 <- cd_pair2[, c(1,2,4,3,5)]
cd_pair2 <- cd_pair2[!duplicated(cd_pair2$timestamp),]
#Estimate the partition points
prts_pair2 <- findPrts(cd_pair2, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair2 <- corrMove(cd_pair2, prts_pair2)

#Create corrData object, manually not using as.corrData()
cd_pair3 <- merge(Kyle_GPS, Bumpus_GPS, )
cd_pair3 <- cd_pair3[, c(1,2,4,3,5)]
cd_pair3 <- cd_pair3[!duplicated(cd_pair3$timestamp),]
#Estimate the partition points
prts_pair3 <- findPrts(cd_pair3, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair3 <- corrMove(cd_pair3, prts_pair3)

#Create corrData object, manually not using as.corrData()
cd_pair4 <- merge(Little_rick_GPS, Elaine_GPS)
cd_pair4 <- cd_pair4[, c(1,2,4,3,5)]
cd_pair4 <- cd_pair4[!duplicated(cd_pair4$timestamp),]
#Estimate the partition points
prts_pair4 <- findPrts(cd_pair4, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair4 <- corrMove(cd_pair4, prts_pair4)

#Create corrData object, manually not using as.corrData()
cd_pair5 <- merge(Makao_GPS, Bumpus_GPS)
cd_pair5 <- cd_pair5[, c(1,2,4,3,5)]
cd_pair5 <- cd_pair5[!duplicated(cd_pair5$timestamp),]
#Estimate the partition points
prts_pair5 <- findPrts(cd_pair5, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair5 <- corrMove(cd_pair5, prts_pair5)

#Create corrData object, manually not using as.corrData()
cd_pair6 <- merge(Puji_GPS, Bumpus_GPS)
cd_pair6 <- cd_pair6[, c(1,2,4,3,5)]
cd_pair6 <- cd_pair6[!duplicated(cd_pair6$timestamp),]
#Estimate the partition points
prts_pair6 <- findPrts(cd_pair6, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair6 <- corrMove(cd_pair6, prts_pair6)

#Create corrData object, manually not using as.corrData()
cd_pair7 <- merge(Rodolfo_GPS, Elaine_GPS)
cd_pair7 <- cd_pair7[, c(1,2,4,3,5)]
cd_pair7 <- cd_pair7[!duplicated(cd_pair7$timestamp),]
#Estimate the partition points 
prts_pair7 <- findPrts(cd_pair7, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair7 <- corrMove(cd_pair7, prts_pair7)

#Create corrData object, manually not using as.corrData()
cd_pair8 <- merge(Larry_GPS, Annie_GPS)
cd_pair8 <- cd_pair8[, c(1,2,4,3,5)]
cd_pair8 <- cd_pair8[!duplicated(cd_pair8$timestamp),]
#Estimate the partition points
prts_pair8 <- findPrts(cd_pair8, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair8 <- corrMove(cd_pair8, prts_pair8)

#Create corrData object, manually not using as.corrData()
cd_pair9 <- merge(Reid_GPS, Larry_GPS)
cd_pair9 <- cd_pair9[, c(1,2,4,3,5)]
cd_pair9 <- cd_pair9[!duplicated(cd_pair9$timestamp),]
#Estimate the partition points
prts_pair9 <- findPrts(cd_pair9, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair9 <- corrMove(cd_pair9, prts_pair9)

#Create corrData object, manually not using as.corrData()
cd_pair10 <- merge(Sheron_GPS, Maria_GPS)
cd_pair10 <- cd_pair10[, c(1,2,4,3,5)]
cd_pair10 <- cd_pair10[!duplicated(cd_pair10$timestamp),]
#Estimate the partition points
prts_pair10 <- findPrts(cd_pair10, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair10 <- corrMove(cd_pair10, prts_pair10)

#Create corrData object, manually not using as.corrData()
cd_pair11 <- merge(Thomas_GPS, Margaret_GPS)
cd_pair11 <- cd_pair11[, c(1,2,4,3,5)]
cd_pair11 <- cd_pair11[!duplicated(cd_pair11$timestamp),]
#Estimate the partition points
prts_pair11 <- findPrts(cd_pair11, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair11 <- corrMove(cd_pair11, prts_pair11)

#Create corrData object, manually not using as.corrData()
cd_pair12 <- merge(Thomas_GPS, Reid_GPS)
cd_pair12 <- cd_pair12[, c(1,2,4,3,5)]
cd_pair12 <- cd_pair12[!duplicated(cd_pair12$timestamp),]
#Estimate the partition points
prts_pair12 <- findPrts(cd_pair12, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cm_pair12 <- corrMove(cd_pair12, prts_pair12)

saveRDS(cm_pair1, file = "RDS/cm_pair1.RDS")
saveRDS(cm_pair2, file = "RDS/cm_pair2.RDS")
saveRDS(cm_pair3, file = "RDS/cm_pair3.RDS")
saveRDS(cm_pair4, file = "RDS/cm_pair4.RDS")
saveRDS(cm_pair5, file = "RDS/cm_pair5.RDS")
saveRDS(cm_pair6, file = "RDS/cm_pair6.RDS")
saveRDS(cm_pair7, file = "RDS/cm_pair7.RDS")
saveRDS(cm_pair8, file = "RDS/cm_pair8.RDS")
saveRDS(cm_pair9, file = "RDS/cm_pair9.RDS")
saveRDS(cm_pair10, file = "RDS/cm_pair10.RDS")
saveRDS(cm_pair11, file = "RDS/cm_pair11.RDS")
saveRDS(cm_pair12, file = "RDS/cm_pair12.RDS")
