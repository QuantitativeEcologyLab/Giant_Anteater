

### LOAD PACKAGES ###
library("readr") # for read_csv()
library("ctmm")
library("ggplot2")
library("dplyr") # for mutate(), part of tidyverse
library("tidyr") # for pivot_longer(), part of tidyverse
library("lme4") # test to see if differences are significant using glmer()
devtools::install_github("jmcalabrese/corrMove", force = TRUE)
library("corrMove")
library("lubridate") # for round_date() for corrMove

### DATA PREPARATION ----

## WORKING DIRECTORY
setwd("C:/Users/achhen.stu/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Anteater Scripts/R working directory")

## IMPORT DATASET
anteater.DATA <- read_csv("C:/Users/achhen.stu/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Anteater Dataset/giantanteater.csv", col_types = cols(timestamp = "c", class = "c", identity = "c", id = "c", .default = "d"))

## TELEMETRY FORMAT
# Convert dataset to a telemetry object, assuming the data has been cleaned and containing no outliers
DATA <- as.telemetry("C:/Users/achhen.stu/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Anteater Dataset/giantanteater.csv") # n=19

## SUBSET DATA FOR SITE 1
site.1 <- DATA[c(1,3,5:9,11,13:14,16,18)] # all individuals (n=12)
site.1.adult <- DATA[c(1,5:9,14,16,18)] # adults only (n=9)
site.1.male <- DATA[c(1,3,7,9,11,13,18)] # males only (n=7)
site.1.male.adult <- DATA[c(1,7,9,18)] # male adults only (n=4)
site.1.female <- DATA[c(5,6,8,14,16)] # female adults (n=5)

## SUBSET DATA FOR SITE 2
site.2 <- DATA[c(2, 4, 10, 12, 15, 17, 19)] # all individuals (n=7)
site.2.adult <- DATA[c(2,4,10,12,15,19)] # adults only (n=6)
site.2.male <- DATA[c(4,12,17,19)]  # males only (n=4)
site.2.male.adult <- DATA[c(4,12,19)] # male adults only (n=3)
site.2.female <- DATA[c(2,10,15)]  # female adults (n=3)

### FITTING MODELS ----
# Load fitted model for SITE 1
FIT.1 <- readRDS("FIT.1.RDS")
overlap(FIT.1)
FIT.1.male <- readRDS("FIT.1.male.RDS")
overlap(FIT.1.male)
FIT.1.male.adult <- readRDS("FIT.1.male.adult.RDS")
overlap(FIT.1.male.adult)
FIT.1.female <- readRDS("FIT.1.female.RDS")
overlap(FIT.1.female)

# Load fitted model for SITE 2
FIT.2 <- readRDS("FIT.2.RDS")
overlap(FIT.2)
FIT.2.male <- readRDS("FIT.2.male.RDS")
overlap(FIT.2.male)
FIT.2.male.adult <- readRDS("FIT.2.male.adult.RDS")
overlap(FIT.2.male.adult)
FIT.2.female <- readRDS("FIT.2.female.RDS")
overlap(FIT.2.female)

### AKDE OVERLAP ----
# Load AKDE aligned UDs for SITE 1
AKDE.1 <- readRDS("AKDE.1.RDS")
overlap(AKDE.1)
AKDE.1.male <- readRDS("AKDE.1.male.RDS")
overlap(AKDE.1.male)
AKDE.1.male.adult <- readRDS("AKDE.1.male.adult.RDS")
overlap(AKDE.1.male.adult)
AKDE.1.female <- readRDS("AKDE.1.female.RDS")
overlap(AKDE.1.female)

# Load AKDE aligned UDs for SITE 2
AKDE.2 <- readRDS("AKDE.2.RDS")
overlap(AKDE.2)
AKDE.2.male <- readRDS("AKDE.2.male.RDS")
overlap(AKDE.2.male)
AKDE.2.male.adult <- readRDS("AKDE.2.male.adult.RDS")
overlap(AKDE.2.male.adult)
AKDE.2.female <- readRDS("AKDE.2.female.RDS")
overlap(AKDE.2.female)

# Adding a meta dataset from a supplementary dataset ----
METADATA <- read_csv("C:/Users/achhen.stu/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Anteater Dataset/Anteater_Results_Final.csv")

# Must correct a mismatch entry for 'Larry 267' and 'Larry' between dataset and meta dataset
METADATA <- mutate(select(METADATA, 1:3), ID = if_else(condition = ID == 'Larry',
                                                       true = 'Larry 267',
                                                       false = ID))

### PAIRWISE ANALYSIS SEX COMPARISON ----
# creating the data frame from overlap values

## PAIRWISE ANALYSIS SEX COMPARISON FOR SITE 1 ----
# extracting median layer
overlap.1 <- overlap(object = AKDE.1, level = 0.95) # assigns the overlap results as an object
overlap.1.median <- overlap.1$CI[ , , 2] # extract the median layer (pulling out the section) of the cube (aka array)
overlap.1.median[upper.tri(overlap.1.median, diag = TRUE)] <- NA # removing the upper triangle & diagonal
# converting the overlap median layer matrix triangle into a pairwise dataframe
pairwise.1.matrix <- as.data.frame(overlap.1.median) # Convert matrix to data frame
pairwise.1.matrix$anteater_A <- rownames(pairwise.1.matrix) # add column of individual names
pairwise.1.pivot <- pivot_longer(pairwise.1.matrix, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap', values_drop_na = TRUE)
# add columns to the data frame matrix
pairwise.1.pivot.A <- left_join(pairwise.1.pivot, rename(METADATA, anteater_A = ID), by = "anteater_A")
pairwise.1.pivot.B <- left_join(pairwise.1.pivot.A, rename(METADATA, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
# adding column to indicate which sexes that are being compared
pairwise.1.df <- mutate(pairwise.1.pivot.B, sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male:male",
                                                                       paste(Sex.A, Sex.B) == "Female Female" ~ "female:female",
                                                                       paste(Sex.A, Sex.B) == "Male Female" ~ "male:female",
                                                                       paste(Sex.A, Sex.B) == "Female Male" ~ "male:female"))
# removing subadults from dataframe matrix
pairwise.1.df.A <- pairwise.1.df[which(pairwise.1.df$Age.A != "Subadult"),] # removing subadults from anteater_A from matrix
pairwise.1.df.adult <- pairwise.1.df.A[which(pairwise.1.df.A$Age.B != "Subadult"),] # removing subadults from anteater_B from matrix with anteater_A filtered

## PAIRWISE ANALYSIS SEX COMPARISON FOR SITE 2 ----
# extracting median layer
overlap.2 <- overlap(object = AKDE.2, level = 0.95) # assigns the overlap results as an object
overlap.2.median <- overlap.2$CI[ , , 2] # extract the median layer (pulling out the section) of the cube (aka array)
overlap.2.median[upper.tri(overlap.2.median, diag = TRUE)] <- NA # removing the upper triangle & diagonal of the matrix
# converting the overlap median layer matrix triangle into a pairwise dataframe
pairwise.2.matrix <- as.data.frame(overlap.2.median) # Convert matrix to data frame
pairwise.2.matrix$anteater_A <- rownames(pairwise.2.matrix) # add column of individual names
pairwise.2.pivot <- pivot_longer(pairwise.1.matrix, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap', values_drop_na = TRUE)
# add columns to the data frame matrix
pairwise.2.pivot.A <- left_join(pairwise.2.pivot, rename(METADATA, anteater_A = ID), by = "anteater_A")
pairwise.2.pivot.B <- left_join(pairwise.2.pivot.A, rename(METADATA, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
# adding column to indicate which sexes that are being compared
pairwise.2.df <- mutate(pairwise.2.pivot.B, sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male:male",
                                                                       paste(Sex.A, Sex.B) == "Female Female" ~ "female:female",
                                                                       paste(Sex.A, Sex.B) == "Male Female" ~ "male:female",
                                                                       paste(Sex.A, Sex.B) == "Female Male" ~ "male:female"))
# removing subadults from dataframe matrix
pairwise.2.df.A <- pairwise.2.df[which(pairwise.2.df$Age.A != "Subadult"),] # removing subadults from anteater_A from matrix
pairwise.2.df.adult <- pairwise.2.df.A[which(pairwise.2.df.A$Age.B != "Subadult"),] # removing subadults from anteater_B from matrix with anteater_A filtered

# PAIRWISE COMPARISON ANALYSIS OF SEX OVERALL COMPARISON ## ----
# All individuals
pairwise.df <- bind_rows(pairwise.1.df, pairwise.2.df)
# Adults only
pairwise.df.adult <- bind_rows(pairwise.1.df.adult, pairwise.2.df.adult)

### PROXIMITY PAIRWISE ANALYSIS BETWEEN SEX ## ----
# refer to help("proximity")

# combining the fitted models for both sites
FIT.ALL <- c(FIT.1, FIT.2)

# Load Proximity Analysis results
proximity.data <- read_csv("proximity.csv")

# Identify pairs that were closer/further from each other
proximity.above1 <- proximity.data[which(proximity.data$proximity_low > 1),]
proximity.below1 <- proximity.data[which(proximity.data$proximity_high < 1),]

### PREP PROXIMITY RESULTS FOR CORRMOVE ANALYSIS ----
# clean up data because there are duplicate values
proximity.data <- proximity.data[!duplicated(proximity.data[,c(1,2)]),]

# Create time stamp table
anteater.time <- seq(from=as.POSIXct("2017-07-05"), to=as.POSIXct("2019-09-05"), by='20 mins')

## Predict the location of each individual
# Load location predictions
predict.Alexander <- readRDS("predict.Alexander.RDS")
predict.Bumpus <- readRDS("predict.Bumpus.RDS")
predict.Christoffer <- readRDS("predict.Christoffer.RDS")
predict.Elaine <- readRDS("predict.Elaine.RDS")
predict.Kyle <- readRDS("predict.Kyle.RDS")
predict.LittleRick <- readRDS("predict.LittleRick.RDS")
predict.Makao <- readRDS("predict.Makao.RDS")
predict.Puji <- readRDS("predict.Puji.RDS")
predict.Rodolfo <- readRDS("predict.Rodolfo.RDS")

#Extract some test individuals and do some data carpentry
Elaine <- DATA$Elaine #data$Elaine
Christoffer <- DATA$Christoffer # data$Christoffer
plot(list(Elaine, Christoffer), col = c("red", "blue"))
El <- data.frame(timestamp = round_date(Elaine$timestamp, "20 minutes") ,
                 E.x = Elaine$longitude,
                 E.y = Elaine$latitude)
Chris <- data.frame(timestamp = round_date(Christoffer$timestamp, "20 minutes"),
                    S.x = Christoffer$longitude,
                    S.y = Christoffer$latitude)
test <- merge(El, Chris)
test <- test[, c(1,2,4,3,5)]
test <- test[!duplicated(test$timestamp),]
#Create corrData object.
cdAnteater <- as.corrData(test)
#Estimate the partition points for the khulan data, with W=25
prtsAnteater <- findPrts(cdAnteater, W=5, IC = 2)
#Get the MCI estimates and selected model conditional on the data and partition points
cmAnteater <- corrMove(cdAnteater, prtsAnteater)
#3-panel plot of the MCIs over time
plot.corrMove(cmAnteater)
title("Elaine and Christoffer")
