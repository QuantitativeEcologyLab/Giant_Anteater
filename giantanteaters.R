### LOAD PACKAGES ###
library("readr")
library("ctmm")
library("ggplot2")
library("dplyr") # for mutate
library("tidyr") # for pivot_longer()


### DATA PREPARATION ----

## WORKING DIRECTORY
setwd("C:\Users\achhen.stu\OneDrive - UBC\BIOL 452 Directed Studies - Giant Anteaters\Anteater Scripts\R working directory")

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
FITS.1.female <- readRDS("FIT.1.female.RDS")
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

# clean up data because there are duplicate values
proximity.data <- proximity.data[!duplicated(proximity.data[,c(1,2)]),]

# Identify pairs that were closer/further from each other
proximity.above1 <- proximity.data[which(proximity.data$proximity_low > 1),]
proximity.below1 <- proximity.data[which(proximity.data$proximity_high < 1),]

# Create time stamp table
anteater.time <- seq(from=as.POSIXct("2017-07-05"), to=as.POSIXct("2019-09-05"), by='20 mins')

# Predict the location of each individual
prediction <- predict(DATA[[1]], CTMM=FIT.ALL[[1]], t = anteater.time)
