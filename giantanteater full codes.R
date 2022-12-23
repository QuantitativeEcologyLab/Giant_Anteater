### FULL CODE (includes notes) ###
devtools::install_github("r-lib/devtools")
devtools::install_github("ctmm-initiative/ctmm")
devtools::install_github("jmcalabrese/corrMove", force = TRUE)

### LOAD PACKAGES ###
library("devtools")
library("readr")
library("ctmm")
library("ggplot2")
library("tidyr") # for pivot_longer()

### DATA PREPARATION ----

## WORKING DIRECTORY
setwd("C:\Users\achhen.stu\OneDrive - UBC\BIOL 452 Directed Studies - Giant Anteaters\Anteater Scripts\R working directory")

## IMPORT DATASET
anteater.DATA <- read_csv("C:/Users/achhen.stu/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Anteater Dataset/giantanteater.csv", col_types = cols(timestamp = "c", class = "c", identity = "c", id = "c", .default = "d"))

## TELEMETRY FORMAT
# Convert dataset to a telemetry object, assuming the data has been cleaned and containing no outliers
#DATA <- as.telemetry(anteater.DATA) # n=19
  #Error in strptime(xx, f, tz = tz) : input string is too long .-. used code below
DATA <- as.telemetry("C:/Users/achhen.stu/OneDrive - UBC/BIOL 452 Directed Studies - Giant Anteaters/Anteater Dataset/giantanteater.csv")

### SUBSET DATA FOR OVERLAP ----
# Subset data to isolate individuals found only in specific sites
# general syntax: subsetname <- originaldataset[c(listofelements)] 
# to select multiple elements from a vector, add square brackets
# refer to https://campus.datacamp.com/courses/free-introduction-to-r/chapter-2-vectors-2?ex=11

## SUBSET DATA FOR SITE 1
site.1 <- DATA[c(1,3,5:9,11,13:14,16,18)] # all individuals (n=12)
site.1.adult <- DATA[c(1,5:9,14,16,18)] # adults only (n=9)
site.1.male <- DATA[c(1,3,7,9,11,13,18)] # males only (n=7)
site.1.male.adult <- DATA[c(1,7,9,18)] # male adults only (n=4)
site.1.female <- DATA[c(5,6,8,14,16)] # female adults (n=5)
# names() to check if individuals are correct and accounted for

## SUBSET DATA FOR SITE 2
site.2 <- DATA[c(2, 4, 10, 12, 15, 17, 19)] # all individuals (n=7)
site.2.adult <- DATA[c(2,4,10,12,15,19)] # adults only (n=6)
site.2.male <- DATA[c(4,12,17,19)]  # males only (n=4)
site.2.male.adult <- DATA[c(4,12,19)] # male adults only (n=3)
site.2.female <- DATA[c(2,10,15)]  # female adults (n=3)
# names() to check if individuals are correct and accounted for

### FIT MOVEMENT MODELS ----

## FITTING MODELS FOR SITE 1 ----
# SITE 1 - ALL INDIVIDUALS (n=12)
GUESS.1 <- lapply(site.1[1:12], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.1 <- lapply(1:12, function(i) ctmm.select(site.1[[i]],GUESS.1[[i]]) )
names(FIT.1) <- names(site.1[1:12])
saveRDS(object = FIT.1, file = "FIT.1.RDS")
# Load saved fitted model
FIT.1 <- readRDS("FIT.1.RDS")
overlap(FIT.1)

# SITE 1 - MALES ONLY (n=7)
GUESS.1.male <- lapply(site.1.male[1:7], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.1.male <- lapply(1:7, function(i) ctmm.select(site.1.male[[i]],GUESS.1.male[[i]]) )
names(FIT.1.male) <- names(site.1.male[1:7])
saveRDS(object = FIT.1.male, file = "FIT.1.male.RDS")
# Load saved fitted model
FIT.1.male <- readRDS("FIT.1.male.RDS")
overlap(FIT.1.male)

# SITE 1 - MALE ADULTS ONLY (n=4)
GUESS.1.male.adult <- lapply(site.1[1:4], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.1.male.adult <- lapply(1:12, function(i) ctmm.select(site.1.male.adult[[i]],GUESS.1.male.adult[[i]]) )
names(FIT.1.male.adult) <- names(site.1.male.adult[1:4])
saveRDS(object = FIT.1.male.adult, file = "FIT.1.male.adult.RDS")
# Load saved fitted model
FIT.1.male.adult <- readRDS("FIT.1.male.adult.RDS")
overlap(FIT.1.male.adult)

# SITE 1 - FEMALES ONLY (n=5)
GUESS.1.female <- lapply(site.1.female[1:5], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.1.female <- lapply(1:5, function(i) ctmm.select(site.1.female[[i]],GUESS.1.female[[i]]) )
names(FIT.1.female) <- names(site.1.female[1:5])
saveRDS(object = FIT.1.female, file = "FIT.1.female.RDS")
# Load saved fitted model
FITS.1.female <- readRDS("FIT.1.female.RDS")
overlap(FIT.1.female)

## FITTING MODELS FOR SITE 2 ----

# SITE 2 - ALL INDIVIDUALS (n=7)
GUESS.2 <- lapply(site.2[1:7], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.2 <- lapply(1:7, function(i) ctmm.select(site.2[[i]],GUESS.2[[i]]) )
names(FIT.2) <- names(site.2[1:7])
saveRDS(object = FIT.2, file = "FIT.2.RDS")
# Load saved fitted model
FIT.2 <- readRDS("FIT.2.RDS")
overlap(FIT.2)

# SITE 2 - MALES ONLY (n=4)
GUESS.2.male <- lapply(site.2.male[1:4], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.2.male <- lapply(1:4, function(i) ctmm.select(site.2.male[[i]],GUESS.2.male[[i]]) )
names(FIT.2.male) <- names(site.2.male[1:4])
saveRDS(object = FIT.2.male, file = "FIT.2.male.RDS")
# Load saved fitted model
FIT.2.male <- readRDS("FIT.2.male.RDS")
overlap(FIT.2.male)

# SITE 2 - MALE ADULTS ONLY (n=3)
GUESS.2.male.adult <- lapply(site.2.male.adult[1:3], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.2.male.adult <- lapply(1:3, function(i) ctmm.select(site.2.male.adult[[i]],GUESS.2.male.adult[[i]]) )
names(FIT.2.male.adult) <- names(site.2.male.adult[1:3])
saveRDS(object = FITS.2.male.adult, file = "FIT.2.male.adult.RDS")
# Load saved fitted model
FIT.2.male.adult <- readRDS("FIT.2.male.adult.RDS")
overlap(FIT.2.male.adult)

# SITE 2 - FEMALES ONLY (n=3)
GUESS.2.female <- lapply(site.2.female[1:3], function(b) ctmm.guess(b,interactive=FALSE) )
FIT.2.female <- lapply(1:3, function(i) ctmm.select(site.2.female[[i]],GUESS.2.female[[i]]) )
names(FIT.2.female) <- names(site.2.female[1:3])
saveRDS(object = FIT.2.female, file = "FIT.2.female.RDS")
# Load saved fitted model
FIT.2.female <- readRDS("FIT.2.female.RDS")
overlap(FIT.2.female)

### AKDE OVERLAP ----
# create aligned UDs

## AKDE OVERLAP SITE 1 ----

# SITE 1 - ALL INDIVIDUALS (n=12)
AKDE.1 <- akde(site.1[1:12],FIT.1)
saveRDS(object = AKDE.1, file = "AKDE.1.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.1 <- readRDS("AKDE.1.RDS")
overlap(AKDE.1)
# colour coding sexes for plot
COL.1 <- c("blue", "light blue", "red", "red", "blue", "red", "blue", "light blue", "light blue", "red",
           "red", "blue") # blue = male; light blue = subadult male; red = female
png(file = "Overlap.1.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1, col.DF = COL.1, col.level = COL.1, col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: all individuals)")
dev.off()

# SITE 1 - MALES ONLY (n=7)
AKDE.1.male <- akde(site.1[1:7],FIT.1.male)
saveRDS(object = AKDE.1.male, file = "AKDE.1.male.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.1.male <- readRDS("AKDE.1.male.RDS")
overlap(AKDE.1.male)
png(file = "Overlap.1.male.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1.male, col.DF = "blue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: males only)")
dev.off()

# SITE 1 - MALE ADULTS ONLY (n=4)
AKDE.1.male.adult <- akde(site.1.male.adult[1:4],FIT.1.male.adult)
saveRDS(object = AKDE.1.male.adult, file = "AKDE.1.male.adult.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.1.male.adult <- readRDS("AKDE.1.male.adult.RDS")
overlap(AKDE.1.male.adult)
png(file = "Overlap.1.male.adult.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1.male.adult, col.DF = "blue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: male adults only)")
dev.off()

# SITE 1 - FEMALES ONLY (n=5)
AKDE.1.female <- akde(site.1.female[1:5],FIT.1.female)
saveRDS(object = AKDE.1.female, file = "AKDE.1.female.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.1.female <- readRDS("AKDE.1.female.RDS")
overlap(AKDE.1.female)
png(file = "Overlap.1.female.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1.female, col.DF = "red", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: females only)")
dev.off()

## AKDE OVERLAP SITE 2 ----

# SITE 2 - ALL INDIVIDUALS (n=7)
AKDE.2 <- akde(site.2[1:7],FITS.2)
saveRDS(object = AKDE.2, file = "AKDE.2.RDS")
# Load saved AKDE aligned UDs
AKDE.2 <- readRDS("AKDE.2.RDS")
overlap(AKDE.2)
# colour coding sexes for plot
COL.2 <- c("red", "blue", "red", "blue", "red", "light blue", "blue") # blue = male; light blue = subadult male; red = female
png(file = "Overlap.2.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2, col.DF = COL.2, col.level = COL.2, col.grid = NA, level = NA) 
title("aKDE Overlap (Site 2: all individuals)")
dev.off()plot(AKDE.2)

# SITE 2 - MALES ONLY (n=4)
AKDE.2.male <- akde(site.2[1:4],FIT.2.male)
saveRDS(object = AKDE.2.male, file = "AKDE.2.male.RDS")
# Load saved AKDE aligned UDs
AKDE.2.male <- readRDS("AKDE.2.male.RDS")
overlap(AKDE.2.male)
png(file = "Overlap.2.male.adult.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2.male.adult, col.DF = "blue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: male adults only)")
dev.off()

# SITE 2 - MALE ADULTS ONLY (n=3)
AKDE.2.male.adult <- akde(site.2.male.adult[1:3],FITS.2.male.adult)
saveRDS(object = AKDE.2.male.adult, file = "AKDE.2.male.adult.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.2.male.adult <- readRDS("AKDE.2.male.adult.RDS")
overlap(AKDE.2.male.adult)
png(file = "Overlap.2.male.adult.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2.male.adult, col.DF = "blue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: male adults only)")
dev.off()

# SITE 2 - FEMALES ONLY (n=3)
AKDE.2.female <- akde(site.2.female[1:3],FITS.2.female)
saveRDS(object = AKDE.2.female, file = "AKDE.2.female.RDS")
# Load saved AKDE aligned UDs, see original file for code
AKDE.2.female <- readRDS("AKDE.2.female.RDS")
overlap(AKDE.2.female)
png(file = "Overlap.2.female.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.2.female, col.DF = "red", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: females only)")
dev.off()

# PLOT AKDE OVERLAP SEX SPECIFIC # ----

# OVERLAP colour code individuals as male/female on the overlap plot
# Make a list of colours that matches the order of the HR estimates
COL.1 <- c("blue", "light blue", "red", "red", "blue", "red", "blue", "light blue", "light blue", "red",
           "red", "blue")
png(file = "Overlap.1.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE.1, col.DF = COL.1, col.level = COL.1, col.grid = NA, level = NA)
title("Overlap aKDE (Site 1)")
# blue = male
# light blue = subadult male
# red = female
dev.off()
#ggplot has a different saving code/method -> ggsave()

COL.2 <- c("red", "blue", "red", "blue", "red", "light blue", "blue")
plot(AKDE.2, col.DF = COL.2, col.level = COL.2, col.grid = NA, level = NA)
png(file = "Overlap.2.png", width = 6.86, height = 6, units = "in", res = 600)
title("Overlap aKDE (Site 2)")
# blue = male
# light blue = subadult male
# red = female
dev.off()

#### ALTERNATE CODE ####
# using if/else to fit models & to include save and load RDS file

# SITE 1 - ALL INDIVIDUALS (n=12)
GUESS.1 <- lapply(site.1[1:12], function(b) ctmm.guess(b,interactive=FALSE) ) #shouldn't take too long to run, may not be necessary to save as RDS
if(!file.exists("FIT.1.RDS"))  {# ! = not
  FIT.1 <- lapply(1:12, function(i) ctmm.select(site.1[[i]],GUESS.1[[i]]) )
  names(FIT.1) <- names(site.1[1:12])
  saveRDS(object = FIT.1, file = "FIT.1.RDS")
} else {# to import/load RDS file, it will appear in the console .-. need to load
  FIT.1 <- readRDS("FIT.1.RDS")
}
overlap(FIT.1)

# SITE 2 - ALL INDIVIDUALS (n=7)
GUESS.2 <- lapply(site.2[1:7], function(b) ctmm.guess(b,interactive=FALSE) ) #shouldn't take too long to run, may not be necessary to save as RDS
if(!file.exists("fittedmodels.2.RDS"))  {# ! = not
  FITS.2 <- lapply(1:7, function(i) ctmm.select(site.2[[i]],GUESS.2[[i]]) )
  names(FIT.2) <- names(site.2[1:7])
  saveRDS(object = FIT.2, file = "FIT.2.RDS")
} else {# to import/load RDS file, it will appear in the console .-. need to load
  FIT.2 <- readRDS("FIT.2.RDS")
}
overlap(FIT.2)

## META DATASET (for pairwise analysis) ## ----

# Adding a meta dataset from a supplementary dataset
METADATA <- read_csv("C:\Users\achhen.stu\OneDrive - UBC\BIOL 452 Directed Studies - Giant Anteaters\Anteater Dataset\Anteater_Results_Final.csv")

# Must correct a mismatch entry for 'Larry 267' and 'Larry' between dataset and meta dataset
METADATA <- mutate(select(METADATA, 1:3), ID = if_else(condition = ID == 'Larry',
                                                                            true = 'Larry 267',
                                                                            false = ID))

### PAIRWISE ANALYSIS SEX COMPARISON ----
# creating the data frame from overlap values

## PAIRWISE ANALYSIS SEX COMPARISON FOR SITE 1 ----
# SITE 1 - taking the overlap cube (array), extracting median layer, removing one portion of the triangle
overlap.1 <- overlap(object = AKDE.1, level = 0.95) # assigns the overlap results as an object
overlap.1.median <- overlap.1$CI[ , , 2] # extract the median layer (pulling out the section) of the cube (aka array)
# [, , ,] = [row, column, layer] of the cube/array
# values of layers: 1 = low, 2 = median (50%), 3 = high
overlap.1.median[upper.tri(overlap.1.median, diag = TRUE)] <- NA # removing the upper triangle & diagonal of the
# matrix because the triangle are duplicate values because its a symmetric matrix
# removing the diagonal because it is the value of home range of yourself
# therefore we change upper and diag to NA
# setting diag = TRUE, because 1. select the values you don't want then 2. remove the values by using "<- NA"
overlap.1.median # to check everything is correct (full-screen console for best results)

# converting the overlap median layer matrix triangle into a pairwise dataframe
pairwise.1.matrix <- as.data.frame(overlap.1.median) # Convert matrix to data frame
pairwise.1.matrix$anteater_A <- rownames(pairwise.1.matrix) # add column of individual names
pairwise.1.pivot <- pivot_longer(pairwise.1.matrix, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap', values_drop_na = TRUE)
# table is too wide .-. rotate it to make it long

# add columns to the dataframe matrix, general syntax -> join_type(firstTable, secondTable, by=columnTojoinOn)
pairwise.1.pivot.A <- left_join(pairwise.1.pivot, rename(METADATA, anteater_A = ID), by = "anteater_A")
# adding anteater_A info to matrix
pairwise.1.pivot.B <- left_join(pairwise.1.pivot.A, rename(METADATA, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
# adding anteater_B info to matrix with anteater_A info
pairwise.1.df <- mutate(pairwise.1.pivot.B, sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male:male",
                                                                       paste(Sex.A, Sex.B) == "Female Female" ~ "female:female",
                                                                       paste(Sex.A, Sex.B) == "Male Female" ~ "male:female",
                                                                       paste(Sex.A, Sex.B) == "Female Male" ~ "male:female"))
# adding column to indicate which sexes that are being compared
pairwise.1.df # to check if matrix has all the correct columns, variables etc., with no NA values

# removing subadults from dataframe matrix
pairwise.1.df.A <- pairwise.1.df[which(pairwise.1.df$Age.A != "Subadult"),] # removing subadults from anteater_A from matrix
pairwise.1.df.adult <- pairwise.1.df.A[which(pairwise.1.df.A$Age.B != "Subadult"),] # removing subadults from anteater_B from matrix with anteater_A filtered

# Plot pairwise sex comparison for SITE 1 ----
## SITE 1 plot pairwise comparison (all)
ggplot(data = pairwise.1.df, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 1: all)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.1.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## SITE 1 plot pairwise comparison (adults only)
ggplot(data = pairwise.1.df.adult, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 1: adults only)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.1.adult.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## PAIRWISE ANALYSIS SEX COMPARISON FOR SITE 2 ----
# taking the overlap cube, extracting median layer, removing one portion of the triangle
overlap.2 <- overlap(object = AKDE.2, level = 0.95) # assigns the overlap results as an object
overlap.2.median <- overlap.2$CI[ , , 2] # extract the median layer (pulling out the section) of the cube (aka array)
overlap.2.median[upper.tri(overlap.2.median, diag = TRUE)] <- NA # removing the upper triangle & diagonal of the matrix
#View(overlap.2.median) # to check everything is correct (full-screen console for best results)

# converting the overlap median layer matrix triangle into a pairwise dataframe
pairwise.2.matrix <- as.data.frame(overlap.2.median) # Convert matrix to data frame
pairwise.2.matrix$anteater_A <- rownames(pairwise.2.matrix) # add column of individual names
pairwise.2.pivot <- pivot_longer(pairwise.1.matrix, cols = -anteater_A, names_to = 'anteater_B', values_to = 'overlap', values_drop_na = TRUE)
# table is too wide .-. rotate it to make it long


# add columns to the dataframe matrix, general syntax -> join_type(firstTable, secondTable, by=columnTojoinOn)
pairwise.2.pivot.A <- left_join(pairwise.2.pivot, rename(METADATA, anteater_A = ID), by = "anteater_A")
# adding anteater_A info to matrix
pairwise.2.pivot.B <- left_join(pairwise.2.pivot.A, rename(METADATA, anteater_B = ID), by = "anteater_B", suffix = c(".A", ".B"))
# adding anteater_B info to matrix with anteater_A info
pairwise.2.df <- mutate(pairwise.2.pivot.B, sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male:male",
                                                                       paste(Sex.A, Sex.B) == "Female Female" ~ "female:female",
                                                                       paste(Sex.A, Sex.B) == "Male Female" ~ "male:female",
                                                                       paste(Sex.A, Sex.B) == "Female Male" ~ "male:female"))
# adding column to indicate which sexes that are being compared
pairwise.2.df # to check if matrix is good, has all the correct columns, variables etc. ie. no NA values

# removing subadults from dataframe matrix
pairwise.2.df.A <- pairwise.2.df[which(pairwise.2.df$Age.A != "Subadult"),] # removing subadults from anteater_A from matrix
pairwise.2.df.adult <- pairwise.2.df.A[which(pairwise.2.df.A$Age.B != "Subadult"),] # removing subadults from anteater_B from matrix with anteater_A filtered

# Plot pairwise sex comparison for SITE 2
## SITE 2 plot pairwise comparison (all)
ggplot(data = pairwise.2.df, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 2: all)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.2.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## SITE 2 plot pairwise comparison (adults only)
ggplot(data = pairwise.2.df.adult, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 2: adults only)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.2.adult.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

# PAIRWISE COMPARISON ANALYSIS OF SEX OVERALL COMPARISON ## ----
# use bind_rows() to join the 2 pairwise comparison and then plot it

# All individuals
pairwise.df <- bind_rows(pairwise.1.df, pairwise.2.df)
#View(pairwise.df)

# Adults only
pairwise.df.adult <- bind_rows(pairwise.1.df.adult, pairwise.2.df.adult)

# NOTE: pairwise coding comprehension from Stefano -> re-coded the pipe version

# Plot Pairwise Comparison SITE 1 & 2 ----
# Plot combined SITE 1 & 2 pairwise analysis (All individuals)
ggplot(data = pairwise.df, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex") +
  ggtitle("Anteater overlap pairwise comparison of sexes (all)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.combined.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

# Plot combined SITE 1 & 2 pairwise analysis (Adults only)
ggplot(data = pairwise.df.adults, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex") +
  ggtitle("Anteater overlap pairwise comparison of sexes (Adults only)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "pairwise.combined.adultsonly.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

# Quick test to see if differences are significant -------
test.sex <- glmer(overlap ~ sex_comparison + (1|Sex.A), family = "binomial", data = pairwise.df.adults)
summary(test.sex)

### PROXIMITY PAIRWISE ANALYSIS BETWEEN SEX ## ----
# refer to help("proximity")

# combining the fitted models for both sites so you don't have to do the proximity looping test twice 
FIT.ALL <- c(FIT.1, FIT.2)

# create empty columns for where the result information will be added/filled into
pairwise.df$proximity_low <- NA
pairwise.df$proximity_est <- NA
pairwise.df$proximity_high <- NA

# this will take a while, days to loop, if R crashes, change the # for the loop number it was on. syntax: for(i in #:nrow(pairwise.df)) 
# Using tryCatch
for(i in 1:nrow(pairwise.df)){
  ANIMAL_A <- as.character(pairwise.df[i, 'anteater_A']) # add as.character due to tibble format
  ANIMAL_B <- as.character(pairwise.df[i, 'anteater_B'])
  TRACKING_DATA <- anteater.tel[c(ANIMAL_A, ANIMAL_B)] # extract anteater by name, has extra layers .-. it doesnt work, that is why
  # line above is using as.character removes all the fluff because you just want the text string
  MODELS <- list(FITS.ALL[ANIMAL_A][[1]], FITS.ALL[ANIMAL_B][[1]])
  PROXIMITY <- tryCatch(
    {
      PROXIMITY <- proximity(data = TRACKING_DATA, CTMM = MODELS , GUESS=ctmm(error=FALSE))},
    error=function(err){
      PROXIMITY <- c(NA,NA,NA)
      return(PROXIMITY)
    }
  )
  pairwise.df[i, c("proximity_low")] <- PROXIMITY[1]
  pairwise.df[i, c("proximity_est")] <- PROXIMITY[2]
  pairwise.df[i, c("proximity_high")] <- PROXIMITY[3]
  write.csv(pairwise.df, "C:\Users\achhen.stu\OneDrive - UBC\BIOL 452 Directed Studies - Giant Anteaters\Anteater Scripts\R working directory/proximity.csv", row.names = FALSE)
}

# Load Proximity Analysis results
proximity.data <- read.csv("C:\Users\achhen.stu\OneDrive - UBC\BIOL 452 Directed Studies - Giant Anteaters\Anteater Scripts\R working directory/proximity.csv")

# Plot Proximity Analysis between sex ----
proximity.FIG <- 
  ggplot(data = proximity.data, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 0.5) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), size = 0.3) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#d1495b", "#009E73", "#0072B2"),
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
        legend.position = c(0.8, 0.8),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave(proximity.FIG,
       width = 3.23, height = 2, units = "in",
       dpi = 600,
       bg = "transparent",
       file="Anteater_Proximity_final.png")

### PROXIMITY ANALYSIS ----
# clean up data because there are duplicate values
proximity.data <- proximity.data[!duplicated(proximity.data[,c(1,2)]),]

# IDENTIFY PAIRS THAT WERE CLOSER/FURTHER TO EACH OTHER ----
# To do this, subset proximity analysis results for proximity values above and below 1
# These individuals displayed distances that were further or closer (.-. above or below 1)
proximity.above1 <- proximity.data[which(proximity.data$proximity_low > 1),]
proximity.below1 <- proximity.data[which(proximity.data$proximity_high < 1),]

# FIND THE TIME STAMPS OF PAIR INDIVIDUALS
# so they align/match up in time to figure out where they were in space
# general syntax: seq(from, to, by, length.out, along.with)
# seq(t1, t2, by = 20 minutes)
anteater.time <- seq(from=as.POSIXct("2017-07-05"), to=as.POSIXct("2019-09-05"), by='20 mins')

# PREDICT THE LOCATION OF THE PAIR INDIVIDUALS
prediction <- predict(anteater.tel[[1]], CTMM=FITS.ALL[[1]], t = anteater.time)
# [[#]] indicates the animal number




