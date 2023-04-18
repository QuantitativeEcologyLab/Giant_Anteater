### GIANT ANTEATER FULL CODE (includes notes) ###

# Using github packages that are not on CRAN i.e. cannot be used via install.packages(), check for updates
devtools::install_github("r-lib/devtools", force = TRUE)
devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
devtools::install_github("jmcalabrese/corrMove", force = TRUE)
devtools::install_github("16EAGLE/moveVis") # development version

### DATA PREPARATION ----

## TELEMETRY FORMAT
# Convert dataset to a telemetry object, assuming the data has been cleaned and containing no outliers
#DATA <- as.telemetry(anteater.DATA) # n=19
#Error in strptime(xx, f, tz = tz) : input string is too long .-. used code below
DATA <- as.telemetry("data/Anteaters_NoOutliers (original data).csv")

## TEMPERATURE DATA
TEMPDATA <- read_csv("data/anteater_annotated.csv")
names(TEMPDATA)[19] <- "temperature"

###################################################
#### ALTERNATE CODE FOR SAVING FIT MODELS RDS #####
# using if/else to fit models & to include save and load RDS file

# SITE 1 - ALL INDIVIDUALS (n=12)
GUESS.1 <- lapply(site.1[1:12], function(b) ctmm.guess(b,interactive=FALSE) ) #shouldn't take too long to run, may not be necessary to save as RDS
if(!file.exists("RDS/FIT.1.RDS"))  {# ! = not
  FIT.1 <- lapply(1:12, function(i) ctmm.select(site.1[[i]],GUESS.1[[i]]) )
  names(FIT.1) <- names(site.1[1:12])
  saveRDS(object = FIT.1, file = "RDS/FIT.1.RDS")
} else {# to import/load RDS file, it will appear in the console .-. need to load
  FIT.1 <- readRDS("RDS/FIT.1.RDS")
}
overlap(FIT.1)

# SITE 2 - ALL INDIVIDUALS (n=7)
GUESS.2 <- lapply(site.2[1:7], function(b) ctmm.guess(b,interactive=FALSE) ) #shouldn't take too long to run, may not be necessary to save as RDS
if(!file.exists("FIT.2.RDS"))  {# ! = not
  FIT.2 <- lapply(1:7, function(i) ctmm.select(site.2[[i]],GUESS.2[[i]]) )
  names(FIT.2) <- names(site.2[1:7])
  saveRDS(object = FIT.2, file = "RDS/FIT.2.RDS")
} else {# to import/load RDS file, it will appear in the console .-. need to load
  FIT.2 <- readRDS("RDS/FIT.2.RDS")
}
overlap(FIT.2)
###################################################

### PAIRWISE ANALYSIS SEX COMPARISON ----
# creating the data frame from overlap values

## PAIRWISE ANALYSIS SEX COMPARISON FOR SITE 1 ----
# SITE 1 - taking the overlap cube (array), extracting median layer, removing one portion of the triangle
overlap.1 <- overlap(object = AKDE.1, level = 0.95) # assigns the overlap results as an object
overlap.1.median <- overlap.1$CI[ , , 2] # extract the median layer (pulling out the section) of the cube (aka array)
# [, , ,] = [row, column, layer] of the cube/array
# values of layers: 1 = low, 2 = median (50%), 3 = high
overlap.1.median[upper.tri(overlap.1.median, diag = TRUE)] <- NA # removing the upper triangle & diagonal of the matrix the triangle are duplicate values 
# because its a symmetric matrix and removing the diagonal because it is the value of home range of itself, .-. we change upper and diag to NA, 
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
DATA.pairwise.1 <- mutate(pairwise.1.pivot.B, sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                                         paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                                         paste(Sex.A, Sex.B) == "Male Female" ~ "male-female",
                                                                         paste(Sex.A, Sex.B) == "Female Male" ~ "male-female"))
# adding column to indicate which sexes that are being compared

#### PROXIMITY METRIC MEASUREMENTS OF IDENTIFIED PAIRS ----
# distances() function further estimates the instantaneous distances between individuals
# Use telemetry data

#################### REQUIRES ANOTHER WORKFLOW/WORKAROUND -----
# Create table that has 3 columns to compare time and space ie. at Time x where was anteater_A and anteater_B
# Column 1: Time that spans first GPS time point to last GPS time point
# Column 2: anteater_A location ie. GPS coordinate
# Column 3: anteater_B location ie. GPS coordinate
# refer to whiteboard photo for this visualization

# FIND THE TIME STAMPS OF PAIR INDIVIDUALS (ie. Column 1: Time)
# so they align/match up in time to figure out where they were in space
# PREDICT THE LOCATION OF THE PAIR INDIVIDUALS (filling in Column 2 and 3)

# seq(t1, t2, by = 20 minutes)

# Create time column
anteater.time <- seq(from=as.POSIXct("2017-07-05"), to=as.POSIXct("2019-09-05"), by='20 mins')

# PREDICT THE LOCATION OF THE PAIR INDIVIDUALS (filling in Column 2 and 3)
predict.Alexander <- predict(DATA[[1]], CTMM=FIT.1[[1]], t = anteater.time)
predict.Bumpus <- predict(DATA[[5]], CTMM=FIT.1[[3]], t = anteater.time)
predict.Christoffer <- predict(DATA[[7]], CTMM=FIT.1[[7]], t = anteater.time)
predict.Elaine <- predict(DATA[[8]], CTMM=FIT.ALL[[8]], t = anteater.time)
predict.Kyle <- predict(DATA[[11]], CTMM=FIT.ALL[[11]], t = anteater.time)
predict.LittleRick <- predict(DATA[[13]], CTMM=FIT.ALL[[13]], t = anteater.time)
predict.Makao <- predict(DATA[[14]], CTMM=FIT.ALL[[14]], t = anteater.time)
predict.Puji <- predict(DATA[[16]], CTMM=FIT.ALL[[16]], t = anteater.time)
predict.Rodolfo <- predict(DATA[[18]], CTMM=FIT.ALL[[18]], t = anteater.time)
# [[#]] indicates the animal number

#findPrts() #Error with -> Estimate the partition points for the anteater data
#Error with dICold > dICnew
# getIC() function not working -> from CompR -> CompR package installed
# Error related to duplicate timestamps?
##########################

# 'DATA' needs to contain the pair, 2 individuals being compared
######## GENERAL SYNTAX WORKFLOW VIA NOONAN ----
metric <- distances(DATA,FIT)
metric # distance measurement and time between the pair
names(metric)
plot(log(est)~timestamp, data=metric, type="l") # type="l" changes the plot from dots to a line

################## WORKFLOW VIA NOONAN ----
#Extract some test individuals and do some data carpentry
Elaine <- DATA$Elaine #data$Elaine
Christoffer <- DATA$Christoffer # data$Christoffer
plot(list(Elaine, Christoffer), col = c("red", "blue"))
El <- data.frame(timestamp = round_date(Elaine$timestamp, "20 minutes") ,
                 E.x = Elaine$longitude,
                 E.y = Elaine$latitude)
Chris <- data.frame(timestamp = round_date(Christoffer$timestamp, "20 minutes"),
                    C.x = Christoffer$longitude,
                    C.y = Christoffer$latitude)
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
###########################



### TEMPERATURE ANALYSIS ####
#see Dr. Noonan


#### MOVEVIS FOR VISUALIZATION OF MOVEMENT DATA #### ----
# https://movevis.org/

#R package workflow
data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

# align move_data to a uniform time scale
m <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "mapbox", 
                         map_type = "satellite", 
                         map_token = "sk.eyJ1Ijoia2F0Y2hoZW4iLCJhIjoiY2xlM25lemwyMDNybDNvbzY4MmEybXl3MSJ9.pEfF_kZ-qZDY-RzV8xh8Kg", 
                         alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customization, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "moveVis.gif")


















