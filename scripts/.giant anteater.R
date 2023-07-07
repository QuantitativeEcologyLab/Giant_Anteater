

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
#analysis
library(devtools)
devtools::install_github("ctmm-initiative/ctmm", force = TRUE) #if package needs to be updated
#devtools::install_github("jmcalabrese/corrMove", force = TRUE) #if installing for the first time
library(ctmm)            #continuous-time movement models
library(lme4)            #pairwise sex test to see if differences are significant using glmer()
library(glmmTMB)         #beta distribution
library(corrMove)        #correlative movement

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant anteater")

#............................................................
# Data ----
#............................................................

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
#calculate mean home range size
round(mean(HR_size$HR_low), 2)
round(mean(HR_size$HR_est), 2)
round(mean(HR_size$HR_high), 2)

#calculate home-range size & compare sex
AKDE_male <- AKDE[c("Alexander", "Anthony", "Beto","Christoffer","Jackson",
                    "Kyle", "Larry", "Little_Rick", "Luigi", "Reid", 
                    "Rodolfo", "Thomas")]
AKDE_female <- AKDE[c("Annie", "Bumpus", "Cate", "Elaine", "Hannah",
                      "Jane","Makao", "Margaret", "Maria", "Puji",
                      "Sheron")]

#calculate mean home-range sizes for male
meta(AKDE_male)

#calculate mean home-range sizes for male
meta(AKDE_female)

#test to see significance of sex on home-range
AKDE_sex_compare <- list(male = AKDE_male,
                         female = AKDE_female)
COL_sex <- c("#004488", "#A50026")
meta(AKDE_sex_compare, col = COL_sex, sort = TRUE)

#mean home range size
round(mean(HR_size$HR_low), 2) #4.55 km^2
round(mean(HR_size$HR_est), 2) #5.45 km^2
round(mean(HR_size$HR_high), 2) #6.56 km^2

#............................................................
## Home range overlap ----
#............................................................

overlap_1_df <- readRDS("RDS/overlap_1_df.RDS")
overlap_2_df <- readRDS("RDS/overlap_2_df.RDS")
overlap_df  <- readRDS("RDS/overlap_df.RDS")

overlap_df$pair_ID <- paste(overlap_df$anteater_A, overlap_df$anteater_B, sep = "_")
overlap_df <- relocate(overlap_df, pair_ID, .before = anteater_A)

#............................................................
### Home range overlap results ----
#............................................................

# Total home range overlap
#calculate mean total home range overlap & range
round(mean(overlap_df$overlap_est), 2)
round(min(overlap_df$overlap_est), 2)
round(max(overlap_df$overlap_est), 2)

#number of home range overlap sex types
table(overlap_df$sex_comparison)
16+49+28 #93 dyads with overlaps

# Home range overlap based on sex comparison categories
#calculate mean home range overlap & range based on sex comparison categories
round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "male-male"]), 2)

round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "female-female"]), 2)

round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "male-female"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "male-female"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "male-female"]), 2)


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

#............................................................
# Interactions ----
#............................................................

## Proximity ratio data ----
#add proximity ratio data to home-range overlap dataframe
proximity_1_df <- readRDS("RDS/proximity_1_df.RDS")
proximity_2_df <- readRDS("RDS/proximity_2_df.RDS")
proximity_df <- readRDS("RDS/proximity_df.RDS")

overlap_df <- left_join(overlap_df, proximity_df, by = c("anteater_A", "anteater_B",
                                                         "Sex.A", "Sex.B",
                                                         "Age.A", "Age.B",
                                                         "sex_comparison",
                                                         "site"))

### Proximity ratio sex analysis ----
#test for significance in sex, compare model with and without sex as a variable
proximity_test <- glmer(proximity_est ~ sex_comparison + (1|site), family = Gamma(link = "log"), data = overlap_df)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = overlap_df)
proximity_test_results <- anova(proximity_test, proximity_test2)
proximity_test_pvalue <- round(proximity_test_results$`Pr(>Chisq)`[2], 2)
#p = 0.13

#............................................................
## Distances ----
#............................................................

distance_df <- readRDS("RDS/distance_df.RDS")

#check for NA values
any(is.na(distance_df))
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

#calculate the number of encounters based on threshold
sum(distance_df$distance_est < 15)
sum(distance_df$distance_est[distance_df$sex_comparison == "male-male" & distance_df$distance_est] < 15)
sum(distance_df$distance_est[distance_df$sex_comparison == "female-female" & distance_df$distance_est] < 15)
sum(distance_df$distance_est[distance_df$sex_comparison == "male-female" & distance_df$distance_est] < 15)

#calculate total encounters of all individuals based on sex comparison type
overlap_df$encounter_count <- NA
unique_pairs <- unique(overlap_df$pair_ID)
for (i in unique_pairs){
  subset_A <- distance_df[distance_df$pair_ID == i,]
  
  # Count the number of times "distance_est" is below 15
  encounter_count <- sum(subset_A$distance_est < 15)
  
  #save results
  overlap_df[overlap_df$pair_ID == i, "encounter_count"] <- encounter_count
  
}

#check for NA values
any(is.na(overlap_df))

## Encounter sex analysis ----
#effect of sex and overlap on encounter rates
encounter_test <- glmer(encounter_count ~ overlap_est + sex_comparison + (1|site), family = poisson(link = "log"), data = overlap_df)
encounter_test2 <- glmer(encounter_count ~ 1 + (1|site), family = poisson(link = "log"), data = overlap_df)
encounter_test_results <- anova(encounter_test, encounter_test2)
encounter_test_pvalue <- round(encounter_test_results$`Pr(>Chisq)`[2], 2)
encounter_test_pvalue

encounter_test_sex <- glmer(encounter_count ~ sex_comparison + (1|site), family = poisson(link = "log"), data = overlap_df)

#model that does not include 0 encounter counts
encounter_test_nozero <- glmer(encounter_count ~ overlap_est + sex_comparison + (1|site), family = poisson(link = "log"), data = overlap_df, subset = encounter_count > 0)

# amount of home-range overlap and the number of observed encounters (β = 4.86 ± 0.148, p = 0.00)
#standard error * 1.96 = CI
0.07568 * 1.96

#............................................................
## Proximity ratio identified pairs ----
#............................................................

proximity_identified_pairs_df <- readRDS("RDS/proximity_identified_pairs_df.RDS")
#Identify pairs that did not have a proximity ratio of 1 based on sex comparison category
table(proximity_identified_pairs_df$sex_comparison)

### Distances of identified pairs ----
#Calculate the instantaneous Euclidean distance between the individuals in the dyad using telemetry data
distance_pair1 <- readRDS("RDS/distance_pair1.RDS")
distance_pair2 <- readRDS("RDS/distance_pair2.RDS")
distance_pair3 <- readRDS("RDS/distance_pair3.RDS")
distance_pair4 <- readRDS("RDS/distance_pair4.RDS")
distance_pair5 <- readRDS("RDS/distance_pair5.RDS")
distance_pair6 <- readRDS("RDS/distance_pair6.RDS")
distance_pair7 <- readRDS("RDS/distance_pair7.RDS")
distance_pair8 <- readRDS("RDS/distance_pair8.RDS")
distance_pair9 <- readRDS("RDS/distance_pair9.RDS")
distance_pair10 <- readRDS("RDS/distance_pair10.RDS")
distance_pair11 <- readRDS("RDS/distance_pair11.RDS")
distance_pair12 <- readRDS("RDS/distance_pair12.RDS")
distance_pair_df <- readRDS("RDS/distance_pair_df.RDS")

### Encounters of identified pairs ----
#calculate the number of encounters based on a distance threshold of 15meters (threshold obtained from the sensitivity analysis)
proximity_identified_pairs_df$encounter_count <- NA
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "1"] <- sum(distance_pair1$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "2"] <- sum(distance_pair2$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "3"] <- sum(distance_pair3$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "4"] <- sum(distance_pair4$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "5"] <- sum(distance_pair5$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "6"] <- sum(distance_pair6$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "7"] <- sum(distance_pair7$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "8"] <- sum(distance_pair8$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "9"] <- sum(distance_pair9$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "10"] <- sum(distance_pair10$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "11"] <- sum(distance_pair11$est < 15)
proximity_identified_pairs_df$encounter_count[proximity_identified_pairs_df$pair_ID_number == "12"] <- sum(distance_pair12$est < 15)

# Correlative Movement ----
cm_pair1 <- readRDS("RDS/cmAnteater_pair1.RDS")
cm_pair2 <- readRDS("RDS/cmAnteater_pair2.RDS")
cm_pair3 <- readRDS("RDS/cmAnteater_pair3.RDS")
cm_pair4 <- readRDS("RDS/cmAnteater_pair4.RDS")
cm_pair5 <- readRDS("RDS/cmAnteater_pair5.RDS")
cm_pair6 <- readRDS("RDS/cmAnteater_pair6.RDS")
cm_pair7 <- readRDS("RDS/cmAnteater_pair7.RDS")
cm_pair8 <- readRDS("RDS/cmAnteater_pair8.RDS")
cm_pair9 <- readRDS("RDS/cmAnteater_pair9.RDS")
cm_pair10 <- readRDS("RDS/cmAnteater_pair10.RDS")
cm_pair11 <- readRDS("RDS/cmAnteater_pair11.RDS")
cm_pair12 <- readRDS("RDS/cmAnteater_pair12.RDS")

# Case study of Pair 11 ----
#home-range size
round(overlap_df$overlap_low[overlap_df$pair_ID == 11], 2)
round(overlap_df$overlap_est[overlap_df$pair_ID == 11], 2)
round(overlap_df$overlap_high[overlap_df$pair_ID == 11], 2)

#proximity ratio
round(proximity_identified_pairs_df$proximity_low[proximity_identified_pairs_df$pair_ID == 11], 2)
round(proximity_identified_pairs_df$proximity_est[proximity_identified_pairs_df$pair_ID == 11], 2)
round(proximity_identified_pairs_df$proximity_high[proximity_identified_pairs_df$pair_ID == 11], 2)

#distance measurement
round(mean(distance_pair11$low), 2)
round(mean(distance_pair11$est), 2)
round(mean(distance_pair11$high), 2)

#mean correlative movement
round(mean(cm_pair11$etaTot.CI.Low), 2)
round(mean(cm_pair11$etaTot.MLE), 2)
round(mean(cm_pair11$etaTot.CI.Upp), 2)

# Plot ----
## Plot study site ----

#Brazil with meso regions outlined
BR_meso_region <- read_meso_region(code_meso = "all", year = 2017, simplified = TRUE, showProgress = TRUE)

#state of Mato Grosso du Sul
MS_state_micro <- read_micro_region(code_micro= "MS", year = 2017, simplified = FALSE, showProgress = TRUE)

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

plot_BR <-
  ggplot() +
  geom_sf(data=BR_meso_region, fill="white", color="black", size=.15, show.legend = FALSE) +
  labs(subtitle="Brazil", size=8) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") +
  no_axis
ggsave(plot = last_plot(), filename = "figures/BR_meso_region.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

plot_MS <-
  ggplot() +
  geom_sf(data=MS_state_micro, fill="white", color="black", size=.15, show.legend = FALSE) +
  labs(subtitle="Mato Grosso du Sul", size=8) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") +
  no_axis
ggsave(plot = last_plot(), filename = "figures/MS_state_micro.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

plot_BR_MS <- grid.arrange(plot_BR, plot_MS, 
                           ncol=2, widths = c(6,5.2))
# Save the figure
ggsave(plot_BR_MS,
       width = 10, height = 5, units = "in",
       dpi = 600,
       bg = "white",
       file="figures/plot_BR_MS.png")

## Plot HR size ----
mean_HR_est <- round(mean(HR_size$HR_est), 2)
plot_HR_size <- 
  ggplot() +
  geom_vline(data = HR_size, aes(xintercept = mean_HR_est),
             linetype = "dotdash") +
  geom_linerange(data = HR_size, 
                 aes(xmin = HR_low, xmax = HR_high, y = ID, color = Sex),
                 linewidth = 3) + 
  labs(x = bquote("Home range area" ~ (km^2)),
       y = "") +
  ggtitle("A)") +
  scale_color_manual(values = c('#004488', '#A50026'), breaks = c('Male', 'Female')) +
  geom_point(data = HR_size,
             aes(x = HR_est, y = ID, fill = "white"), color = "white",
             size = 2) +
  geom_point(data = HR_size, 
             aes(x = HR_est, y = ID, color = Sex),
             size = 1) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")
plot_HR_size
ggsave(plot_HR_size, filename = "figures/HR_size.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 4, units = "in", dpi = 600)

## Plot HR overlap ----
AKDE_1 <- AKDE[c("Alexander", "Anthony", "Bumpus", "Cate", "Christoffer",
                 "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao",
                 "Puji", "Rodolfo")]
AKDE_2 <- AKDE[c("Annie", "Beto", "Hannah", "Jane", "Larry",
                 "Luigi", "Margaret", "Maria", "Reid", "Sheron",
                 "Thomas")]

COL_1 <- c("#004488", "#004488", "#A50026", "#A50026", "#004488", "#A50026", "#004488", "#004488", "#004488", "#A50026", "#A50026", "#004488") 
png(file = "figures/HRO_site1.png", width = 6.86, height = 6, units = "in", res = 600)
plot_HRO_site1 <- 
  plot(AKDE_1, col.DF = COL_1, col.level = COL_1, col.grid = NA, level = NA)
title("B)", adj = 0)
dev.off()

COL_2 <- c("#A50026", "#004488", "#A50026", "#A50026", "#004488", "#004488", "#A50026", "#A50026", "#004488", "#A50026", "#004488") 
png(file = "figures/HRO_site2.png", width = 6.86, height = 6, units = "in", res = 600)
plot_HRO_site2 <- 
  plot(AKDE_2, col.DF = COL_2, col.level = COL_2, col.grid = NA, level = NA) 
title("C)", adj = 0)
dev.off()

png(file = "figures/HR_overlap.png", width = 12, height = 6, units = "in", res = 600)
par(mfrow=c(1,2))
plot(AKDE_1, col.DF = COL_1, col.level = COL_1, col.grid = NA, level = NA)
title("B)", adj = 0)
plot(AKDE_2, col.DF = COL_2, col.level = COL_2, col.grid = NA, level = NA)
title("C)", adj = 0)
dev.off()

## Plot HRO sex comparison ----
plot_HRO_sex_analysis <-
  ggplot(data = overlap_df, 
         mapping = aes(x = sex_comparison, y = overlap_est, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Home range overlap") +
  xlab("Sex") +
  ggtitle("D)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position="none") +
  scale_fill_manual(values = c("#A50026", "#9970AB", "#004488"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))
plot_HRO_sex_analysis
ggsave(plot_HRO_sex_analysis, filename = "figures/HRO_sex_analysis.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 3, units = "in", dpi = 600)

## Plot multi-panel ----
library(cowplot)

figure1_HR <- grid.arrange(plot_HR_size,
                        plot_grid(plot_HRO_site1, plot_HRO_site2, ncol = 2),
                        plot_HRO_sex_analysis,
                        nrow = 3)

ggsave(figure1_HR, filename = "figures/figure1_HR.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 12, units = "in", dpi = 600)

## Plot Proximity ratio  ----
plot_proximity_ratio <- 
  ggplot() +
  geom_hline(data = overlap_df, 
             aes(y = proximity_est, x = overlap_est, col = sex_comparison),
             yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(data = overlap_df, 
             aes(y = proximity_est, x = overlap_est, col = sex_comparison),
             size = 1.5, alpha = 0.3) + #alpha = colour intensity
  geom_segment(data = overlap_df, 
               aes(x = overlap_est, xend = overlap_est, y = proximity_low, yend = proximity_high, col = sex_comparison), 
               linewidth = 0.3, alpha = 0.3) +
  geom_point(data = proximity_pair_df, 
             aes(y = proximity_est, x = overlap_est, col = sex_comparison),
             size = 1.5) +
  geom_segment(data = proximity_pair_df,
               aes(x = overlap_est, xend = overlap_est, y = proximity_low, yend = proximity_high, col = sex_comparison), 
               linewidth = 0.3) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home-range overlap") +
  ggtitle("A)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.85, 0.2),
        legend.key.height = unit(0.3, "cm"),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
plot_proximity_ratio
ggsave(plot_proximity_ratio, width = 6.23,height = 4, units = "in", dpi = 600, bg = "transparent",
       file="figures/proximity_ratio.png")

## Plot encounters ----
#plot_encounters <-
  ggplot(data = overlap_df,
         aes(y = encounter_count, x = overlap_est)) +
  geom_point(data = overlap_df, 
             aes(y = encounter_count, x = overlap_est, col = sex_comparison),
             size = 1.5) + 
  geom_smooth(method="lm", formula = y ~ x, se=F, col = "black") +
  scale_y_log10(expand = c(0,0.1), limits = c(0.1,2000)) +
  scale_x_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  xlab("Home-range overlap") +
  ylab("Encounters count") +
  ggtitle("B)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.1, 0.85),
        legend.key.height = unit(0.3, "cm"),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(plot = last_plot(), width = 6.86,height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/encounters.png")

overlap_df2 <- overlap_df[overlap_df$encounter_count != 0,] #messed up entries with NA values
#plot_encounters_nozeros <-
  ggplot(data = overlap_df2,
         aes(y = encounter_count, x = overlap_est)) +
  geom_point(data = overlap_df2, 
             aes(y = encounter_count, x = overlap_est, col = sex_comparison),
             size = 1.5) + 
  geom_smooth(method="lm", formula = y ~ x, se=F, col = "black") +
  scale_y_log10(expand = c(0,0.1), limits = c(0.1,2000)) +
  scale_x_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  xlab("Home-range overlap") +
  ylab("Encounters count") +
  ggtitle("B)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.1, 0.85),
        legend.key.height = unit(0.3, "cm"),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(plot = last_plot(), width = 6.86,height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/encounters_nozeros.png")

## Multi-panel
FIG_proximityratio_encounters <- grid.arrange(plot_proximity_ratio,
                                              plot_encounters, 
                                              nrow = 2)
ggsave(FIG_proximityratio_encounters, filename = "figures/proximityratio_encounters.png", 
       device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

FIG_proximityratio_encounters2 <- grid.arrange(plot_proximity_ratio,
                                              plot_encounters_nozeros, 
                                              nrow = 2)
ggsave(FIG_proximityratio_encounters2, filename = "figures/proximityratio_encounters2.png", 
       device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## Plot encounter residuals ----
# FIT_ENC <- lm(log10(encounter_count+0.1) ~ log10(overlap_est), data = proximity_pair_df)
# resids <- residuals(FIT_ENC)
# png(file = "figures/encounter_residuals_sex.png", width = 6.86, height = 6, units = "in", res = 600)
# boxplot(resids ~ proximity_pair_df$sex_comparison,
#         xlab = "Encounter Residuals",
#         ylab = "sex")
# dev.off()

## Plot encounters of 12 dyads ----
#plot_encounters_pair <-
ggplot(data = proximity_pair_df,
       aes(y = encounter_count, x = overlap_est)) +
  geom_point(data = proximity_pair_df, 
             aes(y = encounter_count, x = overlap_est, col = sex_comparison),
             size = 1.5) + 
  geom_smooth(method="lm", formula = y ~ x, se=F, col = "black") +
  scale_y_log10(expand = c(0,0.1), limits = c(0.1,2000)) +
  scale_x_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  xlab("Home-range overlap") +
  ylab("Encounters count of 12 dyads") +
  ggtitle("B)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.1, 0.85),
        legend.key.height = unit(0.3, "cm"),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(plot = last_plot(), width = 6.86,height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/PAIR_encounters.png")



## Plot correlative movement of the 12 dyads ----
## Plot CM ----
#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair1.png", width = 6.86, height = 6.86, units = "in", res = 600)
plot.corrMove(cm_pair1)
title("Pair 1: Kyle/Christoffer")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair2.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair2)
title("PAIR 2: Elaine/Christoffer")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair3.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair3)
title("PAIR 3: Bumpus/Kyle")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair4.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair4)
title("PAIR 4: Little Rick/Elaine")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair5.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair5)
title("PAIR 5: Makao/Bumpus")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair6.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair6)
title("PAIR 6: Puji/Bumpus")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair8.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair8)
title("PAIR 8: Larry/Annie")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair9.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair9)
title("PAIR 9: Reid/Larry")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair10.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair10)
title("PAIR 10: Sheron/Maria")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair11.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair11)
title("PAIR 11: Thomas/Margaret")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative_movement/corrmove_pair12.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cm_pair12)
title("PAIR 12: Thomas/Reid")
dev.off()

## Plot Pair 11 ----
AKDE_pair11 <- AKDE[c("Thomas", "Margaret")]
COL_pair11 <- c("#004488", "#A50026")

### home-range overlap with GPS points  ----
png(file = "figures/HRO_pair11.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_pair11, col.DF = COL_pair11, col.level = COL_pair11, col.grid = NA, level = NA,
     main = "Thomas and Margaret")
title("A)", adj = 0)
plot(list(Thomas, Margaret), error = FALSE, #arguments need to be before colour or won't work
     col = c("#004488", "#A50026"),
     add = TRUE) #to overlay the previous plot
dev.off()

### distances ----
png(file = "figures/distance_pair11.png", width = 6.86, height = 4, units = "in", res = 600)
plot_distances_pair11 <- plot(est~timestamp, 
                              data=distance_pair11, 
                              type="l", # type="l" changes the plot from dots to a line
                              xlab = "",
                              ylab = "distance (m)")
# cex.axis = 1.5, #size of axis font (tick values)
# cex.lab = 2, #size of axis label
# font.lab = 2) #bold axis labels
ylim = c(0,9000)
title("B)", adj = 0)
dev.off()

### correlative movement
png(file = "figures/corrmove_pair11.png", width = 6.86, height = 6, units = "in", res = 600)
plot_cm_pair11 <-
  plot.corrMove(cm_pair11,
                cex.axis = 1.5, #size of axis font (tick values)
                cex.lab = 2) #size of axis label))
title("C)", adj = 0)
dev.off()

plot_pair11 <- grid.arrange(plot_distances_pair11, 
                            plot_cm_pair11,
                            ncol=1)
