
# load packages ----
library(readr)
library(ggplot2)
library(khroma)          #colour blind friendly colour palette
library(ctmm)            #continuous time movement models
library(corrMove)        #correlative movement


# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant-anteater")

# Import dataset
anteater_DATA <- read_csv("data/Anteaters_NoOutliers (original data).csv", col_types = cols(timestamp = "c", class = "c", identity = "c", id = "c", .default = "d"))

# Convert dataset to a telemetry object
DATA <- as.telemetry("data/Anteaters_NoOutliers (original data).csv")

# load RDS files ----
#movement models
FIT_1 <- readRDS("RDS/movement model/FIT_1.RDS")
FIT_1_male <- readRDS("RDS/movement model/FIT_1_male.RDS")
FIT_1_male_adult <- readRDS("RDS/movement model/FIT_1_male_adult.RDS")
FIT_1_female <- readRDS("RDS/movement model/FIT_1_female.RDS")
FIT_2 <- readRDS("RDS/movement model/FIT_2.RDS")
FIT_2_male <- readRDS("RDS/movement model/FIT_2_male.RDS")
FIT_2_male_adult <- readRDS("RDS/movement model/FIT_2_male_adult.RDS")
FIT_2_female <- readRDS("RDS/movement model/FIT_2_female.RDS")
#AKDE overlap
AKDE_1 <- readRDS("RDS/movement model/AKDE_1.RDS")
AKDE_1_male <- readRDS("RDS/movement model/AKDE_1_male.RDS")
AKDE_1_male_adult <- readRDS("RDS/movement model/AKDE_1_male_adult.RDS")
AKDE_1_female <- readRDS("RDS/movement model/AKDE_1_female.RDS")
AKDE_2 <- readRDS("RDS/movement model/AKDE_2.RDS")
AKDE_2_male <- readRDS("RDS/movement model/AKDE_2_male.RDS")
AKDE_2_male_adult <- readRDS("RDS/movement model/AKDE_2_male_adult.RDS")
AKDE_2_female <- readRDS("RDS/movement model/AKDE_2_female.RDS")
AKDE_pair1 <- AKDE_1[c(5,8)]
AKDE_pair2 <- AKDE_1[c(5,6)]
AKDE_pair3 <- AKDE_1[c(3,8)]
AKDE_pair4 <- AKDE_1[c(6,9)]
AKDE_pair5 <- AKDE_1[c(3,10)]
AKDE_pair6 <- AKDE_1[c(3,11)]
AKDE_pair7 <- AKDE_1[c(6,12)]
AKDE_pair8 <- AKDE_2[c(1,5)]
AKDE_pair9 <- AKDE_2[c(5,9)]
AKDE_pair10 <- AKDE_2[c(7,11)]
AKDE_pair11 <- AKDE_2[c(9,11)]
AKDE_pair12 <- AKDE_2[c(8,10)]

#pairwise dataframe
DATA_pairwise_1 <- readRDS("RDS/pairwise/DATA_pairwise_1.RDS")
DATA_pairwise_1_adult <- readRDS("RDS/pairwise/DATA_pairwise_1_adult.RDS")
DATA_pairwise_2 <- readRDS("RDS/pairwise/DATA_pairwise_2.RDS")
DATA_pairwise_2_adult <- readRDS("RDS/pairwise/DATA_pairwise_2_adult.RDS")
DATA_pairwise <- readRDS("RDS/pairwise/DATA_pairwise.RDS")
DATA_pairwise_adult <- readRDS("RDS/pairwise/DATA_pairwise_adult.RDS")

#proximity analysis
DATA_proximity_1 <- readRDS("RDS/proximity/DATA_proximity_1.RDS")
DATA_proximity_2 <- readRDS("RDS/proximity/DATA_proximity_2.RDS")
DATA_proximity <- readRDS("RDS/proximity/DATA_proximity.RDS")

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

#correlative movement
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


# Overlap ----
COL_1 <- c("#004488", "#004488", "#A50026", "#A50026", "#004488", "#A50026", "#004488", "#004488", "#004488", "#A50026", "#A50026", "#004488") 
COL_2 <- c("#A50026", "#004488", "#A50026", "#A50026", "#004488", "#004488", "#A50026", "#A50026", "#004488", "#A50026", "#004488") 
# blue = male; red = female
png(file = "figures/overlap/overlap.png", width = 6.86, height = 6, units = "in", res = 600)
par(mfrow = c(1,2))
plot(AKDE_1, col.DF = COL_1, col.level = COL_1, col.grid = NA, level = NA)
#title("A)", adj = 0)
title("Site 1")
plot(AKDE_2, col.DF = COL_2, col.level = COL_2, col.grid = NA, level = NA) 
#title("B)", adj = 0)
title("Site 2")
dev.off()
par(mfrow = c(1,1))

##site 1 ----
COL_1 <- c("#004488", "#004488", "#A50026", "#A50026", "#004488", "#A50026", "#004488", "#004488", "#004488", "#A50026", "#A50026", "#004488") 
# blue = male; red = female
png(file = "figures/overlap/overlap_1.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_1, col.DF = COL_1, col.level = COL_1, col.grid = NA, level = NA)
title("AKDE Overlap (Site 1)")
dev.off()

png(file = "figures/overlap/overlap_1_male.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_1_male, col.DF = "dodgerblue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: male)")
dev.off()

png(file = "figures/overlap/overlap_1_male_adult.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_1_male_adult, col.DF = "blue3", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: male adult)")
dev.off()

png(file = "figures/overlap/overlap_1_female.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_1_female, col.DF = "red", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 1: female)")
dev.off()

##site 2 ----
COL_2 <- c("red", "#004488", "red", "red", "#004488", "#004488", "red", "red", "#004488", "red", "#004488") 
# blue = male; red = female
png(file = "figures/overlap/Overlap_2.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_2, col.DF = COL_2, col.level = COL_2, col.grid = NA, level = NA) 
title("aKDE Overlap (Site 2: male and female)")
dev.off()

png(file = "figures/overlap/Overlap_2_male.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_2_male_adult, col.DF = "dodgerblue", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: male)")
dev.off()

png(file = "figures/overlap/Overlap_2_male_adult.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_2_male_adult, col.DF = "blue3", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: male adult)")
dev.off()

png(file = "figures/overlap/Overlap_2_female.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_2_female, col.DF = "red", col.level = "black", col.grid = NA, level = NA)
title("aKDE Overlap (Site 2: female)")
dev.off()

##poster version ----
COL_1_poster <- c("#004488", "#004488", "#A50026", "#A50026", "#004488", "#A50026", "#004488", "#004488", "#004488", "#A50026",
           "#A50026", "#004488") 
# blue = male; red = female
png(file = "figures/overlap/Overlap_1_poster.png", width = 14.91, height = 13.04, units = "in", res = 600)
par(mar=c(5,6,4.5,4)) #increasing space of left margin so part of the y-axis title isn't cut off (order: bottom, left, top, and right)
plot(AKDE_1, 
     col.DF = COL_1_poster, 
     col.level = COL_1_poster, 
     col.grid = NA, 
     level = NA,
     # font=2, #bold axis font (tick values)
     cex.axis = 2.75, #size of axis font (tick values)
     cex.lab = 3.5, #size of axis label
     font.lab = 2) #bold axis labels
dev.off()

## pairs poster version ----
#pair1
COL_pair1 <- c("#004488", "#4393c3")
png(file = "figures/overlap/overlap_pair1.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_pair1, 
     col.DF = COL_pair1, 
     col.level = COL_pair1, 
     lwd.level = 3, #line thickness
     col.grid = NA, 
     level = NA,
     cex.axis = 1.5, #size of axis font (tick values)
     cex.lab = 2, #size of axis label
     main = "Christoffer and Kyle")
dev.off()

#pair11
COL_pair11 <- c("#004488", "#4393c3")
png(file = "figures/overlap/overlap_pair11.png", width = 6.86, height = 6, units = "in", res = 600)
plot(AKDE_pair11, 
     col.DF = COL_pair11, 
     col.level = COL_pair11, 
     lwd.level = 3, 
     col.grid = NA, 
     level = NA,
     xlim = NULL,
     main = "Reid and Thomas")
dev.off()

# Pairwise ----
## site 1 ----
#plot pairwise comparison (male and female)
ggplot(data = DATA_pairwise_1, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 1: male and female)") +
  theme_bw() +
  scale_fill_manual(values = c("#d1495b", "purple", "#0072B2"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "figures/pairwise/pairwise_1.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## SITE 1 plot pairwise comparison (adult only)
ggplot(data = DATA_pairwise_1_adult, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 1: adult only)") +
  theme_bw() +
  scale_fill_manual(values = c("#d1495b", "purple", "#0072B2"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "figures/pairwise/pairwise_1_adult.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)


## site 2 ----
## pairwise comparison (male and female)
ggplot(data = DATA_pairwise_2, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 2: male and female)") +
  theme_bw() +
  scale_fill_manual(values = c("#d1495b", "purple", "#0072B2"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "figures/pairwise/pairwise_2.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

## pairwise comparison (adult only)
ggplot(data = DATA_pairwise_2_adult, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex Comparison") +
  ggtitle("Overlap pairwise comparison of sexes (Site 2: adult only)") +
  theme_bw() +
  scale_fill_manual(values = c("#d1495b", "purple", "#0072B2"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "figures/pairwise/pairwise_2_adult.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

##combined sites ----
ggplot(data = DATA_pairwise, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex") +
  ggtitle("Anteater overlap pairwise comparison of sexes") +
  theme_bw() +
  scale_fill_manual(values = c("#d1495b", "purple", "#0072B2"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "figures/pairwise/pairwise.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#combined Adult only
ggplot(data = DATA_pairwise_adult, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex") +
  ggtitle("Anteater overlap pairwise comparison of sexes (adult only)") +
  theme_bw() +
  scale_fill_manual(values = c("#d1495b", "purple", "#0072B2"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "figures/pairwise/pairwise_adult.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

##poster version ----
FIG_pairwise_poster <- ggplot(data = DATA_pairwise, mapping = aes(x = sex_comparison, y = overlap, fill = sex_comparison)) + 
  geom_boxplot() +
  ylab("Overlap") +
  xlab("Sex") +
  #  ggtitle("HR Overlap pairwise comparison of sex") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank(), #removes vertical gridlines
        plot.margin = unit(c(0.75,0.5,0.25,0.25), "in"),
        legend.position="none", #removes legend
        axis.title.y = element_text(size=35, family = "sans", face = "bold"),
        axis.title.x = element_text(size=35, family = "sans", face = "bold"),
        axis.text.y = element_text(size=30, family = "sans"),
        axis.text.x  = element_text(size=30, family = "sans")) + 
  scale_fill_manual(values = c("#A50026", "#9970AB", "#004488"),
                    labels = c("Female - Female", "Male - Female", "Male - Male"),
                    name = "") +
  scale_y_continuous(limits = c(0,1))
ggsave(FIG_pairwise_poster, 
       filename = "figures/pairwise/pairwise_poster.png", device = NULL,
       path = NULL, scale = 1, width = 14.91, height = 6.47, units = "in", dpi = 600)

# Proximity ----

## site 1 ----
FIG_proximity_1 <- 
  ggplot(data = DATA_proximity_1, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 0.5) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), linewidth = 0.3) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#d1495b", "purple", "#0072B2"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home range overlap (Site 1)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.8, 0.3),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
# original legend position = c(0.8,0.8)
ggsave(FIG_proximity_1,
       width = 3.23,height = 2, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/proximity/Proximity_site_1.png")

## site 2 ----
FIG_proximity_2 <- 
  ggplot(data = DATA_proximity_2, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 0.5) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), linewidth = 0.3) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Male - Female", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home range overlap (Site 2)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.8, 0.3),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
# original legend position = c(0.8,0.8)
ggsave(FIG_proximity_2,
       width = 3.23,height = 2, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/proximity/Proximity_site_2.png")

## combined ----
FIG_proximity <- ggplot(data = DATA_proximity, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 0.5) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), linewidth = 0.3) +
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
        legend.position = c(0.8, 0.3),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave(FIG_proximity,
       width = 3.23,height = 2, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/proximity/Proximity.png")

##poster version ----
FIG_proximity_poster <- 
  ggplot(data = DATA_proximity, aes(y = proximity_est, x = overlap, col = sex_comparison)) +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 7) +
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high), linewidth = 1.5) +
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
        axis.title.y = element_text(size=35, family = "sans", face = "bold"),
        axis.title.x = element_text(size=35, family = "sans", face = "bold"),
        axis.text.y = element_text(size=30, family = "sans"),
        axis.text.x  = element_text(size=30, family = "sans"),
        legend.text = element_text(size=20, family = "sans", face = "bold"),
        # plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = c(0.85, 0.15),
        legend.key=element_blank(),
        #panel.background = element_rect(fill = "transparent"),
        #legend.background = element_rect(fill = "transparent"),
        #plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.75,0.5,0.25,0.25), "in"))
ggsave(FIG_proximity_poster,
       width = 14.78,height = 6.47, units = "in",
       dpi = 600,
       file="figures/proximity/Proximity_poster.png")

# Distances ----
# x = time
# y = measurement metric
png(file = "figures/distance/distance_pair1.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair1, type="l",
     main = "Pair 1: Christoffer/Kyle (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair2.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair2, type="l",
     main = "Pair 2: Christoffer/Elaine (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair3.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair3, type="l",
     main = "Pair 3: Bumpus/Kyle (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair4.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair4, type="l",
     main = "Pair 4: Elaine/Little Rick (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair5.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair5, type="l",
     main = "Pair 5: Bumpus/Makao (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair6.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair6, type="l",
     main = "Pair 6: Bumpus/Puji (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair7.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair7, type="l",
     main = "Pair 7: Elaine/Rodolfo (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair8.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair8, type="l",
     main = "Pair 8: Annie/Larry (Site 2)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair9.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair9, type="l",
     main = "Pair 8: Larry/Reid (Site 2)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair10.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair10, type="l",
     main = "Pair 10: Margaret/Thomas (Site 2)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair11.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair11, type="l",
     main = "Pair 11: Reid/Thomas (Site 2)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/distance_pair12.png", width = 6.86, height = 6, units = "in", res = 600)
plot(est~timestamp, data=distance_pair12, type="l",
     main = "Pair 12: Maria/Sheron (Site 3)") # type="l" changes the plot from dots to a line
dev.off()

## log transformed ----

png(file = "figures/distance/log-scaled/distance_pair1_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair1, type="l",
     main = "Log-scaled Pair 1: Christoffer/Kyle (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair2_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair2, type="l",
     main = "Log-scaled Pair 2: Christoffer/Elaine (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair3_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair3, type="l",
     main = "Log-scaled Pair 3: Bumpus/Kyle (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair4._log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair4, type="l",
     main = "Log-scaled Pair 4: Elaine/Little Rick (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair5_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair5, type="l",
     main = "Log-scaled Pair 5: Bumpus/Makao (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair6_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair6, type="l",
     main = "Log-scaled Pair 6: Bumpus/Puji (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair7_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair7, type="l",
     main = "Log-scaled Pair 7: Elaine/Rodolfo (Site 1)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair8_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair8, type="l",
     main = "Log-scaled Pair 8: Annie/Larry (Site 2)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair9_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair9, type="l",
     main = "Log-scaled Pair 9: Larry/Reid (Site 2)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair10_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair10, type="l",
     main = "Log-scaled Pair 10: Margaret/Thomas (Site 2)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair11_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair11, type="l",
     main = "Log-scaled Pair 11: Reid/Thomas (Site 2)") # type="l" changes the plot from dots to a line
dev.off()

png(file = "figures/distance/log-scaled/distance_pair12_log.png", width = 6.86, height = 6, units = "in", res = 600)
plot(log(est)~timestamp, data=distance_pair12, type="l",
     main = "Log-scaled Pair 12: Maria/Sheron (Site 3)") # type="l" changes the plot from dots to a line
dev.off()

##poster version ----
png(file = "figures/distance/distance_pair1_poster.png", width = 7.16, height = 4.14, units = "in", res = 600)
par(mar=c(4.5,5,4,2)) #increasing space of left margin so part of the y-axis title isn't cut off (order: bottom, left, top, and right)
plot(est~timestamp, 
     data=distance_pair1, 
     type="l", # type="l" changes the plot from dots to a line
     xlab = "",
     ylab = "distance (m)",
     cex.axis = 1.5, #size of axis font (tick values)
     cex.lab = 2, #size of axis label
     font.lab = 2) #bold axis labels
ylim = c(0,9000)
dev.off()

png(file = "figures/distance/distance_pair11_poster.png", width = 7.16, height = 4.14, units = "in", res = 600)
par(mar=c(4.5,5,4,2)) #increasing space of left margin so part of the y-axis title isn't cut off (order: bottom, left, top, and right)
plot(est~timestamp, 
     data=distance_pair11, 
     type="l", # type="l" changes the plot from dots to a line
     xlab = "",
     ylab = "distance (m)",
     cex.axis = 1.5, #size of axis font (tick values)
     cex.lab = 2, #size of axis label
     font.lab = 2) #bold axis labels
ylim = c(0,9000)
dev.off()

# Tracking Data ----

##pairs ----
#PLOT GPS TRACKING DATA OF IDENTIFIED PAIRS
#male = blue, subadult male = light blue, female = red/yellow
png(file = "figures/tracking data/pair1.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Christoffer, Kyle), col = c("blue3","dodgerblue3"),
     main = "Pair 1: Christoffer/Kyle (Site 1)")
dev.off()

png(file = "figures/tracking data/pair2.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Christoffer, Elaine), col = c("blue3","red"),
     main = "PAIR 2: Christoffer/Elaine (Site 1)")
dev.off()

png(file = "figures/tracking data/pair3.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Bumpus, Kyle), col = c("red","dodgerblue3"),
     main = "PAIR 3: Bumpus/Kyle (Site 1)")
dev.off()

png(file = "figures/tracking data/pair4.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Elaine, Little_rick), col = c("red","dodgerblue3"),
     main = "PAIR 4: Elaine/Little Rick (Site 1)")
dev.off()

png(file = "figures/tracking data/pair5.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Bumpus, Makao), col = c("red","yellow"),
     main = "PAIR 5: Bumpus/Makao (Site 1)")
dev.off()

png(file = "figures/tracking data/pair6.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Bumpus, Puji), col = c("red","yellow"),
     main = "PAIR 6: Bumpus/Puji (Site 1)")
dev.off()

png(file = "figures/tracking data/pair7.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Elaine, Rodolfo), col = c("red","blue3"),
     main = "PAIR 7: Elaine/Rodolfo (Site 1)")
dev.off()

png(file = "figures/tracking data/pair8.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Annie, Larry), col = c("red","blue3"),
     main = "PAIR 8: Annie/Larry (Site 2)")
dev.off()

png(file = "figures/tracking data/pair9.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Larry, Reid), col = c("blue3","dodgerblue3"),
     main = "PAIR 9: Larry/Reid (Site 2)")
dev.off()

png(file = "figures/tracking data/pair10.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Margaret, Thomas), col = c("red","blue3"),
     main = "PAIR 10: Margaret/Thomas (Site 2)")
dev.off()

png(file = "figures/tracking data/pair11.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Reid, Thomas), col = c("dodgerblue3","blue3"),
     main = "PAIR 11: Reid/Thomas (Site 2)")
dev.off()

png(file = "figures/tracking data/pair12.png", width = 6.86, height = 6, units = "in", res = 600)
plot(list(Maria, Sheron), col = c("red","yellow"),
     main = "Maria/Sheron (Site 2)")
dev.off()


##pairs poster version ----
#pair1
png(file = "figures/tracking data/pair1_poster.png", width = 7.16, height = 5.38, units = "in", res = 600)
par(mar=c(5,5,4,2)) #increasing space of left margin so part of the y-axis title isn't cut off (order: bottom, left, top, and right)
plot(list(Christoffer, Kyle), col = c("#004488", "#4393c3"),
     main = "Christoffer and Kyle",
     cex.main = 2, #size of title
     cex.axis = 1.5, #size of axis font (tick values)
     cex.lab = 2,  #size of axis label
     font.lab = 2) #bold axis labels
dev.off()

png(file = "figures/tracking data/pair11_poster.png", width = 7.16, height = 5.38, units = "in", res = 600)
par(mar=c(5,5,4,2)) #increasing space of left margin so part of the y-axis title isn't cut off (order: bottom, left, top, and right)
plot(list(Reid, Thomas), col = c("#004488", "#4393c3"),
     main = "Reid and Thomas",
     cex.main = 2, #size of title
     cex.axis = 1.5, #size of axis font (tick values)
     cex.lab = 2,  #size of axis label
     font.lab = 2) #bold axis labels
dev.off()


# Correlative movement ----

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair1.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair1)
title("Pair 1: Christoffer/Kyle (Site 1)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair2.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair2)
title("PAIR 2: Christoffer/Elaine (Site 1)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair3.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair3)
title("PAIR 3: Bumpus/Kyle (Site 1)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair4.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair4)
title("PAIR 4: Elaine/Little Rick (Site 1)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair5.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair5)
title("PAIR 5: Bumpus/Makao (Site 1)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair6.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair6)
title("PAIR 6: Bumpus/Puji (Site 1)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair8.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair8)
title("PAIR 8: Annie/Larry (Site 2)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair9.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair9)
title("PAIR 9: Larry/Reid (Site 2)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair10.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair10)
title("PAIR 10: Margaret/Thomas (Site 2)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair11.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair11)
title("PAIR 11: Reid/Thomas (Site 2)")
dev.off()

#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair12.png", width = 6.86, height = 6, units = "in", res = 600)
plot.corrMove(cmAnteater_pair12)
title("PAIR 12: Maria/Sheron (Site 2)")
dev.off()

##poster version ----
#3-panel plot of the MCIs over time
png(file = "figures/correlative movement/corrmove_pair1_poster.png", width = 7.16, height = 5.38, units = "in", res = 600)
plot.corrMove(cmAnteater_pair1,
              cex.axis = 1.5, #size of axis font (tick values)
              cex.lab = 2) #size of axis label)
dev.off()
png(file = "figures/correlative movement/corrmove_pair11_poster.png", width = 7.16, height = 5.38, units = "in", res = 600)
plot.corrMove(cmAnteater_pair11,
              cex.axis = 1.5, #size of axis font (tick values)
              cex.lab = 2) #size of axis label))
dev.off()























