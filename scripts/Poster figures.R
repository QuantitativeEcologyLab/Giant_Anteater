# Poster figures

# load packages ----
library(readr)
library(ggplot2)
library(khroma)          #colour blind friendly colour palette
library(ctmm)            #continuous time movement models
library(corrMove)        #correlative movement

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/giant anteater")

# Import dataset
anteater_DATA <- read_csv("data/Anteaters_NoOutliers.csv", col_types = cols(timestamp = "c", class = "c", identity = "c", id = "c", .default = "d"))

# Convert dataset to a telemetry object
DATA <- as.telemetry("data/Anteaters_NoOutliers.csv")

# Home range overlap ----
#load RDS file
AKDE_1 <- readRDS("RDS/movement model/AKDE_1.RDS")

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

#subset overlap for dyads
AKDE_pair1 <- AKDE_1[c(5,8)]
AKDE_pair11 <- AKDE_2[c(9,11)]

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

# Home range pairwise sex comparison ----
#load RDS file
DATA_pairwise <- readRDS("RDS/pairwise/DATA_pairwise.RDS")

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
#load RDS file
DATA_proximity <- readRDS("RDS/proximity/DATA_proximity.RDS")

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
#Load RDS file
distance_pair1 <- readRDS("RDS/distance/distance_pair1.RDS")
distance_pair11 <- readRDS("RDS/distance/distance_pair11.RDS")

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

#Pair tracking data ----
#subset telemetry data for individuals in a dyad
Christoffer <- DATA$Christoffer
Kyle <- DATA$Kyle
Reid <- DATA$Reid
Thomas <- DATA$Thomas

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

##poster version ----
#load RDS files
cmAnteater_pair1 <- readRDS("RDS/correlative movement/cmAnteater_pair1.RDS")
cmAnteater_pair11 <- readRDS("RDS/correlative movement/cmAnteater_pair11.RDS")

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
