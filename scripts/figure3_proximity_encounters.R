
# Proximity ratio and encounters
# Figure 3

#..................................................
## Proximity ratio: Figure 3 A  ----
#..................................................

figure3a_proximity_ratio <- 
  ggplot() +
  geom_hline(data = overlap_df, 
             aes(y = proximity_est, x = overlap_est, col = sex_comparison),
             yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(data = overlap_df, 
             aes(y = proximity_est, x = overlap_est, col = sex_comparison),
             size = 1.5, alpha = 0.3, shape = 16) + #alpha = colour intensity
  geom_segment(data = overlap_df, 
               aes(x = overlap_est, xend = overlap_est, y = proximity_low, yend = proximity_high, col = sex_comparison), 
               linewidth = 0.3, alpha = 0.3) +
  geom_point(data = proximity_identified_pairs_df, 
             aes(y = proximity_est, x = overlap_est, col = sex_comparison),
             size = 1.5) +
  geom_segment(data = proximity_identified_pairs_df,
               aes(x = overlap_est, xend = overlap_est, y = proximity_low, yend = proximity_high, col = sex_comparison), 
               linewidth = 0.3) +
  scale_y_log10(expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_color_manual(values = c("#A50026", "#9970AB", "#004488"),
                     labels = c("Female - Female", "Female - Male", "Male - Male"),
                     name = "") +
  ylab("Proximity ratio") +
  xlab("Home-range overlap") +
  ggtitle("A") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.005, size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = c(0.85, 0.2), #horizontal, vertical
        legend.key.height = unit(0.3, "cm"),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) #top, right, bot, left
figure3a_proximity_ratio
ggsave(figure3a_proximity_ratio, width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent",
       file="figures/figure3a_proximity_ratio.png")

#..................................................
## Encounters: Figure 3 B ----
#..................................................

#do not include 0 encounters
overlap_df2 <- overlap_df[overlap_df$encounter_count != 0,]

figure3b_encounters <-
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
                     labels = c("Female - Female", "Female - Male", "Male - Male"),
                     name = "") +
  xlab("Home-range overlap") +
  ylab("Encounters count") +
  ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        #legend.position="none",
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = c(0.84, 0.2), #horizontal, vertical
        legend.key.height = unit(0.3, "cm"),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
figure3b_encounters
ggsave(figure3b_encounters, width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent",
       file="figures/figure3b_encounters.png")

#..................................................
# multi-panel ----
#..................................................

figure3 <- grid.arrange(figure3a_proximity_ratio,
                        figure3b_encounters,
                             nrow = 2, heights = c(0.35, 0.35))

ggsave(figure3, filename = "figures/figure3.png", device = NULL,
       path = NULL, scale = 1, width = 4, height = 6, units = "in", dpi = 600)



## Plot encounter residuals ----
# FIT_ENC <- lm(log10(encounter_count+0.1) ~ log10(overlap_est), data = proximity_pair_df)
# resids <- residuals(FIT_ENC)
# png(file = "figures/encounter_residuals_sex.png", width = 6.86, height = 6, units = "in", res = 600)
# boxplot(resids ~ proximity_pair_df$sex_comparison,
#         xlab = "Encounter Residuals",
#         ylab = "sex")
# dev.off()



