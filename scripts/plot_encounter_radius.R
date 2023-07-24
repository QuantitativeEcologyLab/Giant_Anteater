

# plot_enc_radius <-
# ggplot() +
#   geom_line(data = distance_df,
#             aes(x = enc_radius, y = enc_count), 
#             size = 0.15) +
#   xlab("Encounter Radius (m)") +
#   ylab("Encounter Count") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 14, family = "sans", face = "bold"),
#         axis.title.y = element_text(size=10, family = "sans", face = "bold"),
#         axis.title.x = element_text(size=10, family = "sans", face = "bold"),
#         axis.text.y = element_text(size=8, family = "sans"),
#         axis.text.x = element_text(size=8, family = "sans"),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         plot.margin = unit(c(0.1,0.1,0.05,0.2), "cm")) #top, right, bot, left)
# 
# ggsave(plot_enc_radius, width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent",
#        file="figures/individual figures/plot_enc_radius.png")