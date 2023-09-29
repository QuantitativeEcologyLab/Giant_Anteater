
# Case study 
#Margaret and Thomas

#............................................................
#subset AKDE for each individual
AKDE_thomas <- AKDE["Thomas"]
AKDE_margaret <- AKDE["Margaret"]

#calculate mean home range sizes for Thomas
meta(AKDE_thomas)

#calculate mean home range sizes for Margaret
meta(AKDE_margaret)

#calculate home range overlap
round(proximity_identified_pairs_df[11,]$overlap_low, 2)
round(proximity_identified_pairs_df[11,]$overlap_est, 2)
round(proximity_identified_pairs_df[11,]$overlap_high, 2)

#calculate proximity ratio
round(proximity_identified_pairs_df$proximity_low[proximity_identified_pairs_df$pair_ID_number == 11], 2)
round(proximity_identified_pairs_df$proximity_est[proximity_identified_pairs_df$pair_ID_number == 11], 2)
round(proximity_identified_pairs_df$proximity_high[proximity_identified_pairs_df$pair_ID_number == 11], 2)

#calculate distances (measurements are in meters, convert them to km)
round(mean(distance_pairs_df$est[distance_pairs_df$pair_ID_number == 11])/1000, 2)
round(min(distance_pairs_df$est[distance_pairs_df$pair_ID_number == 11])/1000, 2)
round(max(distance_pairs_df$est[distance_pairs_df$pair_ID_number == 11])/1000, 2)

#calculate the mean 95% CI correlative movement
round(mean(cm_pair11$etaTot.CI.Low), 2)
round(mean(cm_pair11$etaTot.MLE), 2)
round(mean(cm_pair11$etaTot.CI.Upp), 2)

