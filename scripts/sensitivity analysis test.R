

### Sensitivity Analysis ----
# Calculate the distance threshold to be used as an encounter event

#set encounter radius
#larger the radius = more encounters can occur within that radius due to more individuals that can be within the radius (measurements are in meters)
enc_radius <- 0:1000
enc_count <- vector("numeric", length(enc_radius))

#calculate the number of encounters occurring within each radius size
for(i in 1:length(enc_radius)){
  enc_count[i] <- sum(distance_df$distance_est < enc_radius[i])
}

#to be ggplottified
plot(x = enc_radius, y = enc_count, type = "l")


#sensitivity analysis on male - female encounter significance
encounter_radius_test_pvalue <- vector("numeric", length(enc_radius))
identified_pairs <- unique(overlap_df$pair_ID)

#Loop over encounter radii
for(i in 1:length(enc_radius)){
  
  res <- list()
  
  for (j in identified_pairs){
    subset_A <- distance_df[distance_df$pair_ID == j,]
    
    # Count the number of times "distance_est" is below some threshold distance i 
    encounter_count <- sum(subset_A$distance_est < enc_radius[i])
    
    #save results
    res[[j]] <- data.frame(encounter_count = encounter_count,
                           overlap_est = subset_A$overlap_est[1],
                           sex_comparison = subset_A$sex_comparison[1],
                           site = subset_A$site[1])
    
  }
  
  res <- do.call(rbind, res)
  encounter_radius_test <- try(glmer(encounter_count ~ overlap_est + sex_comparison + (1|site),
                                     family = poisson(link = "log"), data = res, subset = res > 0))
  encounter_radius_test2 <- try(glmer(encounter_count ~ 1 + (1|site), family = poisson(link = "log"), data = res, subset = res > 0))
  encounter_radius_test_results <- try(anova(encounter_radius_test, encounter_radius_test2))
  p_val <- try(encounter_radius_test_results$`Pr(>Chisq)`[2])
  encounter_radius_test_pvalue[i] <- ifelse(class(p_val) == "try-error", NA, p_val)
  
  cat("finished index", i, "\n")
}

#Turn the list of list into a data frame
encounter_radius_pvalue <- do.call(rbind, encounter_radius_test_pvalue)
saveRDS(encounter_radius_pvalue, file = "RDS/encounter_radius_pvalue.RDS")

plot(x = enc_radius[1:16], y = encounter_test_pvalue[1:16], type = "l")
abline(0.05, 0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END