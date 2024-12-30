#######################################
# Script: 2_burst_flying_threshold.R
# Purpose: Calculate and visualize thresholds for flying based on VeDBA and speed metrics.
# Output: Figure S1
#######################################

# Consolidated Functions
source("./src/consolidated_functions.R")

# Load Required Libraries
library(pacman)
p_load(lubridate, tidyverse, ggpubr)

# Load Data
# load("./Data/rdata/spring_swiss_migration_steps_full.robj") # n_df
load("../GitHub/NoctuleMigration/Data/n_df_final.robj")
n_df <- n_df_final

# Calculate Thresholds
## VeDBA Threshold (95th Percentile)
vpm_thresh <- quantile(
  n_df$vpm[n_df$day_night == "Day" & n_df$tag_fell_off == FALSE & n_df$vpm > 0],
  probs = 0.95,
  na.rm = TRUE
)

## Speed Threshold (95th Percentile)
speed_thresh <- quantile(
  n_df$ground_sp[n_df$day_night == "Day" & n_df$tag_fell_off == FALSE],
  probs = 0.95,
  na.rm = TRUE
)

# Save Thresholds
save(vpm_thresh, speed_thresh, file = "./Data/rdata/vpm_speed_threshold.robj")

# Visualization
## Create Labels
vpm_label <- data.frame(x = -9, y = vpm_thresh + 3, label = "95th percentile \n daytime VeDBA")
activity_thresh <- 1.56
activity_label <- data.frame(x = -9, y = activity_thresh + 3, label = "Activity % threshold")

n_df <- n_df %>% mutate(
  time_of_day = hour(timestamp) + minute(timestamp) / 60,
  time_of_day = ifelse(time_of_day >= 12, time_of_day - 24, time_of_day)
)

## Plot VeDBA Threshold
daytime_vpm <- ggplot(
  n_df[n_df$vpm > 0 & n_df$diff_time < 100 & n_df$tag_fell_off == FALSE, ],
  aes(
    y = vpm,
    x = time_of_day,
    col = day_night,
    size = sigfox_computed_location_radius / 1000
  )
) +
  geom_point(alpha = 0.4) +
  ylab("Average VeDBA burst (g)") +
  xlab("Time of day") +
  scale_x_continuous(
    limits = c(-12, 12),
    breaks = seq(-12, 12, by = 6),
    labels = c("Noon", "6 PM", "Midnight", "6 AM", "Noon")
  ) +
  theme_minimal() +
  scale_color_manual(name = "", values = c("#F5D35B", "#294AA1")) +
  scale_size_continuous(name = "Location error (km)") +
  geom_hline(yintercept = vpm_thresh, lty = 2) +
  geom_text(
    data = vpm_label,
    aes(x = x, y = y + 0.25, label = label),
    col = 1,
    size = 3
  ) +
  theme(legend.position = "top") +
  geom_hline(yintercept = activity_thresh) +
  geom_text(
    data = activity_label,
    aes(x = x + 18, y = y + 0.25, label = label),
    col = 1,
    size = 3
  )

## Plot Speed Threshold
speed_label <- data.frame(x = -9, y = speed_thresh + 3, label = "95th percentile \n daytime ground speed")
daytime_speed <- ggplot(
  n_df[n_df$vpm > 0 & n_df$diff_time < 100 & n_df$tag_fell_off == FALSE, ],
  aes(
    y = ground_sp,
    x = time_of_day,
    col = day_night,
    size = sigfox_computed_location_radius / 1000
  )
) +
  geom_point(alpha = 0.4) +
  ylab("Ground speed (m/s)") +
  xlab("Time of day") +
  theme_minimal() +
  scale_x_continuous(
    limits = c(-12, 12),
    breaks = seq(-12, 12, by = 6),
    labels = c("Noon", "6 PM", "Midnight", "6 AM", "Noon")
  ) +
  scale_color_manual(name = "", values = c("#F5D35B", "#294AA1")) +
  scale_size_continuous(name = "Location error (km)") +
  geom_hline(yintercept = speed_thresh, lty = 2) +
  geom_text(
    data = speed_label,
    aes(x = x, y = y + 0.25, label = label),
    col = 1,
    size = 3
  ) +
  theme(legend.position = "top")

## Combine and Save Plots
combined_plot <- ggpubr::ggarrange(daytime_vpm, daytime_speed, 
                                   common.legend = TRUE, 
                                   nrow = 2, 
                                   labels = c("A", "B"))
ggsave(combined_plot, file = "./Figures/Supplementary/FigS1.png", height = 8, width = 5)

