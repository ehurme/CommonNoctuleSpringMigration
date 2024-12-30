#######################################
# Script: 5_behavior_thresholds.R
# Purpose: Identify behavioral thresholds using Gaussian models and bootstrapping.
# Output: Figure S2
#######################################

# Load libraries and data
library(pacman)
p_load(dplyr, tidyverse, lubridate,
        ggplot2, ggpubr, 
        rstatix, mclust)
load("./Data/rdata/n_day.robj")
set.seed(123)

# Identify Migration Thresholds with Clustering
## Clean and Subset Data
clean_idx <- which(n_day$diff_vedba > 0 & 
                     n_day$distance > 0 &
                     n_day$time_int == 1) 

X <- n_day$distance[clean_idx] 
Y <- n_day$diff_vedba[clean_idx] 
IDs <- n_day$ID[clean_idx]

## Distance Threshold
# Fit Gaussian Mixture Model
BIC <- mclustBIC(X)
bestmodel <- names(summary(BIC)[1])
dist_modelnames <- substr(bestmodel,1,1)
dist_clusters <- substr(bestmodel,3,3) |> as.numeric()

# Fit Gaussian Mixture Model for Distance
fit_dist <- densityMclust(X, plot = FALSE, G = dist_clusters, 
                          modelNames = dist_modelnames)

# Extract thresholds 
dist_thresh = X[which.min(abs(0.5-fit_dist$z[,dist_clusters]))] # 43 km


# Bootstrap confidence intervals
dist_thresh_sub <- {}
for(i in 1:1000){ 
  try({
    X_sub <- sample(X, size = length(X)*.9, replace = TRUE)
    suppressMessages({
      fit_dist_sub <- densityMclust(X_sub, plot = FALSE, G = dist_clusters, modelNames = dist_modelnames)
    })
    df <- data.frame(X_sub, fit_dist_sub$density)
    df <- df[order(df$X_sub),]
    dist_sub <- spline(X_sub, y = fit_dist_sub$z[,dist_clusters])
    x_50_indices_sub <- which(abs(dist_sub$y - 0.5) < 0.01)
    dist_thresh_sub[i] = max(dist_sub$x[x_50_indices_sub])
    
    # 50% threshold for last cluster
    dist_thresh_sub[i] <- dist_sub$x[which.min(abs(dist_sub$y - 0.5))]
  })
}

dist_thresh_ci <- quantile(dist_thresh_sub, probs = c(0.025, 0.975), na.rm = T)


## VeDBA Threshold
# Fit Gaussian Mixture Model for VeDBA
BIC <- mclustBIC(Y)
bestmodel <- names(summary(BIC)[1])
vedba_modelnames <- substr(bestmodel,1,1)
vedba_clusters <- substr(bestmodel,3,3) |> as.numeric() # 3


fit_vedba <- densityMclust(Y, plot = FALSE, 
                           G = vedba_clusters, 
                           modelNames = vedba_modelnames)
vedba_thresh = Y[which.min(abs(0.5-fit_vedba$z[,vedba_clusters-2]))] 

#### Bootstrap confidence intervals
vedba_thresh_sub <- {}
for(i in 1:1000){
  try({
    Y_sub <- sample(Y, size = length(Y)*.9, replace = TRUE)
    suppressMessages({
      fit_vedba_sub <- densityMclust(Y_sub, plot = FALSE, 
                                     G = vedba_clusters, 
                                     modelNames = vedba_modelnames)
    })
    df <- data.frame(Y_sub, fit_vedba_sub$density)
    df <- df[order(df$Y_sub),]
    vedba_sub <- spline(Y_sub, y = fit_vedba_sub$z[,vedba_clusters-2]) 
    vedba_thresh_sub[i] <- vedba_sub$x[which.min(abs(vedba_sub$y - 0.5))]
  })
}
vedba_thresh_ci <- quantile(vedba_thresh_sub, probs = c(0.025, 0.975), na.rm = TRUE)

# add behavioral labels
n_day$migrating <- ifelse(n_day$distance > dist_thresh, 
                          "migrating", "stationary")
n_day$migrating[which(n_day$time_int != 1)] <- 
  ifelse(n_day$daily_distance[which(n_day$time_int != 1)] > 
           dist_thresh, "migrating", "stationary")
table(n_day$migrating) |> sum()
n_day$low_migrating  <- ifelse(n_day$distance > dist_thresh_ci[1], 
                               "migrating", "stationary")
n_day$low_migrating[which(n_day$time_int != 1)] <- 
  ifelse(n_day$daily_distance[which(n_day$time_int != 1)] > dist_thresh_ci[1], 
         "migrating", "stationary")
table(n_day$low_migrating) |> sum()
n_day$high_migrating  <- ifelse(n_day$distance > dist_thresh_ci[2], 
                                "migrating", "stationary")
n_day$high_migrating[which(n_day$time_int != 1)] <- 
  ifelse(n_day$daily_distance[which(n_day$time_int != 1)] > dist_thresh_ci[2], 
         "migrating", "stationary")

n_day$active <- ifelse(n_day$diff_vedba > vedba_thresh, "foraging", "inactive")
n_day$active[which(n_day$time_int != 1)] <- 
  ifelse(n_day$daily_vedba[which(n_day$time_int != 1)] > vedba_thresh, 
         "foraging", "inactive")
n_day$low_active <- ifelse(n_day$diff_vedba > vedba_thresh_ci[1], 
                           "foraging", "inactive")
n_day$low_active[which(n_day$time_int != 1)] <- 
  ifelse(n_day$daily_vedba[which(n_day$time_int != 1)] > vedba_thresh_ci[1], 
         "foraging", "inactive")
n_day$high_active <- ifelse(n_day$diff_vedba > vedba_thresh_ci[2], 
                            "foraging", "inactive")
n_day$high_active[which(n_day$time_int != 1)] <- 
  ifelse(n_day$daily_vedba[which(n_day$time_int != 1)] > vedba_thresh_ci[2], 
         "foraging", "inactive")

n_day$behav <- n_day$migrating
n_day$behav[which(n_day$migrating == "stationary")] <- 
  n_day$active[which(n_day$migrating == "stationary")]

n_day$high_behav <- n_day$high_migrating
n_day$high_behav[which(n_day$high_migrating == "stationary")] <- 
  n_day$high_active[which(n_day$high_migrating == "stationary")]
table(n_day$high_behav)# |> sum()

n_day$low_behav <- n_day$low_migrating
n_day$low_behav[which(n_day$low_migrating == "stationary")] <- 
  n_day$low_active[which(n_day$low_migrating == "stationary")]
table(n_day$low_behav)# |> sum()

# How long are migration sequences?
n_seq <- n_day[which(n_day$time_int == 1),] |>
  arrange(ID, time) |>
  group_by(ID) |>
  mutate(
    prev_behav = lag(migrating),
    prev_date = lag(time),
    is_change = ifelse(migrating != prev_behav, 1, 0),
    group = cumsum(is_change),
    behav_order = cumsum(is_change) + 1
  ) |>
  group_by(ID, migrating, group) |>
  reframe(
    start_date = min(time),
    end_date = max(time),
    n_consecutive = n()
  ) |>
  ungroup()
n_seq$n_consecutive[n_seq$migrating == "migrating"] |> table()
n_seq$n_consecutive[n_seq$migrating == "migrating"] |> summary()
n_seq$n_consecutive[n_seq$migrating == "stationary"] |> table()
n_seq$n_consecutive[n_seq$migrating == "stationary"] |> summary()

## consecutive migrations
n_temp <- n_day[which(n_day$time_int == 1),] |>
  arrange(ID, time) |>
  group_by(ID) |>
  mutate(
    rle_len = with(rle(behav), rep(lengths, times = lengths)),
    rle_val = with(rle(behav), rep(values, times = lengths))
  ) |>
  group_by(ID, rle_val, rle_len) |>
  mutate(behav_order = row_number()) |>
  filter(!is.na(behav)) |>
  dplyr::select(ID, time, behav, distance, behav_order)

n_temp$ID[which(n_temp$behav_order == 4 & n_temp$behav == "migrating")]

consecutive_migrations <- n_temp[which(n_temp$behav == "migrating" & n_temp$rle_len > 0),]
consecutive_migrations$doy <- yday(consecutive_migrations$time)
consecutive_migrations$ID <- consecutive_migrations$ID |> as.factor()

# Visualizations
## Density plot for distance
dist_breaks <- c(0.01,0.1, 1,10,30,50,100,300)
dist_limit <- c(-2.5,log10(400))
dist_dens <- 
  ggplot()+
  geom_histogram(data = n_day[clean_idx,], 
                 aes(x = distance |> log10(), y=..density..), 
                 fill = "grey60", col = "grey20", bins = 50)+
  geom_area(aes(x = dist_thresh_ci |> log10(), y = c(0.7, 0.7)), 
            fill = "grey", alpha = 0.6)+
  geom_vline(xintercept = dist_thresh |> log10(), lty = 2)+
  ylab("Density")+
  xlab('Daily distance (km)')+
  scale_x_continuous(breaks = log10(dist_breaks),
                     labels = dist_breaks, limits = dist_limit)+
  theme_classic()

## Density plot for VeDBA
vedba_breaks <- round(10^c(2.5, 3, log10(5000), 4, log10(25000)), -2)
vedba_limit <- c(2.30103, log10(25000))
vedba_dens <- 
  ggplot()+
  geom_histogram(data = n_day[clean_idx,], 
                 aes(x = diff_vedba |> log10(), y=..density..), 
                 fill = "grey60", col = "grey20", bins = 40)+
  geom_area(aes(x = vedba_thresh_ci |> log10(), 
                y = c(1.5,1.5)), #c(6e-6, 6e-6)), 
            fill = "grey", alpha = 0.6)+
  geom_vline(xintercept = vedba_thresh |> log10(), lty = 2)+
  ylab("Density")+
  xlab('Daily VeDBA (g)')+
  scale_x_continuous(breaks = log10(vedba_breaks),
                     labels = vedba_breaks, limits = vedba_limit)+
  theme_classic()

## Distance VeDBA scatterplot
vedba_dist_log <- 
  ggplot(n_day[clean_idx,], 
         aes(x = distance |> log10(), 
             y = diff_vedba |> log10(), 
             col = max_temp,
             size = activity))+
  geom_point(alpha = 0.5)+
  geom_hline(yintercept = vedba_thresh |> log10(), lty = 2)+
  geom_hline(yintercept = vedba_thresh_ci[1] |> log10(), lty = 1, alpha = 0.4)+
  geom_hline(yintercept = vedba_thresh_ci[2] |> log10(), lty = 1, alpha = 0.4)+
  geom_vline(xintercept = dist_thresh |> log10(), lty = 2)+
  geom_vline(xintercept = dist_thresh_ci[1] |> log10(), lty = 1, alpha = 0.4)+
  geom_vline(xintercept = dist_thresh_ci[2] |> log10(), lty = 1, alpha = 0.4)+
  scale_size_continuous(range = c(0.5, 2), name = "Daily activity (%)")+
  scale_color_viridis_c(name = "Maximum daily temperature (C)")+
  scale_x_continuous(breaks = log10(dist_breaks),
                     labels = dist_breaks, limits = dist_limit)+
  scale_y_continuous(breaks = log10(vedba_breaks),
                     labels = vedba_breaks, limits = vedba_limit)+
  xlab('Daily distance (km)')+
  ylab("Daily VeDBA (g)")+
  theme_classic()+
  theme(legend.position = "bottom")+
  annotate("text", x = -2, y = log10(25000), label = "Foraging")+
  annotate("text", x = -2, y = log10(250), label = "Inactive")+
  annotate("text", x = 2.2, y = log10(25000), label = "Migrating")
vedba_dist_log

## Combined plot for Distance and VeDBA thresholds
figs2_dist_vedba <- ggarrange(dist_dens, NULL, vedba_dist_log, 
                              vedba_dens+coord_flip(), 
                              ncol = 2, nrow = 2, 
                              widths = c(3,1.3), heights = c(1.3,3), 
                              align = "hv", labels = c("A"), 
                              common.legend = TRUE, legend = "bottom")


## Density plots of tag temperature and activity
# Adjust the theme to reduce subtitle font size and remove legend titles
theme_adjustments <- theme(
  plot.subtitle = element_text(size = 8), # Reduce subtitle font size
  legend.title = element_blank()           # Remove legend title
)

h1 <- ggdensity(n_day[clean_idx,],
                x = "max_temp", add = "median", rug = TRUE,
                color = "migrating", fill = "migrating",
                palette = c("#E7B800", "#00AFBB"))
stat_test1 <- n_day[clean_idx,] |> 
  wilcox_test(max_temp ~ migrating)
h1 <- h1 + theme_adjustments +
  labs(subtitle = get_test_label(stat_test1, detailed = TRUE))+
  xlab("Maximum \n temperature (\u00B0C)")

h2 <- ggdensity(n_day[clean_idx,],
                x = "activity", add = "median", rug = TRUE,
                color = "migrating", fill = "migrating", 
                binwidth = 0.5, palette = c("#E7B800", "#00AFBB"))+
  xlab("24hr Activity %")
stat_test2 <- n_day[clean_idx,] |> 
  wilcox_test(activity ~ migrating)
h2 <- h2 + theme_adjustments +
  labs(subtitle = get_test_label(stat_test2, detailed = TRUE)[-8])

p_migrating <- ggarrange(h1, h2, common.legend = TRUE, align = "hv", labels = c("B", "C"))

h3 <- ggdensity(n_day[clean_idx,],
                x = "max_temp", add = "median", rug = TRUE,
                color = "active", fill = "active",
                palette = "aaas")#c("#E7B800", "#00AFBB"))#+
stat_test3 <- n_day[clean_idx,] |> 
  wilcox_test(max_temp ~ active)
h3 <- h3 + theme_adjustments +
  labs(subtitle = get_test_label(stat_test3, detailed = TRUE)[-8])+
  xlab("Maximum \n temperature (\u00B0C)")

h4 <- ggdensity(n_day[clean_idx,],
                x = "activity", add = "median", rug = TRUE,
                color = "active", fill = "active", 
                binwidth = 0.5, palette = "aaas")+xlab("24hr Activity %")
stat_test4 <- n_day[clean_idx,] |> 
  wilcox_test(activity ~ active)
h4 <- h4 + theme_adjustments +
  labs(subtitle = get_test_label(stat_test4, detailed = TRUE)[-8])

p_active <- ggarrange(h3, h4, common.legend = TRUE, align = "hv", labels = c("D", "E"))
p_act_temp <- ggarrange(p_migrating, p_active, nrow = 2)

figs2 <- ggarrange(figs2_dist_vedba, p_act_temp, nrow = 2, align = "v")
ggsave(figs2, filename = "./Figures/Supplementary/figs2_density.png", height = 10, width = 7)

ggsave(figs2, filename = "./Figures/Supplementary/figs2_density.svg", height = 10, width = 7)

# save
save(fit_dist, fit_vedba,
     dist_thresh, vedba_thresh, 
     dist_thresh_ci, vedba_thresh_ci,
     n_day, 
     consecutive_migrations, 
     file = "./Data/rdata/behavior_thresholds.robj")
load("./Data/rdata/behavior_thresholds.robj")
