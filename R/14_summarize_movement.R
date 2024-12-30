######################################
# 14_summarize_movement.R
# Purpose: Summarize bat movement metrics and analyze behavioral states.
# Output: Figure S3 - Behavioral states differ by VeDBA
######################################

# Load libraries
library(pacman)
p_load(tidyverse, lubridate, 
       dplyr)

# Load data
load("./Data/rdata/behavior_thresholds.robj")
load("./Data/rdata/migration_metrics.robj")
load("./Data/rdata/spring_swiss_migration_steps_full.robj")
load("./Data/rdata/vpm_speed_threshold.robj")
load("./Data/rdata/n_day_env.robj")
load("./Data/rdata/clean_data.robj")

# Load capture metadata
ts1 <- read.csv("./Tables/TableS1_capture_sheet.csv")

# Identify Flying Status
## Classify records as "flying" or "stationary" based on ground speed and VeDBA thresholds
n_df$flying <- NA
n_df$flying <- with(n_df, ifelse(ground_sp > speed_thresh & 
                                             vpm > vpm_thresh, 
                                           "flying", "stationary"))

# Ground Speed Summary
## Summarize ground speed for flying bats
n_df$ground_sp[n_df$flying == "flying"] %>% summary
n_df$ground_sp[n_df$flying == "flying"] %>% sd(na.rm = TRUE)

# Daily Distance
## Summarize daily distances for migrating bats
n_day$distance[n_day$time_int == 1 & 
                 n_day$behav == "migrating"] %>% summary
n_day$distance[n_day$time_int == 1 & 
                 n_day$behav == "migrating"] %>% sd(na.rm = TRUE)


# How many tags sent data?
n_day$ID |> na.omit() |> unique() |> length() 

# Tagging Duration
## Calculate tag deployment durations
n_df [which(n_df$tag_fell_off == FALSE),] |>
  arrange(tag_local_identifier, timestamp) |>
  group_by(tag_local_identifier) |>
  reframe(start = first(timestamp),
          end = last(timestamp)) -> n_duration

ts1$duration[ts1$attachment == "glue"] %>% as.numeric %>% summary()
ts1$duration[ts1$attachment == "collar"] %>% as.numeric %>% summary()

n_duration$duration <- difftime(n_duration$end, 
                                n_duration$start, units = "days") |> round(1)
n_duration$duration |> as.numeric() |> summary() 
n_duration[order(n_duration$duration, decreasing = TRUE),]

# Behavioral State Summary
## Count occurrences of each behavioral state
n_day$behav %>% table()
n_day$low_behav %>% table()
n_day$high_behav %>% table()

# Migrant Analysis
## Count unique IDs for migrating bats
n_day$ID[n_day$migrating == "migrating"] |> 
  na.omit() |> unique() |> length() 
n_day$ID[n_day$low_migrating == "migrating"] |> 
  na.omit() |> unique() |> length() 
n_day$ID[n_day$high_migrating == "migrating"] |> 
  na.omit() |> unique() |> length() 

# Migration by Year
## Count migrating individuals for each year
n_day$ID[n_day$year == 2022 & n_day$migrating == "migrating"] |>
  na.omit() |> unique() |> length()
n_day$ID[n_day$year == 2022] |>
  na.omit() |> unique() |> length()
n_day$ID[n_day$year == 2023 & n_day$migrating == "migrating"] |>
  na.omit() |> unique() |> length()
n_day$ID[n_day$year == 2023] |>
  na.omit() |> unique() |> length()
n_day$ID[n_day$year == 2024 & n_day$migrating == "migrating"] |>
  na.omit() |> unique() |> length()
n_day$ID[n_day$year == 2024] |>
  na.omit() |> unique() |> length()

# Nights Spent Migrating
## Count migration nights by individual and summarize
n_day[which(n_day$behav == "migrating"),] |>
  group_by(ID) |> summarise(count = n(), year = first(year(date))) -> bat_migration_count
bat_migration_count
bat_migration_count |> summarise(mean(count), sd(count), min(count), max(count))

## by year
n_day[which(n_day$migrating == "migrating"),] |>
  group_by(year, ID) |> summarise(count = n()) -> nights_migrating
nights_migrating |> group_by(year) |> summarise(mean(count), sd(count),
                                                min(count), max(count))
nights_migrating |> ungroup() |> summarise(mean(count), sd(count),
                                           min(count), max(count))

# Migration Speed
## Summarize migration speed (km/day)
n_day[which(n_day$migrating == "migrating"),] |>
  summarise(a = mean(distance, na.rm = TRUE), 
            sd = sd(distance, na.rm = TRUE), 
            min = min(distance, na.rm = TRUE), 
            max = max(distance, na.rm = TRUE))

## total individual average
n_day[which(n_day$migrating == "migrating"),] |> group_by(ID) |>
  summarise(a = mean(distance, na.rm = TRUE)) |>
  summarise(mean = mean(a, na.rm = TRUE), 
            sd = sd(a, na.rm = TRUE), 
            min = min(a, na.rm = TRUE), 
            max = max(a, na.rm = TRUE))
## by year
n_day[which(n_day$migrating == "migrating"),] |> group_by(year, ID) |>
  summarise(a = mean(distance, na.rm = TRUE)) -> dist_year
wilcox.test(x = dist_year$a[dist_year$year == 2022],
            y = dist_year$a[dist_year$year == 2023])
dist_year |> group_by(year) |>
  summarise(mean(a, na.rm = TRUE), 
            sd(a, na.rm = TRUE), 
            min(a, na.rm = TRUE), 
            max(a, na.rm = TRUE))

# Flight Speed
## Summarize ground speed for migrating bats
n_df$year <- year(n_df$timestamp)
n_df[which(n_df$vpm > vpm_thresh & 
             n_df$ground_sp > speed_thresh),] |>
  summarise(mean = mean(ground_sp), 
            sd = sd(ground_sp), 
            min = min(ground_sp), 
            max = max(ground_sp)) |> round(1)

# Cost of Migration
## Compare VeDBA across behavioral states
n_day$behav <- factor(n_day$behav, levels = c("migrating", "foraging", "inactive"))
figs3 <- ggboxplot(n_day[which(!is.na(n_day$behav) & n_day$activity < 90 & n_day$time_int == 1),], x = "behav", y = "diff_vedba")+
  annotate("text", y = 2e4, x = 2, label = "Kruskal-Wallis, p < 0.0001")+
  stat_compare_means(method = "wilcox",
                     comparisons = list(
                       c("migrating", "foraging"),
                       c("migrating", "inactive"),
                       c("foraging", "inactive")),
                     label = "p.signif",
                     label.y = c(2.5e4, 3e4, 3.5e4))+
  theme_minimal() +xlab("")+ylab("Daily Vedba (g)")+ylim(c(0,4.1e4))
figs3

# Save Figure S3
ggsave(figs3,  filename = "./Figures/Supplementary/FigS3.svg",
       width = 4.75, units = "in")
ggsave(figs3,  filename = "./Figures/Supplementary/FigS3.png",
       width = 4.75, units = "in")
