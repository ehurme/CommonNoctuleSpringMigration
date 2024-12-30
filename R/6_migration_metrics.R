#######################################
# Script: 6_migration_metrics.R
# Purpose: Estimate departure dates and migration path metrics
# Table S7: Time to departure
#######################################

# Load libraries and data
library(pacman)
p_load(tidyverse,  
       ggpubr, lubridate, 
       survival)
source("./src/consolidated_functions.R")
load("./Data/rdata/behavior_thresholds.robj")

# Clean data
n_day_clean <- n_day[which(n_day$distance > 0 &
                             n_day$time_int == 1),]

# Distance traveled
## Summary statistics for migrating bats
n_day$distance[which(n_day$behav == "migrating"& 
                       n_day$time_int == 1)] |> summary() 

n_day$distance[which(n_day$behav == "migrating" & 
                       n_day$time_int == 1)] |> sd() 

migrated <- n_day$ID[n_day$migrating == "migrating"] |> 
  unique() |> na.omit() 

## Average number of migration steps per bat
n_day %>% 
  reframe(migrations = length(which(migrating == "migrating")),
          .by = c(ID)) -> n_sum
n_sum$ID[which(n_sum$migrations > 0)] |> unique() |> na.omit() |> length()
n_sum$migrations[n_sum$migrations > 0] %>% summary()
n_sum$migrations[n_sum$migrations > 0] %>% sd()
n_sum$migrations[n_sum$migrations > 0] %>% range()

# Migration Timing
## Identify Migration Days
n_day[which(n_day$migrating == "migrating"
            & n_day$time_int == 1),] %>% 
  reframe(migration_time = first(time),
          year = year(time),
          .by = c(migrating, ID, date)) -> migration_day
migration_day

## Adjust Migration Dates for Early Departures
migration_day$mig_date <- migration_day$date
migration_day$mig_date[hour(migration_day$migration_time) < 6] <- 
  migration_day$date[hour(migration_day$migration_time) < 6] + days(1)

# Calculate Ranges in Migration Dates
range(migration_day$mig_date[migration_day$year == 2022]) |> diff() 
range(migration_day$mig_date[migration_day$year == 2023]) |> diff() 
range(migration_day$mig_date[migration_day$year == 2024]) |> diff() 

median_migration_day <- median(yday(migration_day$mig_date)) 

# How many migrations does each bat have?
n_day %>% group_by(ID) %>% 
  reframe(total_migrations = 
            length(which(behav == "migrating"))) -> number_of_migrations

# Migration Sequence Analysis
## Calculate Migration Sequences per Bat
n_seq <- n_day[which(n_day$time_int == 1),] %>%
  arrange(ID, time) %>%
  group_by(ID) %>%
  mutate(
    prev_behav = lag(migrating),
    prev_date = lag(time),
    is_change = ifelse(migrating != prev_behav, 1, 0),
    group = cumsum(is_change)
  ) %>%
  group_by(ID, migrating, group) %>%
  reframe(
    start_date = min(time),
    end_date = max(time),
    n_consecutive = n()
  ) %>%
  ungroup()

# Analyze Consecutive Migration and Resident Periods
n_seq$n_consecutive[which(n_seq$migrating == "migrating")] %>% summary()
n_seq$n_consecutive[which(n_seq$migrating == "resident")] %>% summary()

# Departure date
## Estimate Departure Times for Each Bat
bats <- n_day$ID %>% unique
i = 1
departure_time <- data.frame(ID = bats, crossing_time = NA, last_time = NA)
for(i in 1:length(bats)){
  b <- n_day[n_day$ID == bats[i],] 
  starting_location <- c(b$lat[which.min(b$time)], b$lon[which.min(b$time)])
  if(!any(is.na(starting_location))){
    crossing_time <- check_trajectory(b, 
                                      starting_location[1], 
                                      starting_location[2])  
    departure_time$last_time[i] <- as.character(b$time[nrow(b)])
    if(!is.null(crossing_time)){
      departure_time$crossing_time[i] <- as.character(crossing_time)
    }
  }
}

# Is departure time related to capture time or capture weight?
## Add capture mass and deployment time
n_day |> reframe(mass = first(capture_mass),
                 fa = as.numeric(first(fa)), 
                 tag_mass = first(tag_mass),
                 tag_date = first(time),
                 .by = ID) -> capture

departure_time <- full_join(departure_time, capture)

## Categorize Bats Based on Departure Timing
departure_time$early_late <- NA
departure_time$early_late <- ifelse(yday(departure_time$crossing_time) > median_migration_day, 
                                    "late", "early")
departure_time$days_to_departure <- yday(departure_time$crossing_time) - yday(departure_time$tag_date)
departure_time$status <- ifelse(is.na(departure_time$crossing_time), 0, 1)
departure_time$year <- year(departure_time$tag_date)

# table S7
## Survival analysis time to departure
### Cox Proportional Hazards Model
departure_time$body_condition <- departure_time$mass/ departure_time$fa
cox_model <- coxph(Surv(days_to_departure, status) ~ 
                     scale(body_condition)+
                     scale(tag_mass)+
                     factor(year), 
                   data = departure_time)
summary(cox_model)

sjPlot::tab_model(cox_model, file = "./Tables/TableS7_survival_timetodeparture.xls", 
                  transform = NULL, auto.label = FALSE, collapse.ci=FALSE)

# Save results
save(n_sum,
     median_migration_day,
     migration_day, 
     departure_time, 
     n_day, 
     file = "./Data/rdata/migration_metrics.robj")

