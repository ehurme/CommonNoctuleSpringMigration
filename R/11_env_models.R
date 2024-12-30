########################################
# File: 11_env_models.R
# Purpose: Fit and analyze environmental models for migration behavior and energetics.
# Outputs: 
# Table S6
# Table S8
# Table S9
# Table S10
########################################

# Load Required Libraries
library(pacman)
p_load(tidyverse,
       lme4,  
       ggpubr, 
       ggeffects,
       MuMIn, 
       sjPlot) 
theme_set(theme_minimal())

# Load data
load("./Data/rdata/migration_metrics.robj")
load("./Data/rdata/migration_interp.robj")
load("./Data/rdata/clean_data.robj")

# Migration model 
## Fit Full Migration Model
fit_migration <- glmer(migration ~ 
                         scale(tp) + scale(diff_tp)+
                         scale(tcc) + scale(diff_tcc)+
                         scale(t2m) + scale(diff_t2m) + 
                         scale(msl) + scale(diff_msl) + 
                         scale(avg_ws) + scale(diff_avg_ws) +
                         scale(doy) + 
                         (1|ID)+(1|year),
                       family = "binomial",
                       data = cm, 
                       na.action = "na.fail")
summary(fit_migration)
tab_model(fit_migration)

### Model Selection via Dredge
d_migration <- MuMIn::dredge(fit_migration) 
s <- subset(d_migration, delta < 2) # 24 models within 2 AIC 
s[1] # Best model

### Model Averaging for Selected Models
# Model average models with delta AICc < 2
ma_migration_delta2 <- try(model.avg(d_migration, subset = delta < 2, fit = TRUE))
ma_migration_delta2_summary <- ma_migration_delta2 |> summary()

# Table S6
tab_model(ma_migration_delta2_summary, 
          transform = NULL, auto.label = FALSE, 
          collapse.ci = FALSE, 
          file = "./Tables/TableS6_migration_model_average.xls")

# Busy model migration
## Subset Data for Migrating Bats
mig <- cm[which(cm$migration == 1),]

## Fit Full Busy Migration Model
fit_busy <- glmer(busy_mean ~
                    scale(t2m) + scale(diff_t2m) + 
                    scale(tcc) + scale(diff_tcc)+
                    scale(tp) + scale(diff_tp)+
                    scale(msl) + scale(diff_msl) + 
                    scale(ws) + scale(diff_ws) +
                    scale(doy) +  
                    (1|ID)+(1|year),
                  family = "binomial",
                  data = mig, 
                  na.action = "na.fail")
summary(fit_busy)

## Dredge for Model Selection
d_busy <- dredge(fit_busy)
d_busy[1]

## Model Averaging for Selected Models
ma_busy_delta2 <- try(model.avg(d_busy, subset = delta < 2, fit = TRUE))
ma_busy_delta2_summary <- ma_busy_delta2 |> summary()

## Table S9
tab_model(ma_busy_delta2_summary,
          file = "./Tables/TableS9_busy_migration.xls",
          transform = NULL, auto.label = FALSE, collapse.ci=FALSE)

# VeDBA Timing and Energetics
## Calculate Early/Late Migration Threshold
early_late_threshold <- median(yday(migration_day$mig_date))
ymd("2023-01-01")+early_late_threshold-1

## Prepare Data for Energetics Model
mig_int <- n_int[which(n_int$migrating == "migrating" & n_int$time_int == 1 &
                     !is.na(n_int$year) & !is.na(n_int$diff_vedba) &
                     !is.na(n_int$distance)),]
mig_int$early_late <- ifelse(mig_int$doy >= early_late_threshold, "late", "early")
mig_clean <- mig_int[which(!is.na(mig_int$activity)),]

## Fit VeDBA Model
fit_vedba <- lmer(scale(diff_vedba) ~
                    scale(distance) +
                    scale(ws)+
                    scale(cw)+
                    scale(activity)+
                    early_late+
                    (1|year),
                  data = mig_clean, na.action = "na.fail")
summary(fit_vedba)
tab_model(fit_vedba)

## Model Selection and Final Model
d_vedba <- dredge(fit_vedba)
fit_vedba_best <- lmer(scale(diff_vedba) ~
                         scale(distance) +
                         scale(ws) +
                         early_late +
                         (1|year),
                       data = mig_clean, na.action = "na.omit")
summary(fit_vedba_best)

## Save Table S10
tab_model(fit_vedba_best,
          transform = NULL, auto.label = FALSE, collapse.ci = FALSE,
          file = "./Tables/TableS10_migration_energetics.xls")

# Save results
save(d_migration, 
     d_busy,
     d_vedba,
     ma_migration_delta2, 
     ma_migration_delta2_summary, 
     ma_busy_delta2,  
     ma_busy_delta2_summary,
     fit_migration, 
     fit_busy, 
     fit_vedba,
     fit_vedba_best,
     m, cm, mig, mig_clean,
     file = "./Data/rdata/migration_model.robj")

