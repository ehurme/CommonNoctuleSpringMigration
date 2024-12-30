#############################
# 13_markov_transition.R
# Purpose: Fit a Markov model to analyze behavioral state transitions for noctule bats.
# Output: Table S2 - Transition Probability Matrix
#############################

# Load libraries
library(pacman)
p_load(tidyverse, 
       msm,
       move)
set.seed(123)

# Load data
load("./Data/rdata/behavior_thresholds.robj")

# Define Behavioral States
## Map behaviors to numerical states for Markov modeling
n_day$state <- NA
n_day$state[n_day$behav == "inactive"] <- 1
n_day$state[n_day$behav == "foraging"] <- 2
n_day$state[n_day$behav == "migrating"] <- 3

# Initial Transition Matrix
## Initialize a 3x3 transition matrix with uniform probabilities
q <- matrix(rep(.1, 9), 3, 3)

# Data Filtering
## Remove individuals with only one location to ensure valid Markov modeling
keep <- names(which(table(n_day$ID) > 1))
n_day$ID[n_day$ID %in% keep] %>% table
msm_data <- n_day[n_day$ID %in% keep,]
msm_data <- msm_data[order(msm_data$ID, msm_data$time),]

# Fit Markov Model
## Fit the Markov model using msm package
n.msm <- msm( state ~ time_running, subject=ID, 
              data = msm_data,
              qmatrix = q,
              control = list ( trace = 2, REPORT = 1 )  )

# Generate Transition Probability Matrix
## Calculate transition probabilities for t = 1
pmatrix.msm(n.msm, t=1) %>% round(2)

# Table S2
## Save Transition Matrix
write.csv(pmatrix.msm(n.msm, t=1) %>% round(2), 
          file = "./Tables/TableS2_transition_probability_matrix.csv")

