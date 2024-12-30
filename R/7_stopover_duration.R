##########################################################
# 7_stopover_duration.R
# Description: Use first passage time to estimate stopover durations
# Figure S4. Histogram of stopover durations
##########################################################

# Load libraries and data
library(pacman)
p_load(lubridate, tidyverse,
       adehabitatLT, sf)

load("./Data/rdata/spring_swiss_migration_steps_full.robj")
load("./Data/rdata/behavior_thresholds.robj") # n_day

# Define Projection Systems
proj.ll <- CRS('+proj=lonlat +datum=WGS84 +no_defs')
projUTM <- CRS("+proj=utm +zone=32 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

# Data Preprocessing
## Filter Valid Locations
n_df <- n_df[which(!is.na(n_df$location_lat) & 
                     n_df$tag_fell_off == FALSE),]
n_df <- n_df[order(n_df$timestamp),]

## Convert Coordinates to UTM
n0 <- spTransform(SpatialPoints(n_df[,c("location_long", "location_lat")], 
                                proj4string=proj.ll), 
                  CRSobj=projUTM)
n_df$y <- n0@coords[,2]
n_df$x <- n0@coords[,1]
n_df$ID <- n_df$tag_local_identifier

# Generate Movement Trajectories
n.traj <- as.ltraj(data.frame(X = n_df$x, Y = n_df$y), 
                   date = n_df$timestamp, id = factor(n_df$ID), 
                   burst = n_df$ID)
nj <- ld(n.traj)

# Determine Optimal Radius for FPT Analysis
radii <- seq(20000, 80000, by = 1000)
fpts <- fpt(n.traj, radii, units = "days")

varfpt <- varlogfpt(fpts, graph = FALSE)
names(varfpt) <- radii

# Select Optimal Radius Based on Maximum Variance
Radius <- as.numeric(names(which.max(colMeans(as.matrix(varfpt), 
                                              na.rm = TRUE))))

# Calculate Variance and Standard Error for FPT
fpt_mean <- colMeans(as.matrix(varfpt), na.rm = TRUE)
fpt_se <- {}
for(i in 1:ncol(varfpt)){
  fpt_se[i] <- sd(varfpt[,i], na.rm = TRUE)
}
fpt_rad <- data.frame(fpt_mean, fpt_se, radii)

# First Passage Time Threshold
## Fit Gaussian Mixture Model to Log(FPT)
fpt <- unlist(fpt(n.traj, Radius, units = "days"))
logfpts <- na.omit(log(fpt))
mixmdl <- mixtools::normalmixEM(logfpts, k = 2)

# Determine FPT Threshold
idx <- which.min(mixmdl$mu)
threshold <- mixmdl$mu[idx]+1.96*mixmdl$sigma[idx]
exp(threshold)

##Identify stopovers
s <- summary(n.traj)
IDs <- unique(s$id)

# Loop Through IDs to Identify Stopovers
stopover = data.frame()
for(i in 1:length(IDs)){
  idx <- which(n_df$tag_local_identifier == s$id[i])
  plot(n.traj[i], main = s$id[i])
  FPT <- fpt(n.traj[i], Radius, units = "days") %>% unlist
  if(length(na.omit(FPT)) > 0){
    plot(n_df$timestamp[idx], FPT, ylab = "First Passage Time (days)", type = "o", main = s$id[i])
    
    a <-(FPT - 1)/abs(FPT-1)
    time <- n_df$timestamp[idx]
    
    starts <- time[which(diff(a) == 2)]
    ends <- time[which(diff(a) == -2)]
    
    # Adjust start and end points if necessary
    if(length(starts) > 0 & length(ends) > 0){
      if(ends[1] < starts[1]){
        ends <- ends[-1] 
      }
    }
    # Record stopover details
    if(length(starts) > 0 & length(ends) > 0){
      if(length(starts) == length(ends)){
        durations = difftime(ends, starts)  
        stops <- data.frame(index = i,
                            ID = s$id[i], 
                            start = starts, 
                            end = ends, 
                            duration = round(durations))
      }
      
      if(length(starts) > length(ends)){
        durations = difftime(ends, starts[1:length(ends)])
        stops <- data.frame(index = i,
                            ID = s$id[i], 
                            start = starts[1:length(ends)], 
                            end = ends, 
                            duration = round(durations))
      }  
      
      stopover <- rbind(stopover, stops)
    }
  }
}

# Filter Stopovers by Duration
stopover <- unique(stopover)
stopover <- stopover[stopover$duration > 1,]

# Figure S4
## Stopover histogram
png(filename = "./Figures/Supplementary/FigS4_stopoverduration.png")
  stopover$duration %>% as.numeric %>% hist(breaks = 20, main = "", 
                                            xlab = "Stopover duration (days)", 
                                            xlim = c(0,22))
dev.off()

# Label behaviors during stopovers
stopover$behavs <- NA
stopover$foraging <- NA
stopover$inactive <- NA

for(i in 1:nrow(stopover)){
  idx <- which(stopover$ID[i] == n_day$ID) 
  idx2 <- which(n_day$time[idx] >= stopover$start[i] &
                  n_day$time[idx] <= stopover$end[i])
  behavs <- n_day$behav[idx[idx2]]
  behavs <- behavs[which(behavs != "migrating")]
  stopover$behavs[i] <- length(behavs)
  stopover$foraging[i] <- length(which(behavs == "foraging"))
  stopover$inactive[i] <- length(which(behavs == "inactive"))
}

stopover$percentage <- stopover$foraging/stopover$behavs
summary(stopover$percentage)

# Save stopover data
save(stopover, file = "./Data/rdata/stopover.robj")