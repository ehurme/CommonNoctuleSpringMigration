############################################
# Figure1a.R
# Purpose: Visualize tagged bat behaviors over time, grouped by year and ID.
############################################

# Load libraries
library(pacman)
p_load(tidyverse, ggplot2, ggpubr, lubridate)

# Load data
load("./Data/rdata/behavior_thresholds.robj") # n_day
ts1 <- read.csv("./Tables/TableS1_capture_sheet.csv")

IDs <- ts1$ID
missing <- ts1[-which(IDs %in% n_day$ID),]
temp <- data.frame(ID = missing$ID, time = ymd_hms(missing$deployment),
                   year = year(missing$deployment),
                   doy = yday(missing$deployment))
n_day <- full_join(n_day, temp)
n_day <- n_day %>% arrange(desc(time))

# Create an ordering variable based on the first occurrence of each ID
n_day <- n_day %>%
  group_by(ID) %>%
  mutate(first_appearance = min(time)) %>%
  ungroup() %>%
  arrange(desc(first_appearance))

# Create a factor with levels ordered by first appearance
n_day$ID_ordered <- factor(n_day$ID, levels = unique(n_day$ID))

p_behav_date <- ggplot(n_day,
                       aes(x = doy, y = ID_ordered,
                           col = factor(behav, 
                                        levels = c("migrating",
                                                   "foraging",
                                                   "inactive",
                                                   "NA")))) +
  geom_point() +
  facet_grid(year ~ ., scales = "free_y", space = "free_y") +
  ylab("Tag ID") +
  xlab("Day of the Year") +
  theme_minimal() +
  theme(legend.position = c(.8, .95),
        # legend.position = "bottom",
        legend.key.size = unit(.1, "cm"),
        legend.spacing.y = unit(0.01, 'cm'),
        axis.text.y = element_text(size = 5, hjust = 0),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill = "white", color = "black"),
        strip.text.y = element_text(size = 6, face = "bold"),
        panel.spacing.y = unit(0.01, "lines"))+  # Adjust spacing between facets
  scale_color_manual(name = "Behavior", 
                     values = c("#E69F00", "#009E73", "#56B4E9", "#999999"))

p_behav_date
save(n_day, p_behav_date, file = "./Data/rdata/Fig1a_tag_date_behavior.robj")
