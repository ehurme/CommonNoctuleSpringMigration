#####################################
# Figure1.R
# Purpose: Plot maps of spring bat migration
#####################################

# Load libraries
library(pacman)
p_load(ggplot2, ggpubr)

# Load plots
load("./Data/rdata/Fig1a_tag_date_behavior.robj")
load("./Data/rdata/Fig1b_direction_distance_year.robj")
load("./Data/rdata/fig1c_basic_map_doy.robj")

fig1 <- ggarrange(p_behav_date+
                    theme(#legend.position = "bottom",
                      legend.title = element_text(size = 8*14/5/.pt),
                      legend.text = element_text(size = 7*14/5/.pt),
                      axis.text=element_text(size=7*14/5/.pt),
                      axis.title=element_text(size=9*14/5/.pt)),
                  ggarrange(p_bearing_dist+
                              theme(legend.position = "bottom",
                                    legend.title = element_text(size = 9*14/5/.pt),
                                    legend.text = element_text(size = 8*14/5/.pt),
                                    axis.text=element_text(size=7*14/5/.pt),
                                    axis.title=element_text(size=9*14/5/.pt)),
                            euro_bat_map_scale, ncol = 1, heights = c(2,3), labels = c("B", "C"),
                            font.label=list(color="black",size=10*14/5/.pt)
                  ),
                  nrow = 1, ncol = 2, widths = c(1, 1.5),
                  labels = c("A"),
                  font.label=list(color="black",size=10*14/5/.pt)
)+
  theme(plot.margin = unit(c(0,0,0,0), 'lines'))
fig1

ggsave(fig1, filename = "./Figures/Fig1/fig1.png",
       height = 8,
       width = 7.25,
       units = "in")
ggsave(fig1, filename = "./Figures/Fig1/fig1.svg",
       height = 8,
       width = 7.25,
       units = "in")

