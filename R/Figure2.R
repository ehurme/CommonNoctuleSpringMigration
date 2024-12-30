#########################################
# Figure 2
# Table S4: comparison of wind conditions

# Figure 2: wind support ----
library(pacman)
p_load(tidyverse, ggpubr, cowplot)

load("./Data/rdata/migration_steps_env_days.robj")
load("./Data/rdata/vpm_speed_threshold.robj") # vpm_thresh, speed_thresh
load("./Data/rdata/fig2_windmap.robj") # p_windmap

source("./src/perform_non_parametric_tests.R")
source("./src/summarize_posthoc.R")
source("./src/plot_variable_env_ridges.R")

days = 2

n_env <- n_env[which(n_env$vpm > vpm_thresh & n_env$ground_sp > speed_thresh),]
n_env$ws100_0 |> summary()
n_env$windsp100_0 |> summary()
n_env$windsp100_0 |> sd(na.rm = TRUE)

ws_test <- perform_non_parametric_tests(df = n_env, env_name = "ws100")
ws_sum <- summarize_posthoc(test_data = ws_test, units = "", round = 1)
ws_sum$var <- "ws"

cw_test <- perform_non_parametric_tests(df = n_env, env_name = "cw100")
cw_sum <- summarize_posthoc(cw_test, units = "", round = 1)
cw_sum$var <- "cw"

as_test <- perform_non_parametric_tests(n_env, "airspeed100")
as_sum <- summarize_posthoc(as_test, units = "", round = 1)
as_sum$var <- "as"

windsp_test <- perform_non_parametric_tests(n_env, "windsp100")
windsp_sum <- summarize_posthoc(windsp_test, units = "", round = 1)
windsp_sum$var <- "windsp"

# 1. ridges ----
windsp_days <- plot_variable_env_ridges(data = n_env, env_name = "windsp100",
                                        env_component = "Wind speed (m/s)",
                                        hours = seq(-24*days, 24*days, by = 24),
                                        main_color = "#0072B2")

windsp_days
ws_days <- plot_variable_env_ridges(data = n_env, env_name = "ws100",
                                    env_component = "Wind support (m/s)",
                                    hours = seq(-24*days, 24*days, by = 24),
                                    main_color = "#F0E442")+
  geom_text(data = ws_test$post_hoc,
            aes(label = label, x = 9, y = level+3),
            hjust = -0.2, vjust = 0.5, size = 3, inherit.aes = FALSE)
ws_days
cw_days <- plot_variable_env_ridges(data = n_env, env_name = "cw100",
                                    env_component = "Crosswind (m/s)",
                                    hours = seq(-24*days, 24*days, by = 24),
                                    main_color = "#D55E00")+
  geom_text(data = cw_test$post_hoc,
            aes(label = label, x = 9, y = level+3),
            hjust = -0.2, vjust = 0.5, size = 3, inherit.aes = FALSE)
cw_days
as_days <- plot_variable_env_ridges(data = n_env, env_name = "airspeed100",
                                    env_component = "Airspeed (m/s)",
                                    hours = seq(-24*days, 24*days, by = 24),
                                    main_color = "#009E73")+
  geom_text(data = as_test$post_hoc,
            aes(label = label, x = 31, y = level+3),
            hjust = -0.2, vjust = 0.5, size = 3, inherit.aes = FALSE)+xlim(c(-5,40))
as_days
p_wind_ridges <- ggarrange(windsp_days, ws_days, cw_days, as_days, ncol = 1,
                           labels = c("A", "B", "C", "D"), label.x = -0.01)
p_wind_ridges
p_wind_ridges_no_label <- ggarrange(windsp_days+ylab("")+theme(legend.title = element_text(size = 9*14/5/.pt),
                                                               legend.text = element_text(size = 8*14/5/.pt),
                                                               axis.text=element_text(size=7*14/5/.pt),
                                                               axis.title=element_text(size=9*14/5/.pt)),
                                    ws_days+ylab("")+theme(legend.title = element_text(size = 9*14/5/.pt),
                                                           legend.text = element_text(size = 8*14/5/.pt),
                                                           axis.text=element_text(size=7*14/5/.pt),
                                                           axis.title=element_text(size=9*14/5/.pt)),
                                    cw_days+ylab("")+theme(legend.title = element_text(size = 9*14/5/.pt),
                                                           legend.text = element_text(size = 8*14/5/.pt),
                                                           axis.text=element_text(size=7*14/5/.pt),
                                                           axis.title=element_text(size=9*14/5/.pt)),
                                    as_days+ylab("")+theme(legend.title = element_text(size = 9*14/5/.pt),
                                                           legend.text = element_text(size = 8*14/5/.pt),
                                                           axis.text=element_text(size=7*14/5/.pt),
                                                           axis.title=element_text(size=9*14/5/.pt)),
                                    ncol = 1, heights = 4, widths = 1)
p_wind_ridges_no_label
fig2a <- annotate_figure(p_wind_ridges_no_label,
                         left = text_grob("Days to Migration\n", rot = 90, vjust = 1,
                                          size = 9*14/5/.pt))
fig2a

fig2 <- ggarrange(fig2a, p_windmap+xlim(c(-10000, 40000))+
                    theme(legend.title = element_text(size = 9*14/5/.pt),
                          legend.text = element_text(size = 8*14/5/.pt),
                          axis.text=element_text(size=7*14/5/.pt),
                          axis.title=element_text(size=9*14/5/.pt)),
                  # fig2b,
                  ncol = 2, widths= c(1,1.5), labels = c("A", "B"),
                  font.label=list(color="black",size=10*14/5/.pt))
fig2
ggsave(fig2, width = 4.75, height = 4, units = "in",
       filename = "./Figures/Fig2/Fig2.png")
ggsave(fig2, width = 4.75, height = 4, units = "in",
       filename = "./Figures/Fig2/Fig2.svg")

write.csv(rbind(windsp_sum, ws_sum, cw_sum, as_sum)[,c(7,1:6)],
          file = "./Tables/TableS4_migration_burst_env_comparison.csv")

