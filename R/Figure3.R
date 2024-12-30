############################################
# Figure 3
# Table S5
############################################

# Figure 3: Bats migrate before storms
library(pacman)
p_load(tidyverse, ggpubr, cowplot, sf, sjPlot)

load("./Data/rdata/clean_data.robj") # m
load("./Data/rdata/migration_model.robj") # ma_migration_delta2_summary
load("./Data/rdata/fig3_pressureplot.robj") # combined_pressure_plot

source("./src/perform_non_parametric_tests.R")
source("./src/summarize_posthoc.R")
source("./src/plot_variable_env_ridges.R")

days = 2

m$`logp_-48` <- log(m$`tp_-48`*1000)
m$`logp_-48`[m$`logp_-48` < -20] <- NA
hist(m$`logp_-48`)

m$`logp_-24` <- log(m$`tp_-24`*1000)
m$`logp_-24`[m$`logp_-24` < -20] <- NA
hist(m$`logp_-24`)

m$logp_0 <- log(m$tp_0*1000)
m$logp_0[m$logp_0 < -20] <- NA
hist(m$logp_0)

m$logp_24 <- log(m$tp_24*1000)
m$logp_24[m$logp_24 < -20] <- NA
hist(m$logp_24)

m$logp_48 <- log(m$tp_48*1000)
m$logp_48[m$logp_48 < -20] <- NA
hist(m$logp_48)

## env plots
### avg
tp_test <- perform_non_parametric_tests(df = m[m$migrating01 == 1,], env_name = "logp")
tp_days <- plot_variable_env_ridges(data = m[m$migrating01 == 1,], env_name = "logp",
                                    env_component = "Total precipitation (mm/hr)",
                                    hours = seq(-24*days, 24*days, by = 24),main_color = "#56B4E9",
                                    adjust_divide = 1)
tp_breaks = c(0.001, 0.01, 0.1, 1, 10)
tp_days <- tp_days+scale_x_continuous(breaks = log(tp_breaks), labels = tp_breaks)


tcc_test <- perform_non_parametric_tests(df = m[m$migrating01 == 1,], env_name = "tcc")
tcc_sum <- summarize_posthoc(tcc_test, units = "", round = 1)
tcc_sum$var <- "tcc"
tcc_days <- plot_variable_env_ridges(data = m[m$migrating01 == 1,], env_name = "tcc",
                                     env_component = "Total cloud cover",
                                     hours = seq(-24*days, 24*days, by = 24),
                                     adjust_divide = 1,main_color = "#888888")+
  geom_text(data = tcc_test$post_hoc,
            aes(label = label, x = 1, y = level+3),
            hjust = -0.2, vjust = 0.5, size = 5, inherit.aes = FALSE)
tcc_days

summary(m$ws100_0[m$migrating01 == 1])
sd(m$ws100_0[m$migrating01 == 1])
ws_test <- perform_non_parametric_tests(df = m[m$migrating01 == 1,], env_name = "avg_ws100")
ws_sum <- summarize_posthoc(ws_test, units = "", round = 1)
ws_sum$var <- "ws"
ws_days <- plot_variable_env_ridges(data = m[m$migrating01 == 1,], env_name = "avg_ws100",
                                    env_component = "Wind support (m/s)",
                                    hours = seq(-24*days, 24*days, by = 24),
                                    main_color = "#F0E442",
                                    adjust_divide = 1)+
  geom_text(data = ws_test$post_hoc,
            aes(label = label, x = 6, y = level+3),
            hjust = -0.2, vjust = 0.5, size = 5, inherit.aes = FALSE)
ws_days
msl_test <- perform_non_parametric_tests(df = m[m$migrating01 == 1,], env_name = "msl")
msl_sum <- summarize_posthoc(msl_test, units = "", round = 0, divide = 100)
msl_sum$var <- "msl"
msl_days <- plot_variable_env_ridges(data = m[m$migrating01 == 1,], env_name = "msl",
                                     env_component = "MSL pressure (hPa)",
                                     hours = seq(-24*days, 24*days, by = 24),
                                     adjust_divide = 100, main_color = "#CC79A7")+
  geom_text(data = msl_test$post_hoc,
            aes(label = label, x = 1028, y = level+3),
            hjust = -0.2, vjust = 0.5, size = 5, inherit.aes = FALSE)+
  scale_x_continuous(breaks = c(1000, 1020, 1040))
msl_days

temp_test <- perform_non_parametric_tests(df = m[m$migrating01 == 1,], env_name = "t2m")
temp_sum <- summarize_posthoc(temp_test, units = "", round = 1, adjust = -273.15)
temp_sum$var <- "temp"
temp_days <- plot_variable_env_ridges(data = m[m$migrating01 == 1,], env_name = "t2m",
                                      env_component = "Temperature (\u00B0C)",
                                      hours = seq(-24*days, 24*days, by = 24),
                                      adjust_add = -273.15, main_color = "#D55E00")+
  geom_text(data = temp_test$post_hoc,
            aes(label = label, x = 16, y = level+3),
            hjust = -0.2, vjust = 0.5, size = 5, inherit.aes = FALSE)
temp_days
fig3b_no_label <- ggarrange(
  ws_days+ylab("")+theme(legend.title = element_text(size = 9*14/5/.pt),
                         legend.text = element_text(size = 8*14/5/.pt),
                         axis.text = element_text(size=7*14/5/.pt),
                         axis.title=element_text(size=9*14/5/.pt)),
  msl_days+ylab("")+theme(legend.title = element_text(size = 9*14/5/.pt),
                          legend.text = element_text(size = 8*14/5/.pt),
                          axis.text=element_text(size=7*14/5/.pt),
                          axis.title=element_text(size=9*14/5/.pt)),
  temp_days+ylab("")+theme(legend.title = element_text(size = 9*14/5/.pt),
                           legend.text = element_text(size = 8*14/5/.pt),
                           axis.text=element_text(size=7*14/5/.pt),
                           axis.title=element_text(size=9*14/5/.pt)),
  tcc_days+ylab("")+theme(legend.title = element_text(size = 9*14/5/.pt),
                          legend.text = element_text(size = 8*14/5/.pt),
                          axis.text = element_text(size=7*14/5/.pt),
                          axis.title=element_text(size=9*14/5/.pt)),
  nrow = 4, ncol = 1)

fig3b <- annotate_figure(fig3b_no_label,
                         left = text_grob("Days to Migration",
                                          rot = 90, vjust = 2,
                                          size = 9*14/5/.pt))
fig3b
# ggsave(fig3b, filename = "./Figures/Fig3/largescale_migration_env_days_avg.png")


df1<-as.data.frame(ma_migration_delta2_summary$coefmat.subset) #selecting full model coefficient averages
tab_model(ma_migration_delta2_summary)
CI <- as.data.frame(confint(ma_migration_delta2_summary, full=F)) # get confidence intervals for full model
df1$CI.min <-CI$`2.5 %` #pulling out CIs and putting into same df as coefficient estimates
df1$CI.max <-CI$`97.5 %`# order of coeffients same in both, so no mixups; but should check anyway
data.table::setDT(df1, keep.rownames = "coefficient") #put rownames into column
names(df1) <- gsub(" ", "", names(df1)) # remove spaces from column headers
df1$coefficient <- c("Intercept",
                     "\u0394 Wind support",
                     "\u0394 MSL pressure",
                     "\u0394 Temperature",
                     "Day of the year",
                     "MSL pressure",
                     "Cloud cover",
                     "Precipitation",
                     "Temperature",
                     "\u0394 Precipitation",
                     "\u0394 Cloud cover")

df1$color = ifelse(df1$Estimate > 0, "darkred", "#0072B2")
df1$coefficient[order(df1$Estimate[2:nrow(df1)])+1]
df1$coefficient = factor(df1$coefficient, levels = df1$coefficient[order(df1$Estimate)])
df1$significant = ifelse(df1$`Pr(>|z|)` > 0.05, "white", "darkred")
df1$significant[which(df1$color == "#0072B2" & df1$significant == "darkred")] <- "#0072B2"

fig3c <- ggplot(data = df1[2:nrow(df1),], aes(x = coefficient, y = Estimate)) + # Excluding intercept because estimates are much larger
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1.5) + # Add dashed line at zero
  geom_errorbar(aes(ymin = CI.min, ymax = CI.max, col = color), width = 0, lwd = 1.5) +
  coord_flip() + # Flip x and y axes
  geom_point(aes(col = color, fill = significant), size = 4, pch = 21) +
  theme_minimal(base_size = 11) +
  xlab("Coefficient") +
  scale_color_manual(values = c("#0072B2", "darkred")) +
  scale_fill_manual(values = c("#0072B2", "darkred", "white")) +
  theme(legend.position = "none")
fig3c
# ggsave(fig3c, filename = "./Figures/Fig3/fig3_modelcoef.png",
#        width = 4, height = 5, units = "in")

fig3 <- ggarrange(combined_pressure_plot,
                  ggarrange(fig3b + theme(legend.title = element_text(size = 9*14/5/.pt),
                                          legend.text = element_text(size = 8*14/5/.pt),
                                          axis.text=element_text(size=7*14/5/.pt),
                                          axis.title=element_text(size=9*14/5/.pt)),
                            fig3c + theme(legend.title = element_text(size = 9*14/5/.pt),
                                          legend.text = element_text(size = 8*14/5/.pt),
                                          axis.text=element_text(size=7*14/5/.pt),
                                          axis.title=element_text(size=9*14/5/.pt)),
                            labels = c("B","C"), widths = c(1,1), nrow = 1,
                            font.label=list(color="black",size=10*14/5/.pt)),
                  labels = c("A", ""), nrow = 2, heights = c(1,2),
                  font.label=list(color="black",size=10*14/5/.pt))
fig3
ggsave(fig3, filename = "./Figures/Fig3/fig3.png",
       width = 6, height = 8, units = "in")
ggsave(fig3, filename = "./Figures/Fig3/fig3.svg",
       width = 4.75, units = "in")

write.csv(rbind(ws_sum, msl_sum, temp_sum, tcc_sum)[,c(7,1:6)],
          file = "./Tables/TableS5_migration_daily_env_comparison.csv")
