# Figure 4

# figure 4
# higher cost in flight
library(pacman)
p_load(tidyverse, 
       moderndive, 
       ggpubr,
       cowplot)
load("./Data/rdata/migration_metrics.robj") # migration_day
load("./Data/rdata/migration_interp.robj") # n_int
load("./Data/rdata/migration_model.robj") # mig_int, fit_vedba_best

theme_set(theme_minimal())

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

early_late_threshold <- median(yday(migration_day$mig_date)) # 127

p_migration_doy <- ggplot(migration_day[migration_day$migrating == "migrating",],
                          aes(x = yday(mig_date),
                              fill = factor(year)))+
  geom_histogram(alpha = 1, binwidth = 1)+
  ylab("Count")+
  xlab("Migration day of year")+
  scale_fill_manual(name = "Year", values = c("#0072B2", "#009E73", "#F0E442"))+
  geom_vline(xintercept = early_late_threshold+0.5, lty = 2)+
  theme(legend.position = "bottom",
        text = element_text(size = 11))
p_migration_doy

p_migration_doy <- p_migration_doy +
  geom_vline(xintercept = early_late_threshold+0.5, lty = 2)+
  annotate("text", x = early_late_threshold+2.5, y = 8, 
           label = "Early-Late", angle = 270, size = 3)
p_migration_doy
mig <- n_int[which(n_int$migrating == "migrating" & n_int$time_int == 1 &
                     !is.na(n_int$year) & !is.na(n_int$diff_vedba) &
                     !is.na(n_int$distance)),]
nrow(mig)
mig$ID %>% unique %>% length
table(mig$year, mig$doy >= early_late_threshold)
mig$early_late <- ifelse(mig$doy >= early_late_threshold, "late", "early")

# model_coef_timing
sum_fit <- summary(fit_vedba_best)
df1<-as.data.frame(sum_fit$coefficients) #selecting full model coefficient averages
CI <- as.data.frame(confint(fit_vedba_best)) # get confidence intervals for full model
df1$CI.min <-CI$`2.5 %`[3:nrow(CI)] #pulling out CIs and putting into same df as coefficient estimates
df1$CI.max <-CI$`97.5 %`[3:nrow(CI)]# order of coeffients same in both, so no mixups; but should check anyway
data.table::setDT(df1, keep.rownames = "coefficient") #put rownames into column
names(df1) <- gsub(" ", "", names(df1)) # remove spaces from column headers
df1$coefficient <- c("Intercept",
                     "Daily distance",
                     "Wind support",
                     "Migration timing [late]") |> factor()
df1$color = ifelse(df1$Estimate > 0, "darkred", "#0072B2")
df1$coefficient[order(df1$Estimate[2:nrow(df1)])+1]

df1$coefficient = factor(df1$coefficient, levels = df1$coefficient[order(df1$Estimate)])
df1$significant = "16" #ifelse(df1$`Pr(>|t|)` > 0.05, "1", "16")

fig4b <-
  ggplot(data=df1[2:nrow(df1),], aes(x=coefficient, y=Estimate))+ #again, excluding intercept because estimates so much larger
  geom_hline(yintercept=0, linetype="dashed", lwd=1.5)+ #add dashed line at zero
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max, col= color), #adj SE
                width=0, lwd=1.5) +
  coord_flip()+ # flipping x and y axes
  geom_point(aes(col = color), size=4)+
  theme_minimal(base_size = 11)+
  xlab("Coefficient")+
  scale_color_manual(values = c("#0072B2", "darkred"))+
  scale_shape_manual(values = c(21, 16))+
  scale_fill_manual(values = c("white", 0))+
  theme(legend.position = "none")+ggtitle("")
fig4b

p_vedba_dist <- ggplot(mig, aes(distance, diff_vedba, shape = factor(year)))+
  geom_point(size = 3)+
  xlab("Daily distance (km)")+ylab("Daily VeDBA (g)")
p_vedba_dist

p_vedba_dist_timing <- ggplot(mig,
                              aes(distance, diff_vedba,
                                  col = early_late,
                                  #col = ws,
                                  shape = factor(year),
                                  #size = ws,
                                  group = early_late))+
  geom_point(size = 3)+
  geom_parallel_slopes()+
  theme_minimal()+
  scale_color_manual(name = "Migration timing", 
                     values = c("#56B4E9", "#E69F00"))+
  scale_shape_discrete(name = "Year")+
  ylab("Daily VeDBA (g)")+
  xlab("Daily distance (km)")+
  theme(legend.position = "bottom",
        text = element_text(size = 11))+
  guides(col=guide_legend(nrow=2,byrow=TRUE),
         shape = guide_legend(nrow=3,byrow=TRUE))
p_vedba_dist_timing

ws_timing <-
  ggplot(mig,
         aes(x = early_late, y = ws,
             fill = early_late))+
  geom_boxplot()+theme_minimal()+
  xlab("Migration timing")+
  ylab("Wind support (m/s)")+
  scale_fill_manual(name = "Migration timing", values = c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")+
  annotate("text", x = 1.5, y = 6.5, label = "**", size = 3)

ws_timing
wilcox.test(mig$ws ~ mig$early_late)

p_vdt <- ggdraw(p_vedba_dist_timing+
                  theme(legend.title = element_text(size = 9*14/5/.pt),
                        legend.text = element_text(size = 8*14/5/.pt),
                        axis.text=element_text(size=7*14/5/.pt),
                        axis.title=element_text(size=9*14/5/.pt)))+
  draw_plot(ws_timing+
              theme(legend.title = element_text(size = 9*14/5/.pt),
                    legend.text = element_text(size = 8*14/5/.pt),
                    axis.text=element_text(size=7*14/5/.pt),
                    axis.title=element_text(size=9*14/5/.pt)),
            x = .68, y = .28,
            width = .3, height = .3)+
  draw_plot_label(c("C", "D"),
                  c(0, 0.55),
                  c(1, 0.55),
                  size = 10*14/5/.pt)

p_vdt

fig4 <- ggarrange(
  ggarrange(p_migration_doy+theme(legend.title = element_text(size = 9*14/5/.pt),
                                  legend.text = element_text(size = 8*14/5/.pt),
                                  axis.text=element_text(size=7*14/5/.pt),
                                  axis.title=element_text(size=9*14/5/.pt)),
            fig4b+theme(legend.title = element_text(size = 9*14/5/.pt),
                        legend.text = element_text(size = 8*14/5/.pt),
                        axis.text=element_text(size=7*14/5/.pt),
                        axis.title=element_text(size=9*14/5/.pt)),
            ncol = 1, labels = c("A", "B"),
            font.label=list(color="black",size=10*14/5/.pt)),
  p_vdt,
  ncol = 2, widths = c(2,3),
  font.label=list(color="black",size=10*14/5/.pt))
fig4

ggsave(fig4, filename = "./Figures/Fig4/Figure4_migration_timing.png",
       width = 6, height = 6, units = "in")
ggsave(fig4, filename = "./Figures/Fig4/Figure4_migration_timing.svg",
       width = 6, height = 6, units = "in")

