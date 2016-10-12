
# prepare df for analysis -------------------------------------------------


# > dataframe to test rainfall --------------------------------------------


# remove not sheltered
ab_tot_biom_ed <- filter(ab_tot_biom, treatment != "No.shelter")


# Time0 to be used as a covariate 
biom_0 <- ab_tot_biom_ed %>%                                      
  filter(season == "winter" & year == 2013) %>% 
  transmute(plot, live0 = live, dead0 = dead, total0 = total)




# >> summer ---------------------------------------------------------------


# complete dataset
ab_biom_s <- ab_tot_biom_ed %>% 
  filter(season == "summer") %>% 
  left_join(biom_0, by = "plot")


# subset of data only with 'Control' herb. This will be used to test only
# rainfall treatments when herb has a significant effect
ab_biom_s_hcont <- filter(ab_biom_s, herb == "Control")




# >> winter ------------------------------------------------------------------


# complete dataset
ab_biom_w <- ab_tot_biom_ed %>% 
  filter(season == "winter" & year != 2013) %>% 
  left_join(biom_0, by = "plot")


# subset of data only with 'Control' herb. This will be used to test only
# rainfall treatments when herb has a significant effect
ab_biom_w_hcont <- filter(ab_biom_w, herb == "Control")




# > dataframe to test rainfall x herb --------------------------------------------


# summer
tld_biom_by_rxh_s <- ab_biom_s %>%
  filter(treatment %in% c("Pulsed.drought", "Ambient", "Drought")) %>%
  droplevels(.)


# winter
tld_biom_by_rxh_w <- ab_biom_w %>%
  filter(treatment %in% c("Pulsed.drought", "Ambient", "Drought")) %>%
  droplevels(.)




# analysis ----------------------------------------------------------------


# > total biomass ---------------------------------------------------------


# >> summer ---------------------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(total ~ treatment * herb * year, data = tld_biom_by_rxh_s)
plot(total ~ total0, pch = 19, col = treatment, data = tld_biom_by_rxh_s)
total_rh_s_m1 <- lmer(log(total) ~ treatment * herb * year + log(total0) + (1|total0), 
                      data = tld_biom_by_rxh_s)
Anova(total_rh_s_m1, test.statistic = "F")
plot(total_rh_s_m1)
qqnorm(resid(total_rh_s_m1))
qqline(resid(total_rh_s_m1))
# no herb effect





# . rain ------------------------------------------------------------------

# the above analysis showed no herb effect so use the complete dataset
create_trans_boxplot(total ~ treatment * year, data = ab_biom_s)
plot(total ~ total0, pch = 19, col = treatment, data = ab_biom_s)
total_s_m1 <- lmer(log(total) ~ year * treatment + log(total0) + (1|plot), data = ab_biom_s)
Anova(total_s_m1, test.statistic = "F")
plot(total_s_m1)
qqnorm(resid(total_s_m1))
qqline(resid(total_s_m1))





# >> winter ---------------------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(total ~ treatment * herb * year, data = tld_biom_by_rxh_w)
plot(total ~ total0, pch = 19, col = treatment, data = tld_biom_by_rxh_w)
total_rh_w_m1 <- lmer(log(total) ~ treatment * herb * year + log(total0) + (1|total0), 
                      data = tld_biom_by_rxh_w)
Anova(total_rh_w_m1, test.statistic = "F")
plot(total_rh_w_m1)
qqnorm(resid(total_rh_w_m1))
qqline(resid(total_rh_w_m1))
# no herb effect




# . rain ------------------------------------------------------------------


# the above analysis showed no herb effect so use the complete dataset
create_trans_boxplot(total ~ treatment * year, data = ab_biom_w)
plot(total ~ total0, pch = 19, col = treatment, data = ab_biom_w)
total_w_m1 <- lmer(log(total) ~ year * treatment + log(total0) + (1|plot), 
                   data = ab_biom_w)
Anova(total_w_m1, test.statistic = "F")
plot(total_w_m1)
qqnorm(resid(total_w_m1))
qqline(resid(total_w_m1))




# figure ------------------------------------------------------------------


# summary df
summary_ab_biom <- ab_tot_biom %>%
  mutate(month = as.numeric(as.character(month))) %>% 
  filter(treatment != "No.shelter" & herb == "Control") %>% 
  gather(key = variable, value = value, total, live, dead) %>% 
  group_by(year, month, season, treatment, variable) %>% 
  summarise_each(funs(M  = mean, SE = se, N  = get_n), value) %>% 
  mutate(time = paste(year, month.abb[month], sep = "-"))



# create plots
fig_ab_biom <- dlply(summary_ab_biom, .(variable), function(x){
  ggplot(data = x, aes(x = time, y = M, fill = treatment)) +
    labs(x = "Time") +
    
    geom_bar(stat = "identity", position = position_dodge(.9)) +
    geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .5,
                  position = position_dodge(.9), size = .4) +
    
    science_theme +
    theme(legend.position  = "none") +
    ylim(0, 420)
})
fig_ab_biom[[1]]


# add legend
fig_ab_biom[[1]] <- fig_ab_biom[[1]] + 
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position  = c(.8, .85),
        legend.key.width = unit(.2, "inches"))
fig_ab_biom[[1]]


# ylab
ylabs <- paste(c("Dead", "Live", "Total"), "biomass")

for (i in 1:3){
  fig_ab_biom[[i]] <- fig_ab_biom[[i]] + labs(y = ylabs[i])
}


# xlab
for(i in 1:2){
  fig_ab_biom[[i]] <- fig_ab_biom[[i]] +
    scale_x_discrete(labels = NULL) +
    labs(x = NULL)
}


# remove subplot labels
for(i in 2:3){
  fig_ab_biom[[i]] <- fig_ab_biom[[i]] +
    theme(strip.text.x = element_blank())
}

# merge plots
ab_biom_plot_merged <- rbind(ggplotGrob(fig_ab_biom[[1]]), 
                             ggplotGrob(fig_ab_biom[[2]]), 
                             ggplotGrob(fig_ab_biom[[3]]), 
                             size = "last")

grid.newpage()
grid.draw(ab_biom_plot_merged)
ggsavePP(filename = "Output/Figs/biomass", plot = ab_biom_plot_merged,
         width = 6.5, height = 6.5)



# # df to test rain x herb interaction
# tld_biom_by_rxh <- ab_tot_biom_ed %>% 
#   filter(treatment %in% c("Pulsed drought", "Ambient", "Drought")) %>% 
#   droplevels(.)
# 
# 
# # analysis ----------------------------------------------------------------
# 
# 
# # > live --------------------------------------------------------------------
# 
# 
# # . rain x herb -----------------------------------------------------------
# 
# 
# create_trans_boxplot(live ~ treatment * herb, data = tld_biom_by_rxh)
# live_by_rh_m1 <- lm(sqrt(live) ~ treatment * herb, data = tld_biom_by_rxh)
# anova(live_by_rh_m1)
# par(mfrow = c(2, 2))
# plot(live_by_rh_m1)
# # no interaction or herbivore effect
# 
# 
# 
# 
# # . rain ------------------------------------------------------------------
# 
# 
# create_trans_boxplot(live ~ treatment, data = ab_tot_biom_ed)
# live_m1 <- lm(sqrt(live) ~ treatment, data = ab_tot_biom_ed)
# anova(live_m1)
# par(mfrow = c(2, 2))
# plot(live_m1)
# visreg(live_m1)
# 
# 
# 
# 
# # > total -------------------------------------------------------------------
# 
# 
# # . rain x herb -----------------------------------------------------------
# 
# create_trans_boxplot(tot_biomass ~ treatment * herb, data = tld_biom_by_rxh)
# tot__by_rh_m1 <- lm(sqrt(tot_biomass) ~ treatment * herb, data = ab_tot_biom_ed)
# anova(tot__by_rh_m1)
# par(mfrow = c(2, 2))
# plot(tot__by_rh_m1)
# # no interaction or herbivore effect
# 
# 
# 
# 
# # . rain ------------------------------------------------------------------
# 
# 
# create_trans_boxplot(tot_biomass ~ treatment, data = ab_tot_biom_ed)
# tot_m1 <- lm(sqrt(tot_biomass) ~ treatment, data = ab_tot_biom_ed)
# anova(tot_m1)
# par(mfrow = c(2, 2))
# plot(tot_m1)
# visreg(tot_m1)
# 
# 
# 
# 
# # > dead --------------------------------------------------------------------
# 
# 
# # . rain x herb -----------------------------------------------------------
# 
# 
# create_trans_boxplot(Dead ~ treatment * herb, data = tld_biom_by_rxh)
# dead_by_rh_m1 <- lm(log(Dead) ~ treatment * herb, data = tld_biom_by_rxh)
# anova(dead_by_rh_m1)
# par(mfrow = c(2, 2))
# plot(dead_by_rh_m1)
# # no interaction or herbivore effect
# 
# 
# 
# 
# # . rain ------------------------------------------------------------------
# 
# 
# create_trans_boxplot(Dead ~ treatment, data = ab_tot_biom_ed)
# dead_m1 <- lm(log(Dead) ~ treatment, data = ab_tot_biom_ed)
# anova(dead_m1)
# par(mfrow = c(2, 2))
# plot(dead_m1)
# visreg(dead_m1)
# 
# 
# 
# 
# # figure ------------------------------------------------------------------
# 
# biom_m_list <- list(live = live_m1, Dead = dead_m1, tot_biomass = tot_m1)
# 
# 
# # post-hoc test
# biom_posthoc <- ldply(biom_m_list, function(x) {
#   
#   # symbols to be used for figures given by post-hoc test
#   symbols <- cld(glht(x, linfct = mcp(treatment = "Tukey")), decreasing = TRUE)$mcletters$Letters 
#   d <- data.frame(treatment = names(symbols), symbols, row.names = NULL)
#   d$symbols <- as.character(d$symbols)
#   return(d)
# },
# .id = "variable")
# 
# 
# # no treateffect for Dead biomass, so remove symbols
# biom_posthoc$symbols[biom_posthoc$variable == "Dead"] <- ""
# 
# 
# # create summary df
# summary_tld_biom <- ab_tot_biom_ed %>% 
#   gather(variable, value, tot_biomass, Dead, live) %>%
#   group_by(treatment, variable) %>% 
#   summarise_each(funs(M = mean, SE = se, N = get_n), value) %>% 
#   left_join(biom_posthoc, by = c("treatment", "variable")) %>% 
#   ungroup() %>% 
#   mutate(treatment = factor(treatment, levels = c("Ambient", "Increased", "Drought", 
#                                                   "Pulsed drought", "Seasonal")))
# 
# 
# # plot
# biomass_fig_list <- dlply(summary_tld_biom, .(variable), function(x){
#   p <- ggplot(x, aes(x = treatment, y = M, fill = treatment)) +
#     labs(x = NULL) +
#     
#     geom_bar(stat = "identity", col = "black") +
#     geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .3) +
#     geom_text(aes(y = M + SE, label = symbols), vjust = -.4) +
#     
#     scale_x_discrete(labels = c("Ambient", "Increased\n(+50%)", "Reduced\n(-50%)",
#                                 "Reduced\nfrequency", "Summer\ndrought")) +
#     scale_fill_manual(values = rain_cols) +
#     
#     theme(legend.position = "none",
#           panel.border      = element_rect(color = "black"),
#           panel.grid.major  = element_blank(), 
#           panel.grid.minor  = element_blank())
#   
#   return(p)
# })
# 
# 
# # edit y labels
# biomass_fig_list[[1]] <- biomass_fig_list[[1]] + labs(y = "Dead biomass (g)")
# biomass_fig_list[[2]] <- biomass_fig_list[[2]] + labs(y = "Live biomass (g)")
# biomass_fig_list[[3]] <- biomass_fig_list[[3]] + labs(y = "Total biomass (g)")
# 
# 
# # save as PDF and PNG
# l_ply(names(biomass_fig_list), function(x){
#   ggsavePP(filename = paste0("Output/Figs/", x, "_2016"), 
#            plot     = biomass_fig_list[[x]], width = 6, height = 4)
# })
