
# prepare df for analysis -------------------------------------------------

ab_tot_biom_ed   <- filter(ab_tot_biom, treatment != "No.shelter")  # remove not sheltered
ab_biom_s        <- filter(ab_tot_biom_ed, season == "Summer")      # Test rainfall; summer; complete dataset
ab_biom_w        <- filter(ab_tot_biom_ed, season == "Winter")      # Test rainfall; winter; complete dataset
ab_biom_s_hcont  <- filter(ab_biom_s, herb == "Control")            # Test rainfall; summer; subset Control-herb. subset of data only with 'Control' herb. This will be used to test only rainfall treatments when herb has a significant effect
ab_biom_w_hcont  <- filter(ab_biom_w, herb == "Control")            # Test rainfall; winter; subset Control-herb
ab_biom_by_rxh_s <- filter(ab_biom_s, treatment %in% c("Reduced.frequency", "Ambient", "Reduced"))  # Test rainfall x herb; summer
ab_biom_by_rxh_w <- filter(ab_biom_w, treatment %in% c("Reduced.frequency", "Ambient", "Reduced"))  # Test rainfall x herb; summer
  



# analysis ----------------------------------------------------------------


# total biomass ---------------------------------------------------------


# > summer ---------------------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(total ~ treatment * herb * year, data = ab_biom_by_rxh_s)
total_rh_s_m1 <- lmer(log(total) ~ treatment * herb * year + (1|plot), 
                      data = ab_biom_by_rxh_s)
total_rh_s_m_fin <- get_persim_lmer(total_rh_s_m1, show.model.res = TRUE)
Anova(total_rh_s_m_fin, test.statistic = "F")
plot(total_rh_s_m_fin)
qqnorm(resid(total_rh_s_m_fin))
qqline(resid(total_rh_s_m_fin))
 # one oulier 
which.min(resid(total_rh_s_m_fin))
total_rh_s_m2 <- update(total_rh_s_m1, subset = -62)
qqnorm(resid(total_rh_s_m2))
qqline(resid(total_rh_s_m2))
Anova(total_rh_s_m2, test.statistic = "F")
 # save results




# . rain ------------------------------------------------------------------

# the above analysis showed no herb effect so use the complete dataset
create_trans_boxplot(total ~ treatment * year, data = ab_biom_s)
total_s_m1    <- lmer(log(total) ~ year * treatment + (1|plot), data = ab_biom_s)
total_s_m_fin <- get_persim_lmer(total_s_m1, show.model.res = TRUE)
Anova(total_s_m_fin, test.statistic = "F")
plot(total_s_m_fin)
qqnorm(resid(total_s_m_fin))
qqline(resid(total_s_m_fin))




# > winter ---------------------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(total ~ treatment * herb * year, data = ab_biom_by_rxh_w)
total_rh_w_m1 <- lmer(I(total^(1/3)) ~ treatment * herb * year + (1|plot), 
                      data = ab_biom_by_rxh_w)
total_rh_w_m_fin <- get_persim_lmer(total_rh_w_m1, show.model.res = TRUE)
Anova(total_rh_w_m_fin, test.statistic = "F")
plot(total_rh_w_m_fin)
qqnorm(resid(total_rh_w_m_fin))
qqline(resid(total_rh_w_m_fin))
# quite poor...
sort(resid(total_rh_w_m_fin), index.return = TRUE)$ix[1:3]
total_rh_w_m2 <- update(total_rh_w_m1, subset = -c(14, 25, 13))
plot(total_rh_w_m2)
qqnorm(resid(total_rh_w_m2))
qqline(resid(total_rh_w_m2))
total_rh_w_m_fin2 <- get_persim_lmer(total_rh_w_m2, show.model.res = TRUE)
Anova(total_rh_w_m_fin2, test.statistic = "F")
 # same results




# . rain ------------------------------------------------------------------


# the above analysis showed no herb effect so use the complete dataset
create_trans_boxplot(total ~ treatment * year, data = ab_biom_w)
total_w_m1 <- lmer(log(total) ~ year * treatment + (1|plot), data = ab_biom_w)
total_w_m_fin <- get_persim_lmer(total_w_m1)
Anova(total_w_m_fin, test.statistic = "F")
plot(total_w_m_fin)
qqnorm(resid(total_w_m_fin))
qqline(resid(total_w_m_fin))
 # quite poor
rmv <- sort(resid(total_w_m1), index.return = TRUE)$ix[1:5]
total_w_m2 <- update(total_w_m1, subset = -rmv)
plot(total_w_m2)
qqnorm(resid(total_w_m2))
qqline(resid(total_w_m2))
Anova(total_w_m2, test.statistic = "F")
 # same results





# Live bioass --------------------------------------------------------------------


# > summer ---------------------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(live ~ treatment * herb * year, data = ab_biom_by_rxh_s)
live_rh_s_m1 <- lmer(log(live) ~ treatment * herb * year + (1|plot), 
                     data = ab_biom_by_rxh_s)
live_rh_s_m_fin <- get_persim_lmer(live_rh_s_m1, show.model.res = TRUE)
Anova(live_rh_s_m_fin, test.statistic = "F")
plot(live_rh_s_m_fin)
qqnorm(resid(live_rh_s_m_fin))
qqline(resid(live_rh_s_m_fin))
# one oulier 
which.min(resid(live_rh_s_m_fin))
live_rh_s_m2 <- update(live_rh_s_m1, subset = -62)
qqnorm(resid(live_rh_s_m2))
qqline(resid(live_rh_s_m2))
Anova(live_rh_s_m2, test.statistic = "F")
# save results




# . rain ------------------------------------------------------------------

# the above analysis showed no herb effect so use the complete dataset
create_trans_boxplot(live ~ treatment * year, data = ab_biom_s)
live_s_m1    <- lmer(I(live^(1/3)) ~ year * treatment + (1|plot), data = ab_biom_s)
live_s_m_fin <- get_persim_lmer(live_s_m1, show.model.res = TRUE)
Anova(live_s_m_fin, test.statistic = "F")
plot(live_s_m_fin)
qqnorm(resid(live_s_m_fin))
qqline(resid(live_s_m_fin))




# > winter ---------------------------------------------------------------


# . rain x herb -----------------------------------------------------------
ab_biom_by_rxh_w_ed <- ab_biom_by_rxh_w[complete.cases(ab_biom_by_rxh_w), ]
create_trans_boxplot(live ~ treatment * herb * year, data = ab_biom_by_rxh_w_ed)
live_rh_w_m1 <- lmer(log(live) ~ treatment * herb * year + (1|plot), 
                     data = ab_biom_by_rxh_w_ed)
live_rh_w_m_fin <- get_persim_lmer(live_rh_w_m1, show.model.res = TRUE)
Anova(live_rh_w_m_fin, test.statistic = "F")
plot(live_rh_w_m_fin)
qqnorm(resid(live_rh_w_m_fin))
qqline(resid(live_rh_w_m_fin))
# herbivore effect




# . rain ------------------------------------------------------------------


# the above analysis showed significant herb effect so use the subset of dataset
ab_biom_w_hcont_ed <- ab_biom_w_hcont[complete.cases(ab_biom_w_hcont), ]
ftable(xtabs(~treatment + year, ab_biom_w_hcont_ed))

create_trans_boxplot(live ~ treatment * year, data = ab_biom_w_hcont_ed)
live_w_m1 <- lmer(log(live) ~ year * treatment + (1|plot), data = ab_biom_w_hcont_ed)
live_w_m_fin <- get_persim_lmer(live_w_m1)
Anova(live_w_m_fin, test.statistic = "F")
plot(live_w_m_fin)
qqnorm(resid(live_w_m_fin))
qqline(resid(live_w_m_fin))



# > summer ---------------------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(Dead ~ treatment * herb * year, data = ab_biom_by_rxh_s)
Dead_rh_s_m1 <- lmer(log(Dead) ~ treatment * herb * year + (1|plot), 
                     data = ab_biom_by_rxh_s)
Dead_rh_s_m_fin <- get_persim_lmer(Dead_rh_s_m1, show.model.res = TRUE)
Anova(Dead_rh_s_m_fin, test.statistic = "F")
plot(Dead_rh_s_m_fin)
qqnorm(resid(Dead_rh_s_m_fin))
qqline(resid(Dead_rh_s_m_fin))




# . rain ------------------------------------------------------------------

# the above analysis showed no herb effect so use the complete dataset
create_trans_boxplot(Dead ~ treatment * year, data = ab_biom_s)
Dead_s_m1    <- lmer(log(Dead) ~ year * treatment + (1|plot), data = ab_biom_s)
Dead_s_m_fin <- get_persim_lmer(Dead_s_m1, show.model.res = TRUE)
Anova(Dead_s_m_fin, test.statistic = "F")
plot(Dead_s_m_fin)
qqnorm(resid(Dead_s_m_fin))
qqline(resid(Dead_s_m_fin))




# > winter ---------------------------------------------------------------


# . rain x herb -----------------------------------------------------------
range(ab_biom_by_rxh_w_ed$Dead)
create_trans_boxplot(Dead + 1 ~ treatment * herb * year, data = ab_biom_by_rxh_w_ed)
Dead_rh_w_m1 <- lmer(Dead ~ treatment * herb * year + (1|plot), 
                     data = ab_biom_by_rxh_w_ed)
Dead_rh_w_m_fin <- get_persim_lmer(Dead_rh_w_m1, show.model.res = TRUE)
Anova(Dead_rh_w_m_fin, test.statistic = "F")
plot(Dead_rh_w_m_fin)
qqnorm(resid(Dead_rh_w_m_fin))
qqline(resid(Dead_rh_w_m_fin))
# very poor
# remove 0
create_trans_boxplot(Dead + 1 ~ treatment * herb * year, data = 
                       subset(ab_biom_by_rxh_w_ed, Dead > 0))
Dead_rh_w_m2 <- lmer(log(Dead) ~ treatment * herb * year + (1|plot), 
                     data = ab_biom_by_rxh_w_ed, subset = Dead > 0)
plot(Dead_rh_w_m2)
qqnorm(resid(Dead_rh_w_m2))
qqline(resid(Dead_rh_w_m2))
Anova(Dead_rh_w_m2, test.statistic = "F")
# very similar results with an improved model, showing herb effect




# . rain ------------------------------------------------------------------


# the above analysis showed significant herb effect so use the subset of dataset
create_trans_boxplot(Dead + 1 ~ treatment * year, data = ab_biom_w_hcont_ed)
Dead_w_m1 <- lmer(Dead ~ year * treatment + (1|plot), data = ab_biom_w_hcont_ed)
Dead_w_m_fin <- get_persim_lmer(Dead_w_m1)
Anova(Dead_w_m_fin, test.statistic = "F")
plot(Dead_w_m_fin)
qqnorm(resid(Dead_w_m_fin))
qqline(resid(Dead_w_m_fin))
# very poor....

create_trans_boxplot(Dead ~ treatment * year, data = subset(ab_biom_w_hcont_ed, Dead > 0))
Dead_w_m2 <- lmer(sqrt(Dead) ~ year * treatment + (1|plot), data = ab_biom_w_hcont_ed, 
                  subset = Dead > 0)
plot(Dead_w_m2)
qqnorm(resid(Dead_w_m2))
qqline(resid(Dead_w_m2))
Anova(Dead_w_m2)
# same result with an improved model




# figure ------------------------------------------------------------------


# summary df
summary_ab_biom <- ab_tot_biom %>%
  mutate(month = as.numeric(as.character(month))) %>% 
  filter(treatment != "No.shelter" & herb == "Control") %>% 
  gather(key = variable, value = value, total, live, Dead) %>% 
  group_by(year, month, season, treatment, variable) %>% 
  summarise_each(funs(M  = mean, SE = se, N  = get_n), value)



# create plots
fig_ab_biom <- dlply(summary_ab_biom, .(variable), function(x){
  ggplot(data = x, aes(x = year, y = M, fill = treatment)) +
    facet_grid(. ~ season) +
    labs(x = "Year") +
    
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
# summary_ab_biom <- ab_tot_biom_ed %>% 
#   gather(variable, value, tot_biomass, Dead, live) %>%
#   group_by(treatment, variable) %>% 
#   summarise_each(funs(M = mean, SE = se, N = get_n), value) %>% 
#   left_join(biom_posthoc, by = c("treatment", "variable")) %>% 
#   ungroup() %>% 
#   mutate(treatment = factor(treatment, levels = c("Ambient", "Increased", "Reduced", 
#                                                   "Reduced.frequency", "Summer.drought")))
# 
# 
# # plot
# biomass_fig_list <- dlply(summary_ab_biom, .(variable), function(x){
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
