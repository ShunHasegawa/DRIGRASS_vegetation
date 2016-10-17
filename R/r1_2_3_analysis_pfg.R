summary(pfg_2016)



# prepare df --------------------------------------------------------------

pfg_2016_ed     <- filter(pfg_2016, treatment != "No.shelter")                    # remove no shelter
pfg_by_rxh_s    <- filter(pfg_2016_ed, season == "Summer" & treatment %in% c("Pulsed.drought", "Ambient", "Drought"))  # test rainfall x herb; summer
pfg_by_rxh_w    <- filter(pfg_2016_ed, season == "Winter" & treatment %in% c("Pulsed.drought", "Ambient", "Drought"))  # test rainfall x herb; winter
pfg_ed_s        <- filter(pfg_2016_ed, season == "Summer")                        # test rainfall; summer; complete dataset
pfg_ed_w        <- filter(pfg_2016_ed, season == "Winter")                        # test rainfall; winter; complete dataset
pfg_ed_s_contrh <- filter(pfg_2016_ed, season == "Summer" & herb == "Control")    # test rainfall; summer; subset control-herb
pfg_ed_w_contrh <- filter(pfg_2016_ed, season == "Winter" & herb == "Control")    # test rainfall; winter; subset control-herb





# c3 ratios -------------------------------------------------------------


# > summer ------------------------------------------------------------------


# . test rain x herb ------------------------------------------------------

range(pfg_by_rxh_s$c3ratio)
boxplot(logit(c3ratio) ~ treatment * herb * year, data = pfg_by_rxh_s)
create_trans_boxplot(c3ratio + 1 ~ treatment * herb * year, data = pfg_by_rxh_s)


# use logig
c3_by_rh_s_m1  <- lmer(logit(c3ratio) ~ treatment * herb * year + (1|plot),
                       data = pfg_by_rxh_s)
c3_by_rh_s_fin <- get_persim_lmer(c3_by_rh_s_m1, show.model.res = TRUE)
Anova(c3_by_rh_s_fin, test.statistic = "F")
plot(c3_by_rh_s_fin)
qqnorm(resid(c3_by_rh_s_fin))
qqline(resid(c3_by_rh_s_fin))
    # no herb effect




# . rainfall treatment ----------------------------------------------------


# the above anlaysis showed no herb effect so use the complete dataset
boxplot(logit(c3ratio) ~ treatment * year, data = pfg_ed_s)
c3_s_m1    <- lmer(logit(c3ratio) ~ treatment * year + (1 | plot), data = pfg_ed_s)
c3_s_m_fin <- get_persim_lmer(c3_s_m1)

Anova(c3_s_m1, test.statistic = "F")
plot(c3_s_m1)
qqnorm(resid(c3_s_m1))
qqline(resid(c3_s_m1))
  # remove outliers
rmv <- sort(resid(c3_s_m1), index.return = TRUE)$ix[1:5]
c3_s_m2 <- update(c3_s_m1, subset = -rmv)
c3_s_m_fin_2 <- get_persim_lmer(c3_s_m2)
plot(c3_s_m_fin_2)
qqnorm(resid(c3_s_m_fin_2))
qqline(resid(c3_s_m_fin_2))
Anova(c3_s_m_fin_2, test.statistic = "F")
  # there seem to be significant interaction so keep interactive terms.
c3_s_m_fin <- c3_s_m1 
Anova(c3_s_m_fin, test.statistic = "F")




# winter ------------------------------------------------------------------


# . rain x herb --------------------------------------------------------------
boxplot(logit(c3ratio) ~ treatment * herb * year, data = pfg_by_rxh_w)
create_trans_boxplot(c3ratio ~ treatment * herb * year, data = pfg_by_rxh_w)
c3_by_rh_w_m1  <- lmer(sqrt(c3ratio) ~ treatment * herb * year + (1|plot),
                       data = pfg_by_rxh_w)
c3_by_rh_w_fin <- get_persim_lmer(c3_by_rh_w_m1, show.model.res = TRUE)
Anova(c3_by_rh_w_fin, test.statistic = "F")
plot(c3_by_rh_w_fin)
qqnorm(resid(c3_by_rh_w_fin))
qqline(resid(c3_by_rh_w_fin))
  # no herb effect





# . rain ------------------------------------------------------------------

# the above test showed no herb effect, so use the complete dataset
boxplot(logit(c3ratio) ~ treatment * year, data = pfg_ed_w)
create_trans_boxplot(c3ratio ~ treatment * year, data = pfg_ed_w)

c3_w_m1    <- lmer(sqrt(c3ratio) ~ treatment * year + (1 | plot), data = pfg_ed_w)
c3_w_m_fin <- get_persim_lmer(c3_w_m1)
Anova(c3_w_m_fin, test.statistic = "F")
plot(c3_w_m_fin)
qqnorm(resid(c3_w_m_fin))
qqline(resid(c3_w_m_fin))




# figure ------------------------------------------------------------------


# summary df
summary_pfg <- pfg_2016_ed %>%
  group_by(year, season, treatment) %>%
  summarise_each(funs(M = mean, SE = se, N = get_n), c3ratio)


# plot
fig_c3r <- ggplot(summary_pfg, aes(x = year, y = M, fill = treatment)) +
  facet_grid(. ~ season) +
  labs(x = "Year", y = expression(C[3]~proportion)) +

  geom_bar(stat = "identity", position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .3, 
                position = position_dodge(.9)) +
  science_theme +
  theme(legend.position = c(.1, .9),
        legend.title    = element_blank())
fig_c3r

ggsavePP(filename = "Output/Figs/C3prop",plot = fig_c3r, width = 6, height = 4)


# # post-hoc test
# symbols <- cld(glht(c3r_by_r_m1, linfct = mcp(treatment = "Tukey")),  # symbols to be used for figures given by post-hoc test 
#                decreasing = TRUE)$mcletters$Letters 
# c3r_posthoc <- data.frame(treatment = names(symbols), 
#                           symbols = as.character(symbols), row.names = NULL)
# 
# # summary df
# summary_pfg <- pfg_rain %>% 
#   select(-total, -c4grass) %>% 
#   gather(variable, value, c3ratio) %>% 
#   group_by(treatment, variable) %>% 
#   summarise_each(funs(M = mean, SE = se, N = get_n), value) %>% 
#   left_join(c3r_posthoc, by = "treatment") %>% 
#   ungroup() %>% 
#   mutate(treatment = factor(treatment, levels = c("Ambient", "Increased", "Drought", 
#                                                   "Pulsed drought", "Seasonal")))
# 
# 
# # plot
# fig_c3r <- ggplot(summary_pfg, aes(x = treatment, y = M, fill = treatment)) +
#   labs(x = NULL, y = "C3 ratios") +
#   
#   geom_bar(stat = "identity", col = "black") +
#   geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .3) +
#   geom_text(aes(y = M + SE, label = symbols), vjust = -.4) +
#   
#   scale_x_discrete(labels = c("Ambient", "Increased\n(+50%)", "Reduced\n(-50%)",
#                               "Reduced\nfrequency", "Summer\ndrought")) +
#   scale_fill_manual(values = rain_cols) +
#   theme(legend.position = "none",
#         panel.border      = element_rect(color = "black"),
#         panel.grid.major  = element_blank(), 
#         panel.grid.minor  = element_blank())
# 
# 
# # save
# ggsavePP(filename = "Output/Figs/C3ratios_2016", 
#          plot = fig_c3r, width = 6, height = 4)


