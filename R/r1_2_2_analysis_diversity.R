# parepare dataframe for analysis -----------------------------------------


# > dataframe to test rainfall --------------------------------------------


# remove shelteered
div_2016_ed   <- filter(div_2016, treatment != "No.shelter")  

# Time0 to be used as a covariate 
div_0 <- div_2016_ed %>%                                      
  filter(season == "winter" & year == 2013) %>% 
  transmute(plot, H0 = H, S0 = S, J0 = J)




# >> summer ---------------------------------------------------------------


# complete dataset
div_2016_ed_s <- div_2016_ed %>%
  filter(season == "summer") %>%
  left_join(div_0, by = "plot")


# subset of data only with 'Control' herb. This will be used to test only
# rainfall treatments when herb has a significant effect
div_2016_ed_s_hcont <- filter(div_2016_ed_s, herb == "Control")
  



# >> winter ---------------------------------------------------------------


div_2016_ed_w <- div_2016_ed %>% 
  filter(season == "winter" & year == 2014) %>% 
  left_join(div_0, by = "plot")




# > dataframe to test rainfall x herb --------------------------------------------


# summer
div_2016_by_rxh_s <- div_2016_ed_s %>%
  filter(treatment %in% c("Pulsed.drought", "Ambient", "Drought")) %>%  # subset rainfall treatments
  droplevels(.)                                                         # remove empty levels. "." indicated the dataframe inherited from the above (i.e. filter) 


# winter
div_2016_by_rxh_w <- div_2016_ed_w %>% 
  filter(treatment %in% c("Pulsed.drought", "Ambient", "Drought")) %>%
  droplevels(.)




# analysis ----------------------------------------------------------------


# > Diversity (H) ---------------------------------------------------------


# >> summer ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------

create_trans_boxplot(H ~ treatment * herb * year, data = div_2016_by_rxh_s)
plot(H ~ H0, pch = 19, col = treatment, data = div_2016_by_rxh_s)
h_by_rh_s_m1 <- lmer(H ~ year * treatment * herb + H0 + (1|plot), data = div_2016_by_rxh_s)
Anova(h_by_rh_s_m1, test.statistic = "F")
plot(h_by_rh_s_m1)
qqnorm(resid(h_by_rh_s_m1))
qqline(resid(h_by_rh_s_m1))
# significant main effects, including herb, so herb shouldn't be analysed with rainfall




# . rainfall -------------------------------------------------------------------


# the above analysis showed a herb effect so use the sutbset dataframe without
# added-herb
create_trans_boxplot(H ~ treatment * year, data = div_2016_ed_s_hcont)
plot(H ~ H0, pch = 19, col = treatment, data = div_2016_ed_s_hcont)
h_s_m1 <- lmer(H ~ treatment * year + H0 + (1|plot), data = div_2016_ed_s_hcont)
Anova(h_s_m1, test.statistic = "F")
plot(h_s_m1)
qqnorm(resid(h_s_m1))
qqline(resid(h_s_m1))




# >> winter ------------------------------------------------------------------


# . rain x herb -----------------------------------------------------------


create_trans_boxplot(H ~ treatment * herb, data = div_2016_by_rxh_w)
plot(H ~ H0, data = div_2016_by_rxh_w, pch = 19, col = treatment)
h_by_rh_w_m1 <- lm(H ~ treatment * herb + H0, data = div_2016_by_rxh_w)
Anova(h_by_rh_w_m1)
par(mfrow = c(2, 2))
plot(h_by_rh_w_m1)
# no herbivore effect




# . rainfall --------------------------------------------------------------


# the above analysis showed no herb effect, so use the complete dataset
create_trans_boxplot(H ~ treatment, data = div_2016_ed_w)
plot(H ~ H0, pch = 19, col = treatment, data = div_2016_ed_w)
h_w_m1 <- lm(H ~ treatment + H0, data = div_2016_ed_w)
Anova(h_w_m1)
par(mfrow = c(2, 2))
plot(h_w_m1)




# > Evenness --------------------------------------------------------------


# >> summer ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(J ~ treatment * herb, data = div_2016_by_rxh_s)
plot(J ~ J0, pch = 19, col = treatment, data = div_2016_by_rxh_s)
j_by_rh_s_m1 <- lmer(J ~ year * treatment * herb + J0 + (1|plot), data = div_2016_by_rxh_s)
j_by_rh_s_m2 <- lmer(J ~ year * treatment * herb + (1|plot), data = div_2016_by_rxh_s)
Anova(j_by_rh_s_m1, test.statistic = "F")
Anova(j_by_rh_s_m2, test.statistic = "F")
plot(j_by_rh_s_m2)
qqnorm(resid(j_by_rh_s_m2))
qqline(resid(j_by_rh_s_m2))
# herbxtreatment effect. So analyse rainfall and herb separately




# . rainfall --------------------------------------------------------------


# the above analysis showed herb effect so use the daset withought herb-added
create_trans_boxplot(J ~ treatment * year, data = div_2016_ed_s_hcont)
plot(J ~ J0, pch = 19, col = treatment, data = div_2016_ed_s_hcont)
j_s_m1 <- lmer(J ~ treatment * year + J0 + (1|plot), data = div_2016_ed_s_hcont)
Anova(j_s_m1, test.statistic = "F")
plot(j_s_m1)
qqnorm(resid(j_s_m1))
qqline(resid(j_s_m1))




# >> winter ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(J ~ treatment * herb, data = div_2016_by_rxh_w)
plot(J ~ J0, data = div_2016_by_rxh_w, pch = 19, col = treatment)
j_by_rh_w_m1 <- lm(J ~ treatment * herb + J0, data = div_2016_by_rxh_w)
Anova(j_by_rh_w_m1)
par(mfrow = c(2, 2))
plot(j_by_rh_w_m1)
# no herbivore effect




# . rainfall -------------------------------------------------------------


# the above anlaysis showed no herb effect so use the complete dataset
create_trans_boxplot(J ~ treatment, data = div_2016_ed_w)
plot(J ~ J0, pch = 19, col = treatment, data = div_2016_ed_w)
j_w_m1 <- lm(J ~ treatment + J0, data = div_2016_ed_w)
Anova(j_w_m1)
par(mfrow = c(2, 2))
plot(j_w_m1)




# > species richness --------------------------------------------------------


# >> summer ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(S ~ treatment * herb, data = div_2016_by_rxh_s)
plot(S ~ S0, pch = 19, col = treatment, data = div_2016_by_rxh_s)
s_by_rh_s_m1 <- lmer(S ~ year * treatment * herb + S0 + (1|plot), data = div_2016_by_rxh_s)
Anova(s_by_rh_s_m1, test.statistic = "F")
plot(s_by_rh_s_m1)
qqnorm(resid(s_by_rh_s_m1))
qqline(resid(s_by_rh_s_m1))
# no herb effect





# . rianfall ----------------------------------------------------------------


# the above analysis showed no herb effect, so use the complete dataset
create_trans_boxplot(S ~ treatment * year, data = div_2016_ed_s)
plot(log(S) ~ log(S0), pch = 19, col = treatment, data = div_2016_ed_s)
s_s_m1 <- lmer(S ~ treatment * year + S0 + (1|plot), data = div_2016_ed_s)
Anova(s_s_m1, test.statistic = "F")
plot(s_s_m1)
qqnorm(resid(s_s_m1))
qqline(resid(s_s_m1))




# >> winter ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(S ~ treatment * herb, data = div_2016_by_rxh_w)
plot(S ~ S0, pch = 19, col = treatment, data = div_2016_by_rxh_w)
s_by_rh_w_m1 <- lm(S ~ treatment * herb + S0, data = div_2016_by_rxh_w)
Anova(s_by_rh_w_m1)
par(mfrow = c(2, 2))
plot(s_by_rh_w_m1)
# no herbivore effect




# . rainfall -------------------------------------------------------------


# the above anlaysis showed no herb effect so use the complete dataset
create_trans_boxplot(S ~ treatment, data = div_2016_ed_w)
plot(S ~ S0, pch = 19, col = treatment, data = div_2016_ed_w)
s_w_m1 <- lm(S ~ treatment + S0, data = div_2016_ed_w)
Anova(s_w_m1)
par(mfrow = c(2, 2))
plot(s_w_m1)
qqnorm(resid(s_w_m1))
qqline(resid(s_w_m1))




# figure ------------------------------------------------------------------


# summary df
summary_div <- div_2016 %>% 
  filter(treatment != "No.shelter" & herb == "Control") %>%       # remove no.shelter
  mutate(month = as.numeric(as.character(month))) %>%             # change month from factor to numeric
  gather(key = variable, value = value, H, S, J) %>%              # reshape df to "long" format
  group_by(year, month, treatment, variable) %>%                  # summary for each group
  summarise_each(funs(M = mean, SE = se, N = get_n), value) %>%   # get mean, SE, sample size for each group
  mutate(season = paste(year, month.abb[month], sep = "-"))       # create new label (e.g. 2014-Oct)


# create figures
fig_div <- dlply(summary_div, .(variable), function(x){
  ggplot(data = x, aes(x = season, y = M, fill = treatment)) +    # change fill colors by treatment
    labs(x = "Time") +                                            # label for x axis
  
                                                                  # make a main plot
    geom_bar(stat = "identity", position = position_dodge(.9)) +  # create bargrph. Each bar is placed next to each other
    geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .5,  # add error bars
                  position = position_dodge(.9), size = .4) +  
                                                                  # set figure formatting  
    science_theme +
    theme(legend.position  = "none")                              # remove legend
})
fig_div[[1]]


# add legend
fig_div[[1]] <- fig_div[[1]] + 
  ylim(c(0, 2.5)) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position  = c(.8, .85),
        legend.key.width = unit(.2, "inches"))
fig_div[[1]]

# edit y labels
ylabs <- c(expression(Diversity~(italic("H'"))),
           expression(Evenness~(italic("J'"))),
           expression(Species~richness~(italic(S))))

for (i in 1:3){
  fig_div[[i]] <- fig_div[[i]] + labs(y = ylabs[i])
}
fig_div[[3]]


# edit x labels
for(i in 1:2){
  fig_div[[i]] <- fig_div[[i]] +
    scale_x_discrete(labels = NULL) +
    labs(x = NULL)
}


# remove subplot labels
for(i in 2:3){
  fig_div[[i]] <- fig_div[[i]] +
    theme(strip.text.x = element_blank())
}

# merge plots
div_plot_merged <- rbind(ggplotGrob(fig_div[[1]]), 
                         ggplotGrob(fig_div[[2]]), 
                         ggplotGrob(fig_div[[3]]), 
                         size = "last")


# plot
grid.newpage()
grid.draw(div_plot_merged)


# save
ggsavePP(filename = "Output/Figs/diversity_ind", plot = div_plot_merged,
         width = 6.5, height = 6.5)



# 
# div_m_list <- list(H = h_m1, S = s_m1)
# 
# 
# # post-hoc test
# div_posthoc <- ldply(div_m_list, function(x) {
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
# # summary df
# summary_div <- div_2016_ed %>%
#   select(-J) %>%
#   gather(variable, value, H, S) %>%
#   group_by(treatment, variable) %>%
#   summarise_each(funs(M = mean, SE = se, N = get_n), value) %>%
#   left_join(div_posthoc, by = c("treatment", "variable")) %>%
#   ungroup() %>%
#   mutate(treatment = factor(treatment, levels = c("Ambient", "Increased", "Drought",
#                                                   "Pulsed drought", "Seasonal")))
# 
# 
# # plot
# div_fig_list <- dlply(summary_div, .(variable), function(x){
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
# div_fig_list[[1]] <- div_fig_list[[1]] + labs(y = expression(Diversity~(italic("H'"))))
# div_fig_list[[2]] <- div_fig_list[[2]] + labs(y = "Species richness")
# 
# 
# # save
# l_ply(names(div_fig_list), function(x){
#   ggsavePP(filename = paste0("Output/Figs/", x, "_2016"),
#            plot = div_fig_list[[x]], width = 6, height = 4)
# })
