# parepare dataframe for analysis -----------------------------------------

div_2016_ed         <- filter(div_2016, treatment != "No.shelter")  # remove shelteered  
div_2016_ed_s       <- filter(div_2016_ed, season == "Summer")      # test Rainfall; summer; complete dataset
div_2016_ed_w       <- filter(div_2016_ed, season == "Winter")      # test Rainfall; winter; complete dataset
div_2016_ed_s_hcont <- filter(div_2016_ed_s, herb == "Control")     # test Rainfall; summer; contrl-herb. subset of data only with 'Control' herb. This will be used to test only rainfall treatments when herb has a significant effect
div_2016_by_rxh_s   <- filter(div_2016_ed_s, treatment %in% c("Reduced frequency", "Ambient", "Reduced"))  # test Rainfall x Herb; summer; subset rainfall treatments
div_2016_by_rxh_w   <- filter(div_2016_ed_w, treatment %in% c("Reduced frequency", "Ambient", "Reduced"))  # test Rainfall x Herb: winter




# analysis ----------------------------------------------------------------


# > Diversity (H) ---------------------------------------------------------


# >> summer ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------

create_trans_boxplot(H ~ treatment * herb * year, data = div_2016_by_rxh_s)
h_by_rh_s_m1 <- lmer(H ~ year * treatment * herb + (1|plot), 
                     data = div_2016_by_rxh_s)
h_by_rh_s_m_fin <- get_persim_lmer(h_by_rh_s_m1)
Anova(h_by_rh_s_m_fin, test.statistic = "F")
plot(h_by_rh_s_m_fin)
qqnorm(resid(h_by_rh_s_m_fin))
qqline(resid(h_by_rh_s_m_fin))
  # small indication of herb effects. aruguable. 




# . rainfall -------------------------------------------------------------------


# the above analysis showed no significnat herb effect so use the complete
# dataset
create_trans_boxplot(H ~ treatment * year, data = div_2016_ed_s)
h_s_m1 <- lmer(H ~ treatment * year + (1|plot), data = div_2016_ed_s)
h_s_m_fin <- get_persim_lmer(h_s_m1)
Anova(h_s_m_fin, test.statistic = "F")
plot(h_s_m_fin)
qqnorm(resid(h_s_m_fin))
qqline(resid(h_s_m1))




# >> winter ------------------------------------------------------------------


# . rain x herb -----------------------------------------------------------


create_trans_boxplot(H ~ treatment * herb * year, data = div_2016_by_rxh_w)
h_by_rh_w_m1 <- lmer(H ~ treatment * herb * year + (1|plot), 
                     data = div_2016_by_rxh_w)

h_by_rh_w_m_fin <- get_persim_lmer(h_by_rh_w_m1)
Anova(h_by_rh_w_m_fin)
qqnorm(resid(h_by_rh_w_m_fin))
qqline(resid(h_by_rh_w_m_fin))
# no herbivore effect




# . rainfall --------------------------------------------------------------


# the above analysis showed no herb effect, so use the complete dataset
create_trans_boxplot(H ~ treatment * year, data = div_2016_ed_w)
h_w_m1 <- lmer(H ~ treatment * year + (1|plot), data = div_2016_ed_w)
h_w_m_fin <- get_persim_lmer(h_w_m1)
Anova(h_w_m_fin, test.statistic = "F")
qqnorm(resid(h_w_m_fin))
qqline(resid(h_w_m_fin))



# > Evenness --------------------------------------------------------------


# >> summer ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(J ~ treatment * herb * year, data = div_2016_by_rxh_s)
j_by_rh_s_m1 <- lmer(J ~ year * treatment * herb + (1|plot), data = div_2016_by_rxh_s)
j_by_rh_s_m_fin <- get_persim_lmer(j_by_rh_s_m1)
Anova(j_by_rh_s_m_fin, test.statistic = "F")
plot(j_by_rh_s_m_fin)
qqnorm(resid(j_by_rh_s_m_fin))
qqline(resid(j_by_rh_s_m_fin))
# herbxtreatment effect. So analyse rainfall and herb separately




# . rainfall --------------------------------------------------------------


# the above analysis showed herb effect so use the daset withought herb-added
create_trans_boxplot(J ~ treatment * year, data = div_2016_ed_s_hcont)
j_s_m1 <- lmer(J ~ treatment * year + (1|plot), data = div_2016_ed_s_hcont)
j_s_m_fin <- get_persim_lmer(j_s_m1)
summary(j_s_m_fin)
plot(j_s_m_fin)
qqnorm(resid(j_s_m_fin))
qqline(resid(j_s_m_fin))




# >> winter ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(J ~ treatment * herb * year, data = div_2016_by_rxh_w)
j_by_rh_w_m1 <- lmer(I(J^2) ~ treatment * herb * year + (1|plot),
                     data = div_2016_by_rxh_w)
j_by_rh_w_m_fin <- get_persim_lmer(j_by_rh_w_m1, show.model.res = TRUE)
Anova(j_by_rh_w_m_fin, test.statistic = "F")
plot(j_by_rh_w_m_fin)
qqnorm(resid(j_by_rh_w_m_fin))
qqline(resid(j_by_rh_w_m_fin))
# no herbivore effect




# . rainfall -------------------------------------------------------------


# the above anlaysis showed no herb effect so use the complete dataset
create_trans_boxplot(J ~ treatment * year, data = div_2016_ed_w)
j_w_m1 <- lmer(I(J^2) ~ treatment * year + (1|plot), data = div_2016_ed_w)
j_w_m_fin <- get_persim_lmer(j_w_m1)
Anova(j_w_m_fin, test.statistic = "F")
plot(j_w_m_fin)
qqnorm(resid(j_w_m_fin))
qqline(resid(j_w_m_fin))




# > species richness --------------------------------------------------------


# >> summer ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(S ~ treatment * herb * year, data = div_2016_by_rxh_s)
s_by_rh_s_m1 <- lmer(S ~ year * treatment * herb + (1|plot), data = div_2016_by_rxh_s)
s_by_rh_s_m_fin <- get_persim_lmer(s_by_rh_s_m1)

Anova(s_by_rh_s_m_fin, test.statistic = "F")
plot(s_by_rh_s_m_fin)
qqnorm(resid(s_by_rh_s_m_fin))
qqline(resid(s_by_rh_s_m_fin))
# no herb effect





# . rianfall ----------------------------------------------------------------


# the above analysis showed no herb effect, so use the complete dataset
create_trans_boxplot(S ~ treatment * year, data = div_2016_ed_s)
s_s_m1 <- lmer(S ~ treatment * year + (1|plot), data = div_2016_ed_s)
s_s_m_fin <- get_persim_lmer(s_s_m1)
Anova(s_s_m_fin, test.statistic = "F")
plot(s_s_m_fin)
qqnorm(resid(s_s_m_fin))
qqline(resid(s_s_m_fin))




# >> winter ---------------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(S ~ treatment * herb * year, data = div_2016_by_rxh_w)
s_by_rh_w_m1 <- lmer(S ~ treatment * herb * year + (1|plot), 
                     data = div_2016_by_rxh_w)
s_by_rh_w_m_fin <- get_persim_lmer(s_by_rh_w_m1, show.model.res = TRUE)
Anova(s_by_rh_w_m_fin, test.statistic = "F")
plot(s_by_rh_w_m_fin)
qqnorm(resid(s_by_rh_w_m_fin))
qqline(resid(s_by_rh_w_m_fin))
# no herbivore effect




# . rainfall -------------------------------------------------------------


# the above anlaysis showed no herb effect so use the complete dataset
create_trans_boxplot(S ~ treatment * year, data = div_2016_ed_w)
s_w_m1 <- lmer(S ~ treatment * year + (1|plot), data = div_2016_ed_w)
s_w_m_fin <- get_persim_lmer(s_w_m1)
Anova(s_w_m_fin, test.statistic = "F")
plot(s_w_m_fin)
qqnorm(resid(s_w_m_fin))
qqline(resid(s_w_m_fin))




# figure ------------------------------------------------------------------


# summary df
summary_div <- div_2016 %>% 
  filter(treatment != "No.shelter" & herb == "Control") %>%       # remove no.shelter
  mutate(month = as.numeric(as.character(month))) %>%             # change month from factor to numeric
  gather(key = variable, value = value, H, S, J) %>%              # reshape df to "long" format
  group_by(year, season, treatment, variable) %>%                 # summary for each group
  summarise_each(funs(M = mean, SE = se, N = get_n), value) %>%   # get mean, SE, sample size for each group
  left_join(anv_psthc_rslt, by = c("year", "season", "treatment", "variable"))

# create figures
fig_div <- dlply(summary_div, .(variable), function(x){
  ggplot(data = x, aes(x = year, y = M, fill = treatment)) +      # change fill colors by treatment
    labs(x = "Year") +                                            # label for x axis
    facet_grid(. ~ season, scales = "free_x", space = "free_x") +
                                                                  # make a main plot
    geom_bar(stat = "identity", position = position_dodge(.9)) +  # create bargrph. Each bar is placed next to each other
    geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .5,  # add error bars
                  position = position_dodge(.9), size = .4) +  
    geom_text(aes(y = M + SE, label = symbols), position = position_dodge(.9),
              vjust = -.4, size = 2) +
    
                                                                  # set figure formatting  
    science_theme +
    scale_fill_manual(values = rain_cols) +
    theme(legend.position  = "none")                              # remove legend
})
fig_div[[1]]


# add legend
fig_div[[1]] <- fig_div[[1]] + 
  theme(legend.position  = "top")
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
