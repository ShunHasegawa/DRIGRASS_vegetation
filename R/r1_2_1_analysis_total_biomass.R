
summary(tld_biom_2016)

# remove not sheltered
tld_biom_2016_ed <- filter(tld_biom_2016, treatment != "Ambient(no shelter)")


# df to test rain x herb interaction
tld_biom_by_rxh <- tld_biom_2016_ed %>% 
  filter(treatment %in% c("Pulsed drought", "Ambient", "Drought")) %>% 
  droplevels(.)


# analysis ----------------------------------------------------------------


# > live --------------------------------------------------------------------


# . rain x herb -----------------------------------------------------------


create_trans_boxplot(Live ~ treatment * herb, data = tld_biom_by_rxh)
live_by_rh_m1 <- lm(sqrt(Live) ~ treatment * herb, data = tld_biom_by_rxh)
anova(live_by_rh_m1)
par(mfrow = c(2, 2))
plot(live_by_rh_m1)
# no interaction or herbivore effect




# . rain ------------------------------------------------------------------


create_trans_boxplot(Live ~ treatment, data = tld_biom_2016_ed)
live_m1 <- lm(sqrt(Live) ~ treatment, data = tld_biom_2016_ed)
anova(live_m1)
par(mfrow = c(2, 2))
plot(live_m1)
visreg(live_m1)




# > total -------------------------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(tot_biomass ~ treatment * herb, data = tld_biom_by_rxh)
tot__by_rh_m1 <- lm(sqrt(tot_biomass) ~ treatment * herb, data = tld_biom_2016_ed)
anova(tot__by_rh_m1)
par(mfrow = c(2, 2))
plot(tot__by_rh_m1)
# no interaction or herbivore effect




# . rain ------------------------------------------------------------------


create_trans_boxplot(tot_biomass ~ treatment, data = tld_biom_2016_ed)
tot_m1 <- lm(sqrt(tot_biomass) ~ treatment, data = tld_biom_2016_ed)
anova(tot_m1)
par(mfrow = c(2, 2))
plot(tot_m1)
visreg(tot_m1)




# > dead --------------------------------------------------------------------


# . rain x herb -----------------------------------------------------------


create_trans_boxplot(Dead ~ treatment * herb, data = tld_biom_by_rxh)
dead_by_rh_m1 <- lm(log(Dead) ~ treatment * herb, data = tld_biom_by_rxh)
anova(dead_by_rh_m1)
par(mfrow = c(2, 2))
plot(dead_by_rh_m1)
# no interaction or herbivore effect




# . rain ------------------------------------------------------------------


create_trans_boxplot(Dead ~ treatment, data = tld_biom_2016_ed)
dead_m1 <- lm(log(Dead) ~ treatment, data = tld_biom_2016_ed)
anova(dead_m1)
par(mfrow = c(2, 2))
plot(dead_m1)
visreg(dead_m1)




# figure ------------------------------------------------------------------

biom_m_list <- list(Live = live_m1, Dead = dead_m1, tot_biomass = tot_m1)


# post-hoc test
biom_posthoc <- ldply(biom_m_list, function(x) {
  symbols <- cld(glht(x, linfct = mcp(treatment = "Tukey")))$mcletters$Letters # symbols to be used for figures given by post-hoc test
  d <- data.frame(treatment = names(symbols), symbols, row.names = NULL)
  d$symbols <- as.character(d$symbols)
  return(d)
},
.id = "variable")


# no treateffect for Dead biomass, so remove symbols
biom_posthoc$symbols[biom_posthoc$variable == "Dead"] <- ""


# create summary df
summary_tld_biom <- tld_biom_2016_ed %>% 
  gather(variable, value, tot_biomass, Dead, Live) %>%
  group_by(treatment, variable) %>% 
  summarise_each(funs(M = mean, SE = se, N = get_n), value) %>% 
  left_join(biom_posthoc, by = c("treatment", "variable")) %>% 
  ungroup() %>% 
  mutate(treatment = factor(treatment, levels = c("Ambient", "Increased", "Drought", 
                                                  "Pulsed drought", "Seasonal")))


# plot
biomass_fig_list <- dlply(summary_tld_biom, .(variable), function(x){
  p <- ggplot(x, aes(x = treatment, y = M, fill = treatment)) +
    labs(x = NULL) +
    
    geom_bar(stat = "identity", col = "black") +
    geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .3) +
    geom_text(aes(y = M + SE, label = symbols), vjust = -.4) +
    
    scale_x_discrete(labels = c("Ambient", "Increased\n(+50%)", "Reduced\n(-50%)",
                                "Reduced\nfreaquency", "Summer\ndrought")) +
    theme(legend.position = "none",
          panel.border      = element_rect(color = "black"),
          panel.grid.major  = element_blank(), 
          panel.grid.minor  = element_blank())
  
  return(p)
})


# edit y labels
biomass_fig_list[[1]] <- biomass_fig_list[[1]] + labs(y = "Dead biomass (g)")
biomass_fig_list[[2]] <- biomass_fig_list[[2]] + labs(y = "Live biomass (g)")
biomass_fig_list[[3]] <- biomass_fig_list[[3]] + labs(y = "Total biomass (g)")


# save as PDF and PNG
l_ply(names(biomass_fig_list), function(x){
  ggsavePP(filename = paste0("Output/Figs/", x, "_2016"), 
           plot     = biomass_fig_list[[x]], width = 6, height = 4)
})
