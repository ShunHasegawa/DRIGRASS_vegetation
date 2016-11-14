summary(sp_biom_2016)

# analyse only specie with >5% coverage
spp_cov      <- colSums(sp_biom_2016[, spp_2016]) / sum(sp_biom_2016[, spp_2016]) # % biomass in total biomass
dominent_spp <- names(spp_cov)[spp_cov > 0.05]                                    # spp wich >5%


# use only dominent spp, and remove no shelter plot
sp_biom_2016_ed <- sp_biom_2016 %>% 
  select(treatment, herb, one_of(dominent_spp)) %>% 
  filter(treatment != "Ambient(no shelter)")





# analysis ----------------------------------------------------------------



# > rain x herb -------------------------------------------------------------


# df to test rain x herb

sp_biom_by_rxh <- sp_biom_2016_ed %>% 
  filter(treatment %in% c("Reduced.frequency", "Ambient", "Reduced")) %>% 
  droplevels(.)

summary(sp_biom_by_rxh)
create_trans_boxplot(Cympobogon.refractus + 1 ~ treatment * herb, data = sp_biom_by_rxh)
create_trans_boxplot(Eragrostis.curvula   + 1 ~ treatment * herb, data = sp_biom_by_rxh)
create_trans_boxplot(Paspalum.dilitatum   + 1 ~ treatment * herb, data = sp_biom_by_rxh)
create_trans_boxplot(Setaria.parviflora   + 1 ~ treatment * herb, data = sp_biom_by_rxh)

# remove one outlier from Setaria.parviflora 
create_trans_boxplot(Setaria.parviflora   + 1 ~ treatment * herb,
                     data = sp_biom_by_rxh[-which.max(sp_biom_by_rxh$Setaria.parviflora), ])
sp_biom_by_rxh$Setaria.parviflora[which.max(sp_biom_by_rxh$Setaria.parviflora)] <- NA


# list of formulas
f_list <- llply(paste0("log(", dominent_spp, "+ 1) ~ treatment * herb"), as.formula)     


# list of models
m_list <- llply(f_list, function(x) lm(x, data = sp_biom_by_rxh, na.action = "na.omit")) 


# anova
llply(m_list, anova)


# model diagnosis
par(mfrow = c(4, 4))
l_ply(m_list, plot)

# no interaction or herbivore effect


# > rain ------------------------------------------------------------------

create_trans_boxplot(Cympobogon.refractus + 1 ~ treatment * herb, data = sp_biom_2016_ed)
create_trans_boxplot(Eragrostis.curvula   + 1 ~ treatment * herb, data = sp_biom_2016_ed)
create_trans_boxplot(Paspalum.dilitatum   + 1 ~ treatment * herb, data = sp_biom_2016_ed)
create_trans_boxplot(Setaria.parviflora   + 1 ~ treatment * herb, data = sp_biom_2016_ed)
sp_biom_2016_ed$Setaria.parviflora[which.max(sp_biom_2016_ed$Setaria.parviflora)] <- NA


# list of formulas
f_rain_list <- llply(paste0("log(", dominent_spp, "+ 1) ~ treatment"), as.formula)     
names(f_rain_list) <- dominent_spp


# list of models
m_rain_list <- llply(f_rain_list, function(x) lm(x, data = sp_biom_2016_ed, na.action = "na.omit")) 


# anova
llply(m_rain_list, anova)


# model diagnosis
par(mfrow = c(4, 4))
l_ply(m_rain_list, plot)




# figures -----------------------------------------------------------------


# post-hoc test
spp_posthoc <- ldply(m_rain_list, function(x) {
  
  # symbols to be used for figures given by post-hoc test
  symbols <- cld(glht(x, linfct = mcp(treatment = "Tukey")), decreasing = TRUE)$mcletters$Letters 
  d <- data.frame(treatment = names(symbols), symbols, row.names = NULL)
  d$symbols <- as.character(d$symbols)
  return(d)
},
.id = "variable")

# there's treatment effect only for Cympobogon.refractus
spp_posthoc$symbols[spp_posthoc$variable != "Cympobogon.refractus"] <- ""


# summary df
summary_spp <- sp_biom_2016_ed %>% 
  gather(variable, value, one_of(dominent_spp)) %>% 
  group_by(treatment, variable) %>% 
  summarise_each(funs(M  = mean(., na.rm = TRUE), 
                      SE = se(., na.rm = TRUE), 
                      N  = get_n), value) %>% 
  left_join(spp_posthoc, by = c("treatment", "variable")) %>% 
  ungroup() %>% 
  mutate(treatment = factor(treatment, levels = c("Ambient", "Increased", "Reduced", 
                                                  "Reduced.frequency", "Summer.drought")),
         variable  = gsub("[.]", " ", as.character(variable))) %>% 
  arrange(variable)


# plot
sp_plot <- ggplot(summary_spp, aes(x = treatment, y = M, fill = treatment)) +
  labs(x = NULL, y = "Biomass (g)") +
  facet_wrap(~ variable) +
  
  
  geom_bar(stat = "identity", col = "black") +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .3) +
  geom_text(aes(y = M + SE, label = symbols), vjust = -.4) +
  
  scale_x_discrete(labels = c("Ambient", "Increased\n(+50%)", "Reduced\n(-50%)",
                              "Reduced\nfrequency", "Summer\ndrought")) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_manual(values = rain_cols) +
  
  theme(legend.position = "none",
        panel.border      = element_rect(color = "black"),
        panel.grid.major  = element_blank(), 
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(size = 9),
        strip.text        = element_text(face = "italic"))

ggsavePP(filename = "Output/Figs/dominent_spp_2016", plot = sp_plot,
         width = 6.5, height = 6.5)
