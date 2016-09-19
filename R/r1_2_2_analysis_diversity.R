summary(div_2016)


# remove shelteered
div_2016_ed <- filter(div_2016, treatment != "Ambient(no shelter)")


# df to test rain x herb
div_2016_by_rxh <- div_2016_ed %>% 
  filter(treatment %in% c("Pulsed drought", "Ambient", "Drought")) %>% 
  droplevels(.)


# analysis ----------------------------------------------------------------


# > Diversity (H) ---------------------------------------------------------


# . rain x herb -------------------------------------------------------------

create_trans_boxplot(H ~ treatment * herb, data = div_2016_by_rxh)
h_by_rh_m1 <- lm(H ~  treatment * herb, data = div_2016_by_rxh)
anova(h_by_rh_m1)
par(mfrow = c(2, 2))
plot(h_by_rh_m1)
# no interaction or herbivore effect




# . rainfall --------------------------------------------------------------

create_trans_boxplot(H ~ treatment * herb, data = div_2016_ed)
h_m1 <- lm(H ~ treatment, data = div_2016_ed)
anova(h_m1)
par(mfrow = c(2, 2))
plot(h_m1)




# > species richness --------------------------------------------------------


# . rain x herb -------------------------------------------------------------


create_trans_boxplot(S ~ treatment * herb, data = div_2016_by_rxh)

# plot mean vs var
div_2016_by_rxh %>% 
  group_by(treatment, herb) %>% 
  summarise_each(funs(M = mean, V = var), S) %>% 
  plot(V ~ M, data = .)
 # no obvious pattern

s_by_rh_m1 <- lm(log(S) ~ treatment * herb, data = div_2016_by_rxh)
anova(s_by_rh_m1)
par(mfrow = c(2, 2))
plot(s_by_rh_m1)
# no interaction or herbivore effect




# . rain --------------------------------------------------------------------

create_trans_boxplot(S ~ treatment, data = div_2016_ed)
s_m1 <- lm(sqrt(S) ~ treatment, data = div_2016_ed)
anova(s_m1)
par(mfrow = c(2, 2))
plot(s_m1)
visreg(s_m1)




# figure ------------------------------------------------------------------

div_m_list <- list(H = h_m1, S = s_m1)


# post-hoc test
div_posthoc <- ldply(div_m_list, function(x) {
  symbols <- cld(glht(x, linfct = mcp(treatment = "Tukey")))$mcletters$Letters # symbols to be used for figures given by post-hoc test
  d <- data.frame(treatment = names(symbols), symbols, row.names = NULL)
  d$symbols <- as.character(d$symbols)
  return(d)
},
.id = "variable")


# summary df
summary_div <- div_2016_ed %>% 
  select(-J) %>% 
  gather(variable, value, H, S) %>% 
  group_by(treatment, variable) %>% 
  summarise_each(funs(M = mean, SE = se, N = get_n), value) %>% 
  left_join(div_posthoc, by = c("treatment", "variable")) %>% 
  ungroup() %>% 
  mutate(treatment = factor(treatment, levels = c("Ambient", "Increased", "Drought", 
                                                  "Pulsed drought", "Seasonal")))


# plot
div_fig_list <- dlply(summary_div, .(variable), function(x){
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
div_fig_list[[1]] <- div_fig_list[[1]] + labs(y = expression(Diversity~(italic("H'"))))
div_fig_list[[2]] <- div_fig_list[[2]] + labs(y = "Species richness")


# save
l_ply(names(div_fig_list), function(x){
  ggsavePP(filename = paste0("Output/Figs/", x, "_2016"), 
           plot = div_fig_list[[x]], width = 6, height = 4)
})