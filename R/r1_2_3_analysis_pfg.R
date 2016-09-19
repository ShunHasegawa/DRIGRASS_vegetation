summary(pfg_2016)

# remove no shelter
pfg_2016_ed <- filter(pfg_2016, treatment != "Ambient(no shelter)")




# analysis ----------------------------------------------------------------


# . c3 ratios -------------------------------------------------------------


# test rain x herb
pfg_rxh <- pfg_2016_ed %>% 
  filter(treatment %in% c("Pulsed drought", "Ambient", "Drought")) %>% 
  droplevels(.)


create_trans_boxplot(c3ratio ~ treatment * herb, data = pfg_rxh)
par(mfrow = c(1, 2))
boxplot(log(c3ratio) ~ treatment * herb, data = pfg_2016_ed, main = "log")
boxplot(logit(c3ratio) ~ treatment * herb, data = pfg_2016_ed, main = "logit")


# use log
c3r_m1 <- lm(log(c3ratio) ~ treatment * herb, data = pfg_rxh)
c3r_m2 <- step(c3r_m1)
anova(c3r_m2)

par(mfrow = c(2, 2))
plot(c3r_m2)


# remove oulier
c3r_m3 <- update(c3r_m1, subset = -which.max(pfg_rxh$c3ratio))
anova(c3r_m3)
plot(c3r_m3)
# pretty much the same result as above



# . rainfall treatment ----------------------------------------------------


# as there is rainfall x herb interaction, use only herb == Ambient

pfg_rain <- pfg_2016_ed %>% 
  filter(herb == "Ambient") %>% 
  droplevels(.)

create_trans_boxplot(c3ratio ~ treatment, data = pfg_rain)

c3r_by_r_m1 <- lm(logit(c3ratio) ~ treatment, data = pfg_rain)
par(mfrow = c(2, 2))
plot(c3r_by_r_m1)
anova(c3r_by_r_m1)
visreg(c3r_by_r_m1)




# figure ------------------------------------------------------------------


# post-hoc test
summary(glht(c3r_by_r_m1, linfct = mcp(treatment = "Tukey")))
symbols <- cld(glht(c3r_by_r_m1, linfct = mcp(treatment = "Tukey")))$mcletters$Letters # symbols to be used for figures given by post-hoc test
c3r_posthoc <- data.frame(treatment = names(symbols), 
                          symbols = as.character(symbols), row.names = NULL)

# summary df
summary_pfg <- pfg_rain %>% 
  select(-total, -c4grass) %>% 
  gather(variable, value, c3ratio) %>% 
  group_by(treatment, variable) %>% 
  summarise_each(funs(M = mean, SE = se, N = get_n), value) %>% 
  left_join(c3r_posthoc, by = "treatment") %>% 
  ungroup() %>% 
  mutate(treatment = factor(treatment, levels = c("Ambient", "Increased", "Drought", 
                                                  "Pulsed drought", "Seasonal")))


# plot
fig_c3r <- ggplot(summary_pfg, aes(x = treatment, y = M, fill = treatment)) +
  labs(x = NULL, y = "C3 ratios") +
  
  geom_bar(stat = "identity", col = "black") +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .3) +
  geom_text(aes(y = M + SE, label = symbols), vjust = -.4) +
  
  scale_x_discrete(labels = c("Ambient", "Increased\n(+50%)", "Reduced\n(-50%)",
                              "Reduced\nfreaquency", "Summer\ndrought")) +
  theme(legend.position = "none",
        panel.border      = element_rect(color = "black"),
        panel.grid.major  = element_blank(), 
        panel.grid.minor  = element_blank())


# save
ggsavePP(filename = "Output/Figs/C3ratios_2016", 
         plot = fig_c3r, width = 6, height = 4)


