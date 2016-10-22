summary(pfg_2016)


# prepare df --------------------------------------------------------------

pfg_2016_ed     <- filter(pfg_2016, treatment != "No.shelter")                    # remove no shelter
pfg_by_rxh_s    <- filter(pfg_2016_ed, season == "Summer" & treatment %in% c("Pulsed.drought", "Ambient", "Drought"))  # test rainfall x herb; summer
pfg_by_rxh_w    <- filter(pfg_2016_ed, season == "Winter" & treatment %in% c("Pulsed.drought", "Ambient", "Drought"))  # test rainfall x herb; winter
pfg_ed_s        <- filter(pfg_2016_ed, season == "Summer")                        # test rainfall; summer; complete dataset
pfg_ed_w        <- filter(pfg_2016_ed, season == "Winter")                        # test rainfall; winter; complete dataset
pfg_ed_s_contrh <- filter(pfg_2016_ed, season == "Summer" & herb == "Control")    # test rainfall; summer; subset control-herb
pfg_ed_w_contrh <- filter(pfg_2016_ed, season == "Winter" & herb == "Control")    # test rainfall; winter; subset control-herb





# analysis ----------------------------------------------------------------

source("R/r1_2_3.1_c3prop.R")     # c3 ratios (c3 proportion); C3 plant / Total 
source("R/r1_2_3.2_c34ratio.R")   # c34ratios; C3 plant / C4 grass
source("R/r1_2_3.3_grassprop.R")  # Grass proportion; Grass / Total




# figure ------------------------------------------------------------------


## summary df
summary_pfg <- pfg_2016_ed %>%
  gather(key = variable, value = value, c3ratio, c34ratios, grprop) %>% 
  group_by(year, season, treatment, variable) %>%
  summarise_each(funs(M = mean, SE = se, N = get_n), value)


## plot
pfg_figs <- dlply(summary_pfg, .(variable), function(x){
  ggplot(x, aes(x = year, y = M, fill = treatment)) +
    facet_grid(. ~ season) +
    labs(x = "", y = "") +
    
    geom_bar(stat = "identity", position = position_dodge(.9)) +
    geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .5, 
                  position = position_dodge(.9), size = .4) +
    science_theme +
    theme(legend.position = "none")
}
)
pfg_figs[[1]]


## legend
pfg_figs[[1]] <- pfg_figs[[1]] + 
  theme(legend.position  = c(.15, .75),
        legend.key.width = unit(.2, "inches"))

## ylabs
unique(summary_pfg$variable)
pfg_figs[[1]] <- pfg_figs[[1]] + labs(x = "", y = expression(C[3]:C[4]~ratios))
pfg_figs[[2]] <- pfg_figs[[2]] + labs(x = "", y = expression(C[3]~proportion))
pfg_figs[[3]] <- pfg_figs[[3]] + labs(x = "Time", y = "Grass proportion")


## remove xaxis tick labels from the top two plots
for(i in 1:2){
  pfg_figs[[i]] <- pfg_figs[[i]] + 
    theme(axis.text.x = element_blank())
}
pfg_figs[[1]]


## remove facet_grid label rom the bottom two plots
for(i in 2:3){
  pfg_figs[[i]] <- pfg_figs[[i]] + 
    theme(strip.text.x = element_blank())
}
pfg_figs[[2]]


## merge fig
pfg_fig_merged <- rbind(ggplotGrob(pfg_figs[[1]]), 
                        ggplotGrob(pfg_figs[[2]]),
                        ggplotGrob(pfg_figs[[3]]),
                        size = "last")
grid.newpage()
grid.draw(pfg_fig_merged)

ggsavePP(filename = "Output/Figs/pfg_prop",plot = pfg_fig_merged, width = 6, height = 6)



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


