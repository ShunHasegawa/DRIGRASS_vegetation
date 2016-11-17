# figure ------------------------------------------------------------------


# summary df

summary_trait <- wghtdavrg_trait %>%
  mutate(month = as.numeric(as.character(month))) %>% 
  filter(treatment != "No.shelter") %>% 
  gather(key = variable, value = value, Forks, sr_ratio, Tips, total_L, total_SA) %>% 
  group_by(year, month, season, treatment, variable) %>% 
  summarise_each(funs(M  = mean(., na.rm = TRUE), SE = se(., na.rm = TRUE), 
                      N  = get_n), value) %>% 
  left_join(anv_psthc_rslt, by = c("year", "season", "treatment", "variable"))


# create plots
fig_trait <- dlply(summary_trait, .(variable), function(x){
  ggplot(data = x, aes(x = year, y = M, fill = treatment)) +
    facet_grid(. ~ season, scales = "free_x", space = "free_x") +
    labs(x = "Year") +
    
    geom_bar(stat = "identity", position = position_dodge(.9)) +
    geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .5,
                  position = position_dodge(.9), size = .4) +
    geom_text(aes(y = M + SE, label = symbols), position = position_dodge(.9),
              vjust = -.4, size = 2) +
    
    science_theme +
    theme(legend.position  = "none") +
    scale_fill_manual(values = rain_cols) 
})
fig_trait[[1]]


# add legend
fig_trait[[1]] <- fig_trait[[1]] + 
  theme(legend.position  = "top")
fig_trait[[1]]


# ylab
names(fig_trait)
ylabs <- c(expression(atop("Root forks", (mg^'-1'))),
           "Shoot:Root",
           expression(atop("Root tips", (mg^'-1'))),
           expression(atop("Root length", (mg~mg^'-1'))),
           expression(atop("Root surface area", (mm^'-2'~mg^'-1'))))


for (i in 1:5){
  fig_trait[[i]] <- fig_trait[[i]] + labs(y = ylabs[i])
}


# xlab
for(i in 1:4){
  fig_trait[[i]] <- fig_trait[[i]] +
    scale_x_discrete(labels = NULL) +
    labs(x = NULL)
}


# remove subplot labels
for(i in 2:5){
  fig_trait[[i]] <- fig_trait[[i]] +
    theme(strip.text.x = element_blank())
}

# merge plots
trait_plot_merged <- rbind(ggplotGrob(fig_trait[[1]]), 
                           ggplotGrob(fig_trait[[2]]), 
                           ggplotGrob(fig_trait[[3]]),
                           ggplotGrob(fig_trait[[4]]),
                           ggplotGrob(fig_trait[[5]]),
                           size = "last")

grid.newpage()
grid.draw(trait_plot_merged)
ggsavePP(filename = "Output/Figs/community_weighted_average_trait", plot = trait_plot_merged,
         width = 6.5, height = 8.5)

