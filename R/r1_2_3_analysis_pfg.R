summary(pfg_2016)


# prepare df --------------------------------------------------------------

pfg_2016_ed     <- filter(pfg_2016, treatment != "No.shelter")                    # remove no shelter
pfg_by_rxh_s    <- filter(pfg_2016_ed, season == "Summer" & treatment %in% c("Reduced frequency", "Ambient", "Reduced"))  # test rainfall x herb; summer
pfg_by_rxh_w    <- filter(pfg_2016_ed, season == "Winter" & treatment %in% c("Reduced frequency", "Ambient", "Reduced"))  # test rainfall x herb; winter
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
summary_pfg <- filter(anv_psthc_rslt, variable %in% c("c34ratios", "grprop"))


## plot
pfg_figs <- dlply(summary_pfg, .(variable), function(x){
  ggplot(x, aes(x = year, y = response, fill = treatment)) +
    facet_wrap( ~ season, scales = "free") +
    labs(x = "", y = "") +
    
    geom_bar(stat = "identity", position = position_dodge(.9)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .5, 
                  position = position_dodge(.9), size = .4) +
    geom_text(aes(y = upper.CL, label = symbols), position = position_dodge(.9),
              vjust = -.4, size = 2) +
    geom_blank(aes(y = (upper.CL) * 1.03)) +  # adjust ymax so that symbols above error bars are placed wihtin a plot
    
    scale_fill_manual(values = rain_cols)+
    science_theme +
    theme(legend.position = "none")
})
pfg_figs[[1]]


## legend
pfg_figs[[1]] <- pfg_figs[[1]] + 
  theme(legend.position  = "top")


## ylim
pfg_figs[[2]] <- pfg_figs[[2]] + ylim(0, 1)


## ylabs
unique(summary_pfg$variable)
pfg_figs[[1]] <- pfg_figs[[1]] + labs(x = NULL, y = expression(C[3]:C[4]~ratios))
pfg_figs[[2]] <- pfg_figs[[2]] + labs(x = "Year", y = "Grass proportion")


## remove xaxis tick labels from the top plot
pfg_figs[[1]] <- pfg_figs[[1]] + theme(axis.text.x = element_blank())
pfg_figs[[1]]


## remove facet_wrap label rom the bottom plot
pfg_figs[[2]] <- pfg_figs[[2]] + theme(strip.text.x = element_blank())
pfg_figs[[2]]


## merge fig
pfg_fig_merged <- rbind(ggplotGrob(pfg_figs[[1]]), 
                        ggplotGrob(pfg_figs[[2]]),
                        size = "last")
grid.newpage()
grid.draw(pfg_fig_merged)

ggsavePP(filename = "Output/Figs/pfg_prop",plot = pfg_fig_merged, 
         width = 6.5, height = 6.5 * 2 / 3)

