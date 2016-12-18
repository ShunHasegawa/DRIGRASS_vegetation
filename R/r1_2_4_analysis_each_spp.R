summary(ab_biom)

# analyse only dominent spp
dominent_spp <- c("Axonopus.fissifolius","Cymbopogon.refractus", "Cynodon.dactlyon", 
                  "Digitaria.sp","Eragrostis.curvula","Hypochaeris.radicata", 
                  "Microlaena.stipoides", "Paspalum.dilitatum","Setaria.parviflora")

# check sum for each season
ab_biom %>% 
  group_by(year, season) %>% 
  summarise_each(funs(sum), one_of(dominent_spp))
## remove Digitaria.sp in the winter 2013


# use only dominent spp, and remove no shelter plot
ab_biom_ed <- ab_biom %>% 
  select(year, month, season, treatment, plot, herb, one_of(dominent_spp)) %>% 
  filter(treatment != "Ambient(no shelter)")




# analysis ----------------------------------------------------------------



# rain x herb

ab_biom_ed %>% 
  filter(treatment %in% c("Reduced frequency", "Ambient", "Reduced")) %>% 
  droplevels(.) %>% 
  gather(variable, value, one_of(dominent_spp)) %>% 
  mutate(ys = paste(year, season, sep = "_")) %>% 
ggplot(data = ., aes(x = treatment, y = log(value + 1), col = herb)) +
  geom_boxplot() +
  facet_grid(variable ~ ys, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# rain

ab_biom_ed %>% 
  filter(treatment != "No.shelter") %>% 
  droplevels(.) %>% 
  gather(variable, value, one_of(dominent_spp)) %>% 
  mutate(ys = paste(year, season, sep = "_")) %>% 
  ggplot(data = ., aes(x = treatment, y = log(value + 1))) +
  geom_boxplot() +
  facet_grid(variable ~ ys, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# figure ------------------------------------------------------------------



# . prepare df --------------------------------------------------------------

summary(ab_biom)

# treatment mean
treat_summary <- ab_biom %>% 
  filter(treatment != "No.shelter") %>%                       # remove non-sheltered
  select(year, season, treatment, one_of(dominent_spp)) %>%  # select required columns
  group_by(treatment, year, season) %>%                      # grouping
  summarise_each(funs(mean(., na.rm = TRUE)), -treatment) %>%                  # get mean for each species (i.e. all columns except treatment)
  ungroup() %>% 
  mutate(treatment = dplyr::recode(treatment, 
                            Ambient             = "AMB",
                            Increased           = "IR",
                            Reduced             = "RR",
                            `Reduced frequency` = "RF",
                            `Summer drought`    = "SD"))


# . plot ------------------------------------------------------------------


spcol <- brewer.pal(9, "RdYlGn")                                                # color palette
loc   <- matrix(data = c(0, 6, 3, 6, 6, 6, 0, .5, 6, .5), ncol = 2, byrow = T)  # layout of each subplot

# species labels
str_lab <- sapply(strsplit(dominent_spp, split = "[.]"),                       # split character by period (.)
                  function(x){           
                    parse(text = paste0("italic(",substr(x[1], 1, 1),          # create expression objects
                                        ".", x[2], ")"))  
                  })

# star plot

plot_starplot <- function(){
  par(mfrow = c(2, 3))
  d_ply(treat_summary, .(season, year), function(x){
    figtitle <- paste(unique(x$season), unique(x$year))
    stars(x[,c(-1:-3)], location = loc, key.loc=c(3,3),         
          labels = x$treatment, key.labels = str_lab,
          col.segments = spcol, col.stars = treatcol, frame.plot=TRUE, 
          main = figtitle, draw.segment = TRUE, cex = 1,lwd = 1)
  })
}


save_png600(filename = "Output/Figs/star_plot.png", width = 6.5, height = 5)
plot_starplot()
dev.off()

pdf(file = "Output/Figs/star_plot.pdf", width = 6.5, height = 5)
plot_starplot()
dev.off()

