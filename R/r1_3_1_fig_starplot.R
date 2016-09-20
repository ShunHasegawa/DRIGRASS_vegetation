

# prepare df --------------------------------------------------------------

summary(sp_biom_2016)

# species to be plotted
plot_spp <- c("Axonopus.affinis","Cympobogon.refractus", "Cynodon.dactlyon",
              "Digitaria.sp","Eragrostis.curvula","Hypochaeris.radicata", 
              "Microlaena.stipoides", "Paspalum.dilitatum","Setaria.parviflora")


# treatment mean
treat_summary <- sp_biom_2016 %>% 
  filter(treatment != "Ambient(no shelter)" & herb == "Ambient") %>%              # remove non-sheltered and added herb
  select(treatment, one_of(plot_spp)) %>%                                         # select required columns
  mutate(treatment = factor(treatment,                                            # change treatment labels
                            labels = c("Ambient", "Increased", "Reduced", 
                                       "Reduced frequency", "Summer drought"))) %>% 
  group_by(treatment) %>%                                                         # grouping
  summarise_each(funs(sum), -treatment)                                           # get sum for each species (i.e. all columns except treatment)



# figure ------------------------------------------------------------------


spcol <- brewer.pal(9, "RdYlGn")                                              # color palette
loc   <- matrix(data = c(0, 6, 3, 6, 6, 6, 0, .5, 6, .5), ncol = 2, byrow = T)  # layout of each subplot

# species labels
str_lab <- sapply(strsplit(plot_spp, split = "[.]"),                       # split character by period (.)
                  function(x){           
                    parse(text = paste0("italic(", x[1], "~", x[2], ")"))  # create expression objects
                  })

# star plot
plot_starplot <- function(){
  stars(treat_summary[,-1], location = loc, key.loc=c(3,3),         
        labels = treat_summary$treatment, key.labels = str_lab,
        col.segments = spcol, col.stars = treatcol, frame.plot=FALSE,
        main = "March 2016 biomass", draw.segment = TRUE, cex = 1,lwd = 2)
}

save_png600(filename = "Output/Figs/star_plot_2016.png", width = 6, height = 6)
plot_starplot()
dev.off()
