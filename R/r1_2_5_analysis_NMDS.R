
# prepare df --------------------------------------------------------------

summary(sp_biom_2016)


# subset Ambient herbivore, and remove no sheltered plot
vegmatrixtreat <- sp_biom_2016 %>% 
  filter(herb == "Ambient" & treatment != "Ambient(no shelter)") %>% 
  droplevels(.) %>% 
  mutate(treatment = factor(treatment, 
                            levels = c("Ambient", "Increased", "Drought", 
                                       "Pulsed drought", "Seasonal"),
                            labels = c("Ambient", "Increased(+50%)", 
                                       "Reduced(-50%)","Reduced frequency", 
                                       "Summer drought")))


# remov sppeceis with no observations
sp_sum         <- colSums(vegmatrixtreat[, spp_2016])                       # sum for each spp
keep_sp        <- names(sp_sum)[sp_sum > 0]                                 # spp with obs
vegmatrixtreat <- select(vegmatrixtreat, treatment, plot, one_of(keep_sp))  # use only those spp




# perform NMDS ------------------------------------------------------------

orid <- metaMDS(wisconsin(sqrt(vegmatrixtreat[, keep_sp])), k=2, trymax=100, 
                distance="jaccard")


# Check the stress of the NMDS (it is important to evaluate the ordination)
stressplot(orid)


# Save stress in text to place on the plot
stress <- paste("Stress = ", round(orid$stress, digits=2))




# figure ------------------------------------------------------------------


# Build a color pallatte
col <- rev(brewer.pal(8, "BrBG"))[-c(4,5)]

# Graph Oridnation
dev.off()
par(mar = c(4, 5, 4, 2))
plot(orid,type="n", display = "sites", cex.lab = 1.8)                                   # plot ord object
abline(h=0,v=0,lty=2)                                                    # put lines at 0 in X and Y
text(orid, display="sites", labels = as.character(vegmatrixtreat$plot))  # Place plot numbers on graph

# Make ellispes for water treatments
treat_var <- levels(vegmatrixtreat$treatment)
for(i in 1:length(treat_var)){                            
  ordiellipse(orid, vegmatrixtreat$treatment, kind="se", conf=0.95, lwd=2, 
              col = col[i], draw = "polygon", show.groups = treat_var[i])
}

mtext(paste0("NMDS of biomass March 2016 (", stress, ")"), side = 3, cex = 1.8)                            # title
legend("bottomleft", legend = levels(vegmatrixtreat$treatment), text.col = col,  # Place legend on plot 
        bty = "n", cex = 1.5, ncol = 1) 


# save fig as pdf and png
p <- recordPlot()                                                   # store the current plot

# pdf
pdf(file = "Output/Figs/NMDS_2016.pdf", height = 6.5, width = 7)    # set the pdf options to save following as pdf
replayPlot(p)                                                       # replot the stored plot
dev.off()                                                           # Need to set device off for the pdf to save

# png
save_png600(file = "Output/Figs/NMDS_2016.png", height = 6.5, width = 7)
replayPlot(p) # replot the stored plot
dev.off()                             
