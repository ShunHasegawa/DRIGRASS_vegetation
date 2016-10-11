rm(list = ls(all = TRUE))
source("R/packages.R")
source("R/functions.R")

# process data
source("R/r1_process_data.R")
summary(spp_names)    # species names in 2016
summary(ab_tot_biom)  # df for total/live/dead biomass
summary(ab_spp_biom)  # df for each spp
summary(div_2016)     # df for diversity indices
# summary(pfg_2016)       # df for plant functional groups (PFG)


# summary table
source("R/r1_1_create_summary_tbl.R")


# stat and figure
theme_set(theme_bw())                                                 # set ggplot backgroud
rain_cols <- c("green4", "dodgerblue3", "red", "palegreen", "plum3")  # colors to be used to plot rainfall treatments

source("R/r1_2_1_analysis_total_biomass.R")  # anlaysis for total/live/dead biomass
source("R/r1_2_2_analysis_diversity.R")      # analysis for diversity indices
source("R/r1_2_3_analysis_pfg.R")            # analysis for pfg ratios
source("R/r1_2_4_analysis_each_spp.R")       # analysis for dominent spp
source("R/r1_2_5_analysis_NMDS.R")           # Community composition (NMDS and PCA)

 
# figures
source("R/r1_3_1_fig_starplot.R")  # starplot


# save
save.image(file = "Output/Data/all_obj.RData")
