rm(list = ls(all = TRUE))
source("R/packages.R")
source("R/functions.R")

# process data
source("R/r1_process_data.R")
summary(spp_names)    # species names in 2016
summary(ab_tot_biom)  # df for total/live/dead biomass
summary(ab_spp_biom)  # df for each spp
summary(div_2016)     # df for diversity indices
summary(pfg_2016)     # df for plant functional groups (PFG)


# summary table
source("R/r1_1_create_summary_tbl.R")  # save as csv
    ## Run the function below to save as excel. This may take some time.
    ## get_summary_excel()  

# stat and figure
theme_set(theme_bw())                                                 # set ggplot backgroud
rain_cols <- c("green4", "dodgerblue3", "red", "palegreen", "plum3")  # colors to be used to plot rainfall treatments
options(na.action = "na.fail")


source("R/r1_2_analysis_all_ow_anova.R")   # analysis for all measurements for each harvest in each year by one-way anova
source("R/r1_2.1_example_oneway_anova.R")  # show an example of one-way anova
source("R/r1_2_1_analysis_total_biomass.R")  # anlaysis for total/live/dead biomass
source("R/r1_2_2_analysis_diversity.R")      # analysis for diversity indices
source("R/r1_2_3_analysis_pfg.R")            # analysis for pfg ratios
# source("R/r1_2_4_analysis_each_spp.R")       # analysis for dominent spp
# source("R/r1_2_5_analysis_NMDS.R")           # Community composition (NMDS and PCA)
source("R/r1_4_analysis_MDS.R")
source("R/r1_5_analysis_PRC.R")
source("R/r1_2_6_analysis_community_traits.R")  # analysis for community-plant-traits 


# figures
# source("R/r1_3_1_fig_starplot.R")  # starplot


# save
save.image(file = "Output/Data/all_obj.RData")
