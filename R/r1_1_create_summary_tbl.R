
# create summary tables -----------------------------------------------------


# put all required dfs into a list
all_list <- list('total_biomass'  = ab_tot_biom, 
                 'biomass_bySpp' = ab_spp_biom, 
                 'diversity'      = div_2016, 
                 'biomass_byPfg' = pfg_2016)




# > by rainfall treatments ------------------------------------------------


summary_by_rain_list <- llply(all_list, function(x){
  x %>% 
    select(-plot, -herb) %>%                                   # remove unneccessary columns
    group_by(year, month, season, treatment) %>%               # get mean, SE and number of obs for each treatement
    summarise_each(funs(Mean = mean, SE = se, N = get_n)) %>% 
    .[, order(names(.))] %>%                                   # order columns by alphabet; "." is df inherited from the abvoe
    select(year, month, season, treatment, everything())       # reorder columns; bring Date and treatment first
})

# save as csv
l_ply(names(summary_by_rain_list), function(x) 
  write.csv(summary_by_rain_list[[x]],                                   # save each object within a list as csv
            file      = paste0("Output/Tables/summary_csv/summary_", x,  # file names are defined by object names in a list 
                               "_byRain.csv"),  
            row.names = FALSE))


# save as xlxs
writeWorksheetToFile(file  = "Output/Tables/summary_byRain.xlsx",         # define file name to be saved
                     data  = llply(summary_by_rain_list, as.data.frame),  # writeWorksheetToFile doesn't take dplyr object so turn them into data frames using as.data.frame
                     sheet = names(summary_by_rain_list))                 # sheet names in excel are defined by object names a list




# by herbivore treatemnts -------------------------------------------------


summary_by_herb_list <- llply(all_list, function(x){           
  x %>% 
    select(-plot, -treatment) %>% 
    group_by(year, month, season, herb) %>% 
    summarise_each(funs(Mean = mean, SE = se, N = get_n)) %>% 
    .[, order(names(.))] %>% 
    select(year, month, season, herb, everything())
})


# save as csv
l_ply(names(summary_by_herb_list), function(x) 
  write.csv(summary_by_herb_list[[x]], 
            file      = paste0("Output/Tables/summary_csv/summary_", x, "byHerb.csv"),
            row.names = FALSE))


# save as exel
writeWorksheetToFile(file  = "Output/Tables/summary_byHerb.xlsx", 
                     data  = llply(summary_by_herb_list, as.data.frame),
                     sheet = names(summary_by_herb_list))




# by rainfall x herbivore interaction -------------------------------------


summary_by_rxh_list <- llply(all_list, function(x){            
  x %>% 
    select(-plot) %>% 
    group_by(year, month, season, treatment, herb) %>% 
    summarise_each(funs(Mean = mean, SE = se, N = get_n)) %>% 
    .[, order(names(.))] %>% 
    select(year, month, season, treatment, herb, everything())
})


# save as csv
l_ply(names(summary_by_rxh_list), function(x) 
  write.csv(summary_by_rxh_list[[x]], 
            file      = paste0("Output/Tables/summary_csv/summary_", x, "_byRxH.csv"),
            row.names = FALSE))


# save as exel
writeWorksheetToFile(file  = "Output/Tables/summary_byRxH.xlsx", 
                     data  = llply(summary_by_rxh_list, as.data.frame),
                     sheet = names(summary_by_rxh_list))
