# put all required dfs into a list

all_list <- list('biomass'        = tld_biom_2016, 
                 'biomass_by_spp' = sp_biom_2016, 
                 'diversity'      = div_2016, 
                 'biomass_by_pfg' = pfg_2016)




# by rainfall treatments ------------------------------------------------


summary_by_rain_list <- llply(all_list, function(x){
  x %>% 
    select(-plot, -herb) %>%                                   # remove unneccessary columns
    group_by(Date, treatment) %>%                              # get mean, SE and number of obs for each treatement
    summarise_each(funs(Mean = mean, SE = se, N = get_n)) %>% 
    .[, order(names(.))] %>%                                   # order columns by alphabet; "." is df inherited from the abvoe
    select(Date, treatment, everything())                      # reorder columns; bring Date and treatment first
})

# save as csv
l_ply(names(summary_by_rain_list), function(x) 
  write.csv(summary_by_rain_list[[x]],                                           # save each object within a list as csv
            file      = paste0("Output/Tables/summary_csv/", x, "by_rain.csv"),  # file names are defined by object names in a list
            row.names = FALSE))


# save as xlxs
writeWorksheetToFile(file  = "Output/Tables/summary_by_rain.xlsx",        # define file name to be saved
                     data  = llply(summary_by_rain_list, as.data.frame),  # writeWorksheetToFile doesn't take dplyr object so turn them into data frames using as.data.frame
                     sheet = names(summary_by_rain_list))                 # sheet names in excel are defined by object names a list




# by herbivore treatemnts -------------------------------------------------


summary_by_herb_list <- llply(all_list, function(x){           
  x %>% 
    select(-plot, -treatment) %>% 
    group_by(Date, herb) %>% 
    summarise_each(funs(Mean = mean, SE = se, N = get_n)) %>% 
    .[, order(names(.))] %>% 
    select(Date, herb, everything())
})


# save as csv
l_ply(names(summary_by_herb_list), function(x) 
  write.csv(summary_by_herb_list[[x]], 
            file      = paste0("Output/Tables/summary_csv/", x, "by_herb.csv"),
            row.names = FALSE))


# save as exel
writeWorksheetToFile(file  = "Output/Tables/summary_by_herb.xlsx", 
                     data  = llply(summary_by_herb_list, as.data.frame),
                     sheet = names(summary_by_herb_list))




# by rainfall x herbivore interaction -------------------------------------


summary_by_rxh_list <- llply(all_list, function(x){            
  x %>% 
    select(-plot) %>% 
    group_by(Date, treatment, herb) %>% 
    summarise_each(funs(Mean = mean, SE = se, N = get_n)) %>% 
    .[, order(names(.))] %>% 
    select(Date, treatment, herb, everything())
})


# save as csv
l_ply(names(summary_by_rxh_list), function(x) 
  write.csv(summary_by_rxh_list[[x]], 
            file      = paste0("Output/Tables/summary_csv/", x, "_by_rxh.csv"),
            row.names = FALSE))


# save as exel
writeWorksheetToFile(file  = "Output/Tables/summary_by_rxh.xlsx", 
                     data  = llply(summary_by_rxh_list, as.data.frame),
                     sheet = names(summary_by_rxh_list))
