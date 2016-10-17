
# put all required dfs into a list
all_list <- list('total_biomass'  = ab_tot_biom,  
                 'biomass_bySpp'  = ab_spp_biom,  
                 'diversity'      = div_2016, 
                 'biomass_byPfg'  = pfg_2016)




# by rainfall treatments ------------------------------------------------


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




# save as exel ------------------------------------------------------------


# store all summar tables in a list
summar_tbl_list <- llply(names(all_list), function(x){
  list('raw'     = all_list[[x]],
       'by_rain' = summary_by_rain_list[[x]],
       'by_herb' = summary_by_herb_list[[x]],
       'by_RxH'  = summary_by_rxh_list[[x]])
})
names(summar_tbl_list) <- names(all_list)


# save as excel
get_summary_excel <- function(){
  l_ply(names(summar_tbl_list), function(x){
    filename  <- paste0("Output/Tables/summary_", x, ".xlsx")
    data_list <- summar_tbl_list[[x]]
    writeWorksheetToFile(file        = filename, 
                         data        = llply(data_list, as.data.frame),
                         sheet       = names(data_list), 
                         clearSheets = TRUE)
  })
}
# If one wants to save the above tables as excel run get_summary_excel(). This
# takes some time.