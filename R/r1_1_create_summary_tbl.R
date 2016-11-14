
# Here, two types summary tables will be generated
# 1) standard table with mean, SE and number of obs
# 2) tables for a report with mean(SE) for each season


# put all required dfs into a list
all_list <- list('total_biomass'  = ab_tot_biom,  
                 'biomass_bySpp'  = ab_spp_biom,  
                 'diversity'      = div_2016, 
                 'biomass_byPfg'  = select(pfg_2016, 
                                           -one_of("c3plant", "c4grass", "c3ratio", 
                                                   "grass", "forb", "grforatio", 
                                                   "total")))




# by rainfall treatments ------------------------------------------------



# . standard table --------------------------------------------------------


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


# . table for report ------------------------------------------------------

# create a table
summary_by_rain_list_rep <- ldply(all_list, function(x){
  x %>% 
    filter(treatment != "No.shelter") %>% 
    select(-plot, -herb, -month) %>%                         # remove unnecessary columns
    gather(variable, value, -year, -season, -treatment) %>%  # reshape to long format
    group_by(year, season, treatment, variable) %>%          # group factors to get summary data
    summarise(value = get_mean_se(value, na.rm = TRUE)) %>%  # get Mean(SE)
    ungroup() %>%                                            # remove grouping factor
    spread(treatment, value)                                 # reshape to a wide format
}) %>% 
  mutate(unit = dplyr::recode(.id,                           # add unit
                       total_biomass = "g plot-1", 
                       biomass_bySpp = 'g plot-1',
                       diversity = '',
                       biomass_byPfg = ''),
         .id = factor(.id, levels = c("total_biomass", "biomass_bySpp", 
                                      "diversity", "biomass_byPfg"))) %>%
  select(season, .id, variable, unit, year, everything()) %>%  # reorder columns
  split(., .$season)                                           # split to a list by seasons


# save as csv
l_ply(names(summary_by_rain_list_rep), function(x){
  d <- summary_by_rain_list_rep[[x]] %>% 
    select(-season) %>% 
    arrange(.id, variable, year)
  f <- paste0("Output/Tables/report/summary_byRain_rprt_", x, ".csv")
  write.csv(d, row.names = FALSE, file = f)
})




# by herbivore treatemnts -------------------------------------------------


# . standard table --------------------------------------------------------


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




# . table for report ------------------------------------------------------


# create a table
summary_by_herb_list_rep <- ldply(all_list, function(x){
  x %>% 
    filter(treatment %in% c("Pulsed.drought", "Ambient", "Drought")) %>% 
    select(-plot, -treatment, -month) %>%
    gather(variable, value, -year, -season, -herb) %>%
    group_by(year, season, herb, variable) %>%
    summarise(value = get_mean_se(value, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(herb, value)
}) %>% 
  mutate(unit = dplyr::recode(.id,
                              total_biomass = "g plot-1", 
                              biomass_bySpp = 'g plot-1',
                              diversity = '',
                              biomass_byPfg = ''),
         .id = factor(.id, levels = c("total_biomass", "biomass_bySpp", 
                                      "diversity", "biomass_byPfg"))) %>% 
  select(season, .id, variable, unit, year, everything()) %>%  # reorder columns
  split(., .$season)                                           # split to a list by seasons


# save as csv
l_ply(names(summary_by_herb_list_rep), function(x){
  d <- summary_by_herb_list_rep[[x]] %>% 
    select(-season) %>% 
    arrange(.id, variable, year)
  f <- paste0("Output/Tables/report/summary_byHerb_rprt_", x, ".csv")
  write.csv(d, row.names = FALSE, file = f)
})




# by rainfall x herbivore interaction -------------------------------------


# . standard table --------------------------------------------------------


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




# . table for report ------------------------------------------------------


# create a table
summary_by_rh_list_rep <- ldply(all_list, function(x){
  x %>% 
    filter(treatment %in% c("Pulsed.drought", "Ambient", "Drought")) %>% 
    select(-plot, -month) %>%
    gather(variable, value, -year, -season, -treatment, -herb) %>%
    group_by(year, season, treatment, herb, variable) %>%
    summarise(value = get_mean_se(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(RH = factor(treatment:herb, 
                       levels = c("Ambient:Control", "Ambient:Added",
                                  "Drought:Control", "Drought:Added",
                                  "Pulsed.drought:Control", "Pulsed.drought:Added"))) %>% 
    select(-treatment, -herb) %>% 
    spread(RH, value)
}) %>% 
  mutate(unit = dplyr::recode(.id,
                              total_biomass = "g plot-1", 
                              biomass_bySpp = 'g plot-1',
                              diversity = '',
                              biomass_byPfg = ''),
         .id = factor(.id, levels = c("total_biomass", "biomass_bySpp", 
                                      "diversity", "biomass_byPfg"))) %>% 
  select(season, .id, variable, unit, year, everything()) %>%  # reorder columns
  split(., .$season)                                           # split to a list by seasons


# save as csv
l_ply(names(summary_by_rh_list_rep), function(x){
  d <- summary_by_rh_list_rep[[x]] %>% 
    select(-season) %>% 
    arrange(.id, variable, year)
  f <- paste0("Output/Tables/report/summary_byRxH_rprt_", x, ".csv")
  write.csv(d, row.names = FALSE, file = f)
})




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