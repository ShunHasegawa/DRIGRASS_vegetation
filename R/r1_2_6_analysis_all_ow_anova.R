
# prepare df --------------------------------------------------------------


# store all relevant dfs in a list
all_dfs <- list('total_biomass'  = ab_tot_biom,  
                 'diversity'     = div_2016, 
                 'biomass_byPfg' = select(pfg_2016, -total, -c4grass))  # unnecessary columns are removed 


# merge them
all_merged <- Reduce(function(...){
  merge(..., by = c("plot", "year", "month","season", "treatment", "herb"))
        },
  all_dfs) %>% 
  filter(treatment != "No.shelter")                                          # remove no-sheltered treatment


# transform data for anova
all_merged_trfm <- all_merged %>% 
  mutate(Dead    = log(Dead + 1),
         live    = log(live),
         total   = log(total),
         H       = H,
         S       = S,
         J       = J,
         c3ratio = logit(c3ratio)) %>% 
  gather(variable, trfm_value, Dead, live, total, H, S, J, c3ratio)  # reshape df to a long format


# merge original and transformed data
all_df <- all_merged %>% 
  gather(variable, original_value, Dead, live, total, H, S, J, c3ratio) %>% 
  left_join(all_merged_trfm, 
            by = c("plot", "year", "month","season", "treatment", "herb", "variable"))


  

# analysis ----------------------------------------------------------------

# Here, one-way anova is performed for all measurements for each season in each 
# year separately. 

# example analysis is shown in "R/r1_2_6_1_example_oneway_anova.R"

# 1) Test Rain x Herbivore interaction
# 2) If 1) shows significant Herb effect, create a subset of dataset with 'control'-herb
# 3) Test Rain using the complete dataset (control and added herb) or subset (control herb)
# 4) If 3) shows significant treatment effect, perform a post-hoc test
# 5) summarise the above results
        # 1. model used to test Rainfall
        # 2. summary table showing results of anova (P value for treatment), mean 
        # and SE for each treatment and associated significant characters that 
        # represent significant difference between treatment.


all_ow_anova <- dlply(all_df, .(variable, season, year), function(x){

  d <- x[complete.cases(x), ]                                                    # remove rows with NA
  
  
  # test Rain x Herb
  d_hxr     <- filter(d, treatment %in%  c("Pulsed.drought", "Ambient", 
                                           "Drought"))                           # subset required treatment
  m_hxr     <- lm(trfm_value ~ treatment * herb, data = d_hxr)                   # anova 
  anova_hxr <- Anova(m_hxr)                                                      # get anova results
  sig_terms <- row.names(anova_hxr)[anova_hxr$`Pr(>F)` <= 0.05 ]                 # identify significant terms
  
  
  # prepare df for analysis on rainfall
  cat(paste("\n", unique(x$variable), unique(x$season), unique(x$year)))         # print if there is a significant herb/rainxherb effect
  if(grepl("herb", sig_terms)){
    cat("\n  Note: significant herb/herb:treatment effect!!\n  added herb plots were remved for the analysis on rainfall treatments...")
    d_r <- filter(d, herb == "Control")                                          # if there is a significant herb effect, only control-herb is used for the analysis on rainfall
  } else {
    cat("\n  no herb effect...")
    d_r <- d                                                                     # if there no herb effect, only control-herb is used for the analysis on rainfall
  }
  
  
  # test Rain
  model_rain <- lm(trfm_value ~ treatment, data = d_r)                           # anvoa
  anova_r    <- anova(model_rain)                                                # get anova results
  treat_p    <- anova_r$`Pr(>F)`[1]                                              # get P value for treatment
  
  
  # post-hoc test when treatment was significant
  if(treat_p <= 0.05){
    symbols <- cld(glht(model_rain, linfct = mcp(treatment = "Tukey")),           # get symbols representing significant difference between treatmens 
                   decreasing = TRUE)$mcletters$Letters
    symbol_d <- data.frame(treatment = names(symbols), symbols, row.names = NULL) # store the result in df
  } else {
    symbol_d <- data.frame(treatment = unique(d_r$treatment), symbols = "")       # if there's no significant treatment effect, a post-hoc test is not performed
  }
  
  
  # create summary table (Mean(SE) and significant symbols)
  summary_tbl <- d_r %>% 
    group_by(year, season, treatment) %>%                                        # summarise for each group
    summarise_each(funs(M = mean, SE = se), original_value) %>%                  # mean and SE of original value
    ungroup() %>% 
    left_join(symbol_d, by = "treatment") %>%                                    # merge with a post-hoc table 
    transmute(year, season, treatment,                                           # concatenate mean, se and symbols
              value = paste0(round(M, 2), "(", round(SE, 2), ")", symbols)) %>% 
    spread(treatment, value) %>%                                                 # turn df to a wide format
    mutate(P = round(treat_p, 3)) %>%                                            # add P-value for treatment
    select(year, season, P, everything())                                        # reorder columns
  
  
  l <- list('model' = model_rain, 'summary_tbl' = summary_tbl)                   # store the model and summary table in a list for output           
  return(l)
})


# save model diagnosig plot in pdf
pdf(file = "Output/Figs/model_diagnosis_all_ow_anova.pdf", onefile = TRUE,
    width = 6.5, height = 6.5)
l_ply(names(all_ow_anova), function(x){
  par(mfrow = c(2, 2))
  plot(all_ow_anova[[x]]$mode)
  mtext(x, outer = TRUE, line = -2, cex = 1.5)
})
dev.off()


# save sumamry table
summary_tbl <- ldply(all_ow_anova, function(x) x$summary_tbl) %>% 
  arrange(variable, season, year)
write.csv(summary_tbl, "Output/Tables/Summary_all_anova_results.csv",
          row.names = FALSE)

