
# prepare df --------------------------------------------------------------


# store all relevant dfs in a list
all_dfs <- list('total_biomass'  = ab_tot_biom,  
                 'diversity'     = div_2016, 
                 'biomass_byPfg' = select(pfg_2016, plot, year, month, season,  # required columns are selected
                                          treatment, herb, c34ratios, grprop),
                'traits'         = wghtdavrg_trait)  


# merge them
all_merged <- Reduce(function(...){
  merge(..., by = c("plot", "year", "month","season", "treatment", "herb"), 
        all.x = TRUE)
        },
  all_dfs) %>% 
  filter(treatment != "No.shelter")                                            # remove no-sheltered treatment
  

# transform data for anova

## define transfomation for each variable; these values will be passed to
## make.tran function
trans_df <- data.frame(rbind(
  c("Dead","genlog", 1),         # log(x + 1)
  c("live","genlog", 0),         # log(x)
  c("total","genlog", 0),
  c("H","identity", 1),          # no transformation (1 won't be used. just random value is placed no to leave it empty)
  c("S","identity", 1),
  c("J","identity", 1),
  c("c34ratios","genlog", .001), # log(x + .001)
  c("grprop","asin.sqrt", 1),    # asin sqrt
  c("Forks","genlog", 0),
  c("sr_ratio","genlog", 0),
  c("Tips","genlog", 0),
  c("total_L","genlog", 0),
  c("total_SA","genlog", 0))) %>% 
  rename(variable = X1,
         transtype = X2,
         transparam = X3) %>% 
  mutate(transparam = as.numeric(as.character(transparam)))


## combine with df for data
all_merged_trfm <- all_merged %>% 
  gather(variable, value, Dead, live, total, H, S, J, c34ratios, grprop,   # reshape df to a long format
         Forks, sr_ratio, Tips, total_L, total_SA) %>% 
  filter(!is.na(value)) %>% 
  left_join(trans_df, by = "variable")





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
        # 3. summary table of P values for each of treatment, herb and their
        # interactions
        # 4. summary table of post-hoc test results; in addition to symbols, it 
        # provides estimated 95% CI. Also when response variables are 
        # transformed for analysys, means (referred to as response) and CI are
        # reverse-transformed. The reversed-mean could be slightly different
        # than original means

all_ow_anova <- dlply(all_merged_trfm, .(variable, season, year), function(x){
  
  cat(paste("\n", unique(x$variable), unique(x$season), unique(x$year)))         
  
  d <- x[complete.cases(x), ]                                                    # remove rows with NA
  
  trstype <- as.character(unique(d$transtype))  # transformation to be used
  trspar  <- unique(d$transparam)               # transformation parameter
  
  
  # test Rain x Herb
  d_hxr     <- filter(d, treatment %in%  c("Reduced frequency", "Ambient", 
                                           "Reduced"))                           # subset required treatment
  if(trstype == "identity"){
    m_hxr <- lm(value ~ treatment * herb, data = d_hxr)
  }else{
    m_hxr <- with(make.tran(type = trstype, trspar), 
                  lm(linkfun(value) ~ treatment * herb, data = d_hxr))
  }
  
  
    
  # anova 
  anova_hxr <- tidy(Anova(m_hxr)) %>% # get anova results
    filter(grepl("herb", term))    # extract terms for herb and treatment:herb
  
  
  # prepare df for analysis on rainfall and print if there is a significant
  # herb/rainxherb effect
  if(any(anova_hxr$p.value <= 0.05)){
    cat("\n  Note: significant herb/herb:treatment effect!!\n  added herb plots were remved for the analysis on rainfall treatments...")
    d_r <- filter(d, herb == "Control")                                          # if there is a significant herb effect, only control-herb is used for the analysis on rainfall
  } else {
    cat("\n  no herb effect...")
    d_r <- d                                                                     # if there no herb effect, only control-herb is used for the analysis on rainfall
  }
  
  
  # test Rain
  
  if(trstype == "identity"){
    model_rain <- lm(value ~ treatment, data = d_r)
  }else{
    model_rain <- with(make.tran(type = trstype, trspar), 
                  lm(linkfun(value) ~ treatment, data = d_r))
  }
  anova_r    <- tidy(Anova(model_rain))                                          # get anova results
  treat_p    <- anova_r$p.value[1]                                               # get P value for treatment
  treat_f    <- paste0("F(", anova_r$df[1], ",", anova_r$df[2], ")=",            # get F value and associated DFs
                       round(anova_r$statistic[1], 2)) 
  
  # post-hoc test when treatment was significant
  lsmtest <- lsmeans::lsmeans(model_rain, specs = "treatment")                    # post-hoc test with lsmeans
  symbols <- gsub(" ", "", cld(lsmtest, Letters = letters, sort = FALSE)$.group)  # result of the post-hoc test (symbols)
  ci_df   <- data.frame(summary(lsmtest, type = "response")) %>%                  # summary df for estimated means (response) and 95% CI. note reponse values could be slightly different than means of actual data when they're transformed for anlaysis and reverse-transformed
    mutate(symbols = if(treat_p <= 0.05) symbols else "")                         # if there's no significant treatment effect, a post-hoc test is not shown
  names(ci_df)[2] <- "response"

  
  
  # create summary table (Mean(SE) and significant symbols)
  summary_tbl <- d_r %>% 
    group_by(year, season, treatment) %>%                                        # summarise for each group
    summarise_each(funs(M = mean, SE = se), value) %>%                           # mean and SE of original value
    ungroup() %>% 
    left_join(select(ci_df, treatment, symbols), by = "treatment") %>%            # merge with a post-hoc table 
    transmute(year, season, treatment,                                           # concatenate mean, se and symbols
              value = paste0(round(M, 2), "(", round(SE, 2), ")", symbols)) %>% 
    spread(treatment, value) %>%                                                 # turn df to a wide format
    mutate(F = treat_f,                                                          # add F value for treatment
           P = round(treat_p, 3)) %>%                                            # add P-value for treatment
    select(year, season, F, P, everything())                                     # reorder columns
  
  
  # summaty stats
  summary_stats <- bind_rows(anova_r[1, ], anova_hxr) %>% 
    select(term, p.value) %>% 
    spread(term, p.value)
  
  
  l <- list('model' = model_rain, 'summary_tbl' = summary_tbl,                    # store the model and summary table in a list for output
            'summary_stats' = summary_stats, 'summary_posthoc' = ci_df)
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
  mutate(variable = factor(variable, 
                           levels = c("live", "Dead", "total", "c34ratios", 
                                      "grprop", "H", "J", "S", "Forks", "sr_ratio", 
                                      "Tips", "total_L", "total_SA"))) %>% 
  arrange(variable, season, year)
write.csv(summary_tbl, "Output/Tables/Summary_all_anova_results.csv",
          row.names = FALSE)


# result of posthoc test 
anv_psthc_rslt <- ldply(all_ow_anova, function(x) x$summary_posthoc)


# save summary stats table
summary_stt_tbl <- ldply(all_ow_anova, function(x) x$summary_stats) %>%
  gather(term, pval, herb, treatment, `treatment:herb`) %>% 
  mutate(pval = ifelse(pval <= .05, as.character(get_star(pval, dagger = FALSE)), "ns"),                                               # change pvalues to start marks
         term = recode_factor(term, treatment = "Rain", herb = "Herbivore",   # relabel and reorder factors
                              `treatment:herb` = "RxH"),
         SYT  = paste(season, year, term, sep = "_")) %>%
  select(-season, -year, -term) %>% 
  spread(SYT, pval) %>% 
  mutate(variable = factor(variable, levels = c("live", "Dead", "total", "c34ratios", 
                                                "grprop", "H", "J", "S", "Forks", "sr_ratio", 
                                                "Tips", "total_L", "total_SA"))) %>% 
  arrange(variable)

write.csv(summary_stt_tbl, "Output/Tables/report/Summary_all_anova_results_stt.csv",
            row.names = FALSE)
  
