

# prepare dfs -------------------------------------------------------------

## sp df

### summer
sp_summ_df <- ab_spp_biom %>% 
  filter(treatment != "No.shelter" & herb == "Control" & month == 4) %>% 
  mutate(treatment = factor(treatment), year = factor(year)) %>% 
  arrange(plot, year) %>% 
  select(one_of(spp_names[colSums(.[spp_names]) != 0])) %>% 
  droplevels(.)

### winter
sp_wint_df <- ab_spp_biom %>% 
  filter(treatment != "No.shelter" & herb == "Control" & month == 10) %>% 
  mutate(treatment = factor(treatment), year = factor(year)) %>% 
  arrange(plot, year) %>% 
  select(one_of(spp_names[colSums(.[spp_names]) != 0])) %>% 
  droplevels(.)


## site df

### summer
site_summ_df <- ab_spp_biom %>%
  filter(treatment != "No.shelter" & herb == "Control" & month == 4) %>% 
  select(-one_of(spp_names)) %>%
  mutate(time = paste(year, month.abb[as.numeric(month)], sep = "-"),
         year = factor(year)) %>% 
  arrange(plot, year) %>% 
  droplevels(.)


### winter
site_wint_df <- ab_spp_biom %>%
  filter(treatment != "No.shelter" & herb == "Control" & month == 10) %>% 
  select(-one_of(spp_names)) %>%
  mutate(time = paste(year, month.abb[as.numeric(month)], sep = "-"),
         year = factor(year)) %>% 
  arrange(plot, year) %>% 
  droplevels(.)





# analysis ----------------------------------------------------------------


## tranform data
pc_df_s <- decostand(sp_summ_df, method = "log")
pc_df_w <- decostand(sp_wint_df, method = "log")
plot(prc(pc_df_s, site_summ_df$treatment , site_summ_df$year))
plot(prc(pc_df_w, site_wint_df$treatment , site_wint_df$year))


## PRC
prc_smmr <- capscale(pc_df_s ~ treatment * year + Condition(year), site_summ_df,
                    distance = "euclidean")
prc_wint <- capscale(pc_df_w ~ treatment * year + Condition(year), site_wint_df,
                     distance = "euclidean")


## species score of prc
prc_sp_smmr <- summary(prc_smmr, scaling = 3)$species
prc_sp_wint <- summary(prc_wint, scaling = 3)$species


### subset abundanct species
sppabund_smmr <- colSums(sp_summ_df)  # total abundance for each sp in summer
sppabund_wint <- colSums(sp_wint_df)  # total abundance for each sp in winter

prc_sp_smmr <- prc_sp_smmr[sppabund_smmr > 100 , ]
prc_sp_wint <- prc_sp_wint[sppabund_wint > 100 , ]


## effect size on the site scores of the 1st axis of PRC
prc_effect_smmr <- get_prc_effect_df(prc_smmr, site_summ_df)
prc_effect_wint <- get_prc_effect_df(prc_wint, site_wint_df)




# Permanova ---------------------------------------------------------------


## get all permutation; this requires computational power, so use multicores and
## carry out parallel processing to save time
detectCores()                  # number of cores in the current machine
registerDoParallel(cores = 3)  # register parallel background


prc_res_smmr <- anova(prc_smmr, by = "axis", parallel = 3,
                      permutations = how(within = Within(type = "series"),
                                         plots  = Plots(strata = site_summ_df$plot, 
                                                        type = "free"),
                                         nperm  = 9999))

prc_res_wint <- anova(prc_wint, by = "axis", parallel = 3,
                      permutations = how(within = Within(type = "series"),
                                         plots  = Plots(strata = site_wint_df$plot, 
                                                        type = "free"),
                                         nperm  = 9999))
prc_res_smmr
prc_res_wint




## Test treatement effect for each year (i.e. permanova for each year) 
prc_by_year_smmr <- llply(2014:2016, function(x){
  spd          <- pc_df_s[site_summ_df$year == x, ]
  sited        <- site_summ_df[site_summ_df$year == x, ]
  prc_year     <- adonis(spd ~ treatment, sited, method = "bray", permutations = 9999)
  return(prc_year)
}, .parallel = TRUE, .paropts = list(.export = c("pc_df_s", "site_summ_df")))
names(prc_by_year_smmr) <- paste(2014:2016, "Summer", sep = "_")


prc_by_year_wint <- llply(2013:2014, function(x){
  spd          <- pc_df_w[site_wint_df$year == x, ]
  sited        <- site_wint_df[site_wint_df$year == x, ]
  prc_year     <- adonis(spd ~ treatment, sited, method = "bray", permutations = 9999)
  return(prc_year)
}, .parallel = TRUE, .paropts = list(.export = c("pc_df_w", "site_wint_df")))
names(prc_by_year_wint) <- paste(2013:2014, "Winter", sep = "_")




## merge the above results
prmnv_res_l <- unlist(list(prc_by_year_smmr, prc_by_year_wint), recursive = FALSE)
prmnv_res_tbl <- ldply(prmnv_res_l, function(x){
  atb  <- x$aov.tab
  Fval <- paste0("F(", atb$Df[1], ",", atb$Df[2], ")", "=", round(atb$F.Model[1], 2))
  Pval <- atb$`Pr(>F)`[1]
  ptbl <- data.frame(pseudo_F = Fval, R2 = atb$R2[1], P = Pval)
  return(ptbl)
})
prmnv_res_tbl


# Figure ------------------------------------------------------------------


range(prc_effect_smmr$eff)
range(prc_sp_smmr[,1])
range(prc_effect_wint$eff)
range(prc_sp_wint[,1])

get_prc_fig_smmr <- function(){
  get_prc_fig(prc_effect_smmr, prc_sp_smmr, ylim = c(-3, 3))
  text(x = 3, y = max(prc_effect_smmr$eff) * 1.1, labels = "*", cex = 2)  # add star for the year where significant treatment effects were found
}
get_prc_fig_smmr()


get_prc_fig(prc_effect_wint, prc_sp_wint, ylim = c(-1.6, 2.7))

par(mfrow = c(2, 1))
get_prc_fig_smmr()
get_prc_fig(prc_effect_wint, prc_sp_wint, ylim = c(-1.3, 1.3))

