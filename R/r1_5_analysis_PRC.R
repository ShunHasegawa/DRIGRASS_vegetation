

# prepare dfs -------------------------------------------------------------

## sp df
sp_summ_df <- ab_spp_biom %>% 
  filter(treatment != "No.shelter" & herb == "Control" & month == 4) %>% 
  mutate(treatment = factor(treatment), year = factor(year)) %>% 
  arrange(plot, year) %>% 
  select(one_of(spp_names[colSums(.[spp_names]) != 0])) %>% 
  droplevels(.)


## site df
site_summ_df <- ab_spp_biom %>%
  filter(treatment != "No.shelter" & herb == "Control" & month == 4) %>% 
  select(-one_of(spp_names)) %>%
  mutate(time = paste(year, month.abb[as.numeric(month)], sep = "-"),
         year = factor(year)) %>% 
  arrange(plot, year) %>% 
  droplevels(.)





# analysis ----------------------------------------------------------------


## tranform data
pc_df_s <- decostand(sp_summ_df, method = "log")
plot(prc(pc_df_s, site_summ_df$treatment , site_summ_df$year, site_summ_df))  # eucldien


prc_smmr <- capscale(pc_df_s ~ treatment * year + Condition(year), site_summ_df,  # bray-curtis distance
                    distance = "euclidean")
prc_smmr_res <- summary(prc_smmr, scaling = 3)
prc_smmr_site <- data.frame(prc_smmr_res$sites, site_summ_df)
effect_d <- prc_smmr_site %>% 
  group_by(year, treatment) %>% 
  summarise(value = mean(CAP1)) %>% 
  group_by(year) %>% 
  mutate(eff = value - value[treatment == "Ambient"], 
         colval = as.character(mapvalues(treatment, treatment, rain_cols)))
prc_spp <- prc_smmr_res$species
range(prc_spp[, 1])

get_prc_fig <- function(){
  par(mai = c(1, .8, .2, 1.5))
  plot(eff ~ as.numeric(year), data = effect_d, ylim = c(-3, 3), 
       axes = F, xlab = "Year", ylab = "Effect", type = "n")
  axis(side = 1, at = c(1, 2, 3), labels = 2014:2016)
  axis(side = 2, las = 2)
  box()
  d_ply(effect_d, .(treatment), function(x){
    lines(eff ~ as.numeric(year), data = x, col = colval)
    points(eff ~ as.numeric(year), data = x, col = colval, pch = 19)
  })
  legend("bottomleft", col = rain_cols, legend = levels(effect_d$treatment), pch = 19, bty = "n")
  sppabund <- colSums(sp_summ_df)
  axis(side = 4, tck = .03, at = prc_spp[sppabund > 100 , 1], 
       labels = row.names(prc_spp)[sppabund > 100], las = 2, cex.axis = .7)
  text(x = 3, y = max(effect_d$eff) * 1.2, labels = "*", cex = 2)
}
get_prc_fig()




## get all permutation; this requires computational power, so use multicores and
## carry out parallel processing to save time
detectCores()                  # number of cores in the current machine
registerDoParallel(cores = 3)  # register parallel background



contr <- how(within = Within(type = "series"),
             plots  = Plots(strata = site_summ_df$plot, type = "free"),
             nperm  = 4999)
prc_res <- anova(prc_smmr, permutations = contr, by = "axis", parallel = 3)
prc_res


## Test treatement effect for each year (i.e. permanova for each year) 
prc_by_year <- llply(2014:2016, function(x){
  spd          <- pc_df_s[site_summ_df$year == x, ]
  sited        <- site_summ_df[site_summ_df$year == x, ]
  prc_year     <- capscale(spd ~ treatment, sited, distance = "euclidean")
  prc_year_res <- anova(prc_year, permutations = 9999)
  return(prc_year_res)
}, .parallel = TRUE, .paropts = list(.export = c("pc_df_s", "site_summ_df")))
prc_by_year
