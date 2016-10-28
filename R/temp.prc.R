sp_summ_df <- ab_spp_biom %>% 
  filter(treatment != "No.shelter" & herb == "Control" & month == 4) %>% 
  mutate(treatment = factor(treatment), year = factor(year)) %>% 
  arrange(plot, year) %>% 
  select(one_of(spp_names[colSums(.[spp_names]) != 0]))

site_summ_df <- ab_spp_biom %>%
  filter(treatment != "No.shelter" & herb == "Control" & month == 4) %>% 
  select(-one_of(spp_names)) %>%
  mutate(time = paste(year, month.abb[as.numeric(month)], sep = "-")) %>% 
  arrange(plot, year)


pc_df_s <- decostand(sp_summ_df, method = "hellinger")
prc_smmr <- prc(pc_df_s, treatment = site_summ_df$treatment, time = site_summ_df$year) 
plot(prc_smmr)
contr <- how(within = Within(type = "series"),
             plots  = Plots(strata = site_summ_df$plot, type = "free"),
             nperm  = 999)
anova(prc_smmr, permutations = contr)
