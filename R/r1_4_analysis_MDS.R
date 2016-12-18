summary(spp_names)    # species names in 2016
summary(ab_spp_biom)  # df for each spp



# prepare df --------------------------------------------------------------
ab_spp_biom$id <- 1:nrow(ab_spp_biom)

sp_summ_df <- ab_spp_biom %>% 
  filter(treatment != "No.shelter" & herb == "Control" & month == 4) %>% 
  arrange(plot, year) %>% 
  select(one_of(spp_names[colSums(.[spp_names]) != 0]))

sp_wint_df <- ab_spp_biom %>% 
  filter(treatment != "No.shelter" & herb == "Control" & month == 10) %>% 
  arrange(plot, year) %>% 
  select(one_of(spp_names[colSums(.[spp_names]) != 0]))

site_summ_df <- ab_spp_biom %>%
  filter(treatment != "No.shelter" & herb == "Control" & month == 4) %>% 
  select(-one_of(spp_names)) %>%
  mutate(time = paste(year, month.abb[as.numeric(month)], sep = "-")) %>% 
  arrange(plot, year)

site_wint_df <- ab_spp_biom %>%
  filter(treatment != "No.shelter" & herb == "Control"& month == 10) %>% 
  select(-one_of(spp_names)) %>%
  mutate(time = paste(year, month.abb[as.numeric(month)], sep = "-")) %>% 
  arrange(plot, year)




# PCoA --------------------------------------------------------------------



# . summer ------------------------------------------------------------------


pc_df <- decostand(sp_summ_df, method = "log")
PCoA_summ <- cmdscale(d = vegdist(pc_df, method = "bray"), eig = TRUE, k = 3)
summary(PCoA_summ)
colnames(PCoA_summ$points) <- paste0("PCoA", 1:ncol(PCoA_summ$points))
pcoa_site_df <- cbind(site_summ_df, PCoA_summ$points)

summary_site <- pcoa_site_df %>% 
  gather(key = variable, value = value, PCoA2, PCoA3) %>% 
  group_by(year, time, treatment, variable) %>%
  summarise_each(funs(M = mean, SE = se, n = get_n), PCoA1, value)

# variance explained by PCoA axes
exp_var <- round(PCoA_summ$eig[1:3] * PCoA_summ$GOF[2]/(sum(PCoA_summ$eig[1:3])) * 100, 1)

fig_pcoa_summer <- summary_site %>% 
  filter(variable == "PCoA2") %>% 
  ggplot(., aes(x = PCoA1_M, y = value_M, col = treatment)) +
  facet_grid( ~ year, scales = "free_y") +
  labs(x = paste0("MDS1 (", exp_var[1], "%)"), 
       y = paste0("MDS2 (", exp_var[2], "%)")) +
  
  geom_errorbar(aes(ymin = value_M - value_SE, ymax = value_M + value_SE), 
                width = .01, size = .5, alpha = .8) +
  geom_errorbarh(aes(xmin = PCoA1_M - PCoA1_SE, xmax = PCoA1_M + PCoA1_SE), 
                 height = .01, size = .5, alpha = .8) +
  geom_point(size = 3, alpha = .8) +
  
  scale_color_manual(values = rain_cols) +
  science_theme +
  theme(legend.position = "top")
fig_pcoa_summer

ggsavePP(filename = "Output/Figs/PCoA_contherb_summer", width = 6.5, height  = 3.5, 
         plot = fig_pcoa_summer)




# . winter ------------------------------------------------------------------


pc_df_wint <- decostand(sp_wint_df, method = "log")
PCoA_wint <- cmdscale(d = vegdist(pc_df_wint, method = "bray"), eig = TRUE, k = 3)
summary(PCoA_wint)
colnames(PCoA_wint$points) <- paste0("PCoA", 1:ncol(PCoA_wint$points))
pcoa_site_df_wint <- cbind(site_wint_df, PCoA_wint$points)

summary_site_wint <- pcoa_site_df_wint %>% 
  gather(key = variable, value = value, PCoA2, PCoA3) %>% 
  group_by(year, time, treatment, variable) %>%
  summarise_each(funs(M = mean, SE = se, n = get_n), PCoA1, value)

# variance explained by PCoA axes
exp_var <- round(PCoA_wint$eig[1:3] * PCoA_wint$GOF[2]/(sum(PCoA_wint$eig[1:3])) * 100, 1)

fig_PCoA_winter <- summary_site_wint %>% 
  filter(variable == "PCoA2") %>% 
  ggplot(., aes(x = PCoA1_M, y = value_M, col = treatment)) +
  facet_grid( ~ year, scales = "free_y") +
  labs(x = paste0("MDS1 (", exp_var[1], "%)"), 
       y = paste0("MDS2 (", exp_var[2], "%)")) +
  
  geom_errorbar(aes(ymin = value_M - value_SE, ymax = value_M + value_SE), 
                width = .01, size = .5, alpha = .8) +
  geom_errorbarh(aes(xmin = PCoA1_M - PCoA1_SE, xmax = PCoA1_M + PCoA1_SE), 
                 height = .01, size = .5, alpha = .8) +
  geom_point(size = 3, alpha = .8) +
  
  scale_color_manual(values = rain_cols) +
  science_theme +
  theme(legend.position = "top")
fig_PCoA_winter

ggsavePP(filename = "Output/Figs/PCoA_contherb_winter", width = 6.5 * 2 / 3, height  = 3.5, 
         plot = fig_PCoA_winter)

