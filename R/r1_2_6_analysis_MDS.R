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


pc_df <- decostand(sp_summ_df, method = "hellinger")
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

fig_pcoa <- summary_site %>% 
  mutate(variable = factor(variable, 
                           levels = c("PCoA2", "PCoA3"), 
                           labels = c(paste0("PCoA2 (", exp_var[2], "%)"),
                                      paste0("PCoA3 (", exp_var[3], "%)")))) %>% 
  ggplot(., aes(x = PCoA1_M, y = value_M, col = treatment)) +
  facet_grid(variable ~ time, scales = "free_y") +
  labs(x = paste0("PCoA1 (", exp_var[1], "%)"), y = "Subsequent PCoA axis") +
  geom_point(size = 1, alpha = .8) +
  geom_errorbar(aes(ymin = value_M - value_SE, ymax = value_M + value_SE), width = .01, size = .2, alpha = .8) +
  geom_errorbarh(aes(xmin = PCoA1_M - PCoA1_SE, xmax = PCoA1_M + PCoA1_SE), height = .01, size = .2, alpha = .8) +
  science_theme+
  theme(legend.position = "top",
        axis.text.x = element_text(size = 5))
fig_pcoa

ggsavePP(filename = "Output/Figs/PCoA_contherb_summer", width = 6.5, height  = 4, 
         plot = fig_pcoa)