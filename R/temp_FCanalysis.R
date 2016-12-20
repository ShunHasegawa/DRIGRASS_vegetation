
summary(trate_d)
row.names(trate_d) <- treat_spp
trate_dd <- trate_d[, c("sr_ratio", "total_L")]
# trate_dd2 <- cbind(trate_dd, y = sample(nrow(trate_dd)))
# vif(lm(y ~  Forks + sr_ratio + Tips + total_SA + total_L, data = trate_dd2))
# vif(lm(y ~   sr_ratio + Tips + total_SA, data = trate_dd2))
scatterplotMatrix(trate_dd)

# sp_d_16apr <- ab_spp_biom %>% 
#   filter(year == "2016" & season == "Summer" & treatment != "No.shelter") %>% 
#   arrange(plot) %>% 
#   select(-plot, -year, -month, -season, -herb) 

sp_d_16apr <- ab_spp_biom %>% 
  filter(year == "2013" & season == "Winter" & treatment != "No.shelter") %>% 
  arrange(plot) %>% 
  select(-plot, -year, -month, -season, -herb) 

ab_spp_biom_ed <- ab_spp_biom %>% 
  filter(treatment != "No.shelter")

rql_m_l <- dlply(ab_spp_biom_ed, .(season, year), function(x){
  rm(d, sp_d, tr_d, env_d, prsent_spp, common_spp, envir = .GlobalEnv)
  d <<- x %>% 
    arrange(plot) %>% 
    select(-plot, -year, -month, -season, -herb) 
  d[, -1] <- round(d[, -1], 0)
  prsent_spp <<- colnames(d[, -1])[colSums(d[, -1]) > 0]
  common_spp <<- intersect(prsent_spp, treat_spp)
  
  
  sp_d  <<-  droplevels(d[, common_spp])
  tr_d  <<-  droplevels(trate_dd[common_spp, ])
  env_d <<- droplevels(data.frame(treatment = d[, "treatment"]))
  env_d <- data.frame(model.matrix(~env_d$treatment)[, -1])
  
  # test full model
  ft1 <- traitglm(sp_d, env_d, tr_d)
  # rql_res <- anova(ft1, nBoot = 99)
  
  # simplification
  ft2 <- traitglm(sp_d, env_d, tr_d, method="glm1path", n.lambda = 100, composition = TRUE)
  return(list(full_model = ft1, last_model = ft2, sp_d = sp_d, env_d = env_d, tr_d = tr_d))
  rm(d, sp_d, tr_d, env_d, prsent_spp, common_spp, envir = .GlobalEnv)
})

rql_models <- llply(rql_m_l, function(x) x$last_model)

## model diagnosis
par(mfrow = c(2, 3))
l_ply(rql_models, plot)

par(mfrow = c(2, 3))
l_ply(rql_models, function(x){ 
  qqnorm(residuals(x))
  abline(c(0,1), col="red")
})


llply(rql_models, function(x) x$fourth.corner)


## Summer 2015 with RR:total_L
s2015_rql <- rql_m_l$Summer.2015
names(s2015_rql$env_d) <- c("IR", "RR", "RF", "SD")
s2015_f1 <- traitglm(s2015_rql$sp_d, s2015_rql$env_d, s2015_rql$tr_d, composition = TRUE, formula = ~1)
s2015_f2 <- traitglm(s2015_rql$sp_d, s2015_rql$env_d, s2015_rql$tr_d, composition = TRUE, formula = ~ RR:total_L)
anova(s2015_f1, s2015_f2)
s2015_aov <- anova(s2015_f2)


## Winter 2014
w2014_rql <- rql_m_l$Winter.2014
names(w2014_rql$env_d) <- c("IR", "RR", "RF", "SD")
w2014_f1 <- traitglm(w2014_rql$sp_d, w2014_rql$env_d, w2014_rql$tr_d, composition = TRUE, formula = ~1)
w2014_f2 <- traitglm(w2014_rql$sp_d, w2014_rql$env_d, w2014_rql$tr_d, composition = TRUE, formula = ~ IR:total_L + RR:sr_ratio + SD:sr_ratio + SD:total_L)
anova(w2014_f1, w2014_f2)
w2014_aov <- anova(w2014_f2)

# result table

rql_res <- ldply(list('Summer 2015' = s2015_aov, 'Winter 2014' = w2014_aov), function(x){ 
  tbl <- x$table[2, ]
  cbind(term = row.names(tbl), tbl)
  }) %>% 
  mutate(Season = tstrsplit(.id, split = " ")[[1]],
         Year = tstrsplit(.id, split = " ")[[2]]) %>% 
  select(Season, Year, everything(), -.id, -term) %>% 
  bind_rows(data.frame(Season = c("Summer", "Summer", "Winter"), 
                       Year   = c("2014", "2016", "2013"))) %>% 
  arrange(Season, Year)

    


# figure ------------------------------------------------------------------

rql_plt_l <- llply(names(rql_models)[c(2, 5)], function(x){
  m <- rql_models[[x]]
  a        = max(abs(m$fourth.corner))
  colort   = colorRampPalette(c("blue","white","red")) 
  
  mx <- as.matrix(m$fourth.corner)
  colnames(mx) <- c("IR", "RR", "RF", "SD")
  tmx <- t(mx)
  colnames(tmx) <- c("Shoot:root ratio", "Root length")
  
  plot.4th = levelplot(tmx, 
                       xlab="Environmental Variables",
                       ylab="Species traits", 
                       col.regions=colort(100), 
                       at=seq(-a, a, length=100),
                       main = gsub("[.]", " ", x))
  return(plot.4th)
}) 

pdf(file = "Output/Figs/fourth_coner_analysis.pdf", width = 6, height = 5)
grid.arrange(rql_plt_l[[1]],rql_plt_l[[2]], ncol=1)
  dev.off()

save_png600(filename = "Output/Figs/fourth_coner_analysis.png", width = 6, height = 5)
grid.arrange(rql_plt_l[[1]],rql_plt_l[[2]], ncol=1)
dev.off()


# visualise ---------------------------------------------------------------


# . Summer 2015 -----------------------------------------------------------

s2015_rql$tr_d %>% 
  mutate(variable = row.names(.)) %>% 
  arrange(total_L)

treatval <- ab_spp_biom %>% 
  filter(season == "Summer" & year == "2015" & treatment!= "No.shelter") %>% 
  droplevels(.) %>% 
  arrange(plot) %>% 
  .$treatment
sp_d2 <- data.frame(t(apply(s2015_rql$sp_d, 1, function(x) x/sum(x))))  # proportion for each plot
par(mfrow = c(2, 3))

# thick root
boxplot(log(sp_d2$Microlaena.stipoides + 1) ~ treatval, main = "Microlaena.stipoides")
boxplot(log(sp_d2$Paspalum.dilitatum   + 1) ~ treatval, main = "Paspalum.dilitatum")
boxplot(log(sp_d2$Hypochaeris.radicata + 1) ~ treatval, main = "Hypochaeris.radicata")

# thin root
boxplot(log(sp_d2$Cynodon.dactlyon     + 1) ~ treatval, main = "Cynodon.dactlyon")
boxplot(log(sp_d2$Setaria.parviflora   + 1) ~ treatval, main = "Setaria.parviflora")
boxplot(log(sp_d2$Plantago.lanceolata  + 1) ~ treatval, main = "Plantago.lanceolata")
boxplot(log(sp_d2$Eragrostis.curvula  + 1) ~ treatval, main = "Eragrostis.curvula")



### Winter 2014
w2014_rql$tr_d %>% 
  mutate(srl = total_L/sr_ratio,
         variable = row.names(.)) %>% 
  arrange(srl)

treatval <- ab_spp_biom %>% 
  filter(season == "Winter" & year == "2014" & treatment!= "No.shelter") %>% 
  droplevels(.) %>% 
  arrange(plot) %>% 
  .$treatment
sp_d2 <- data.frame(t(apply(w2014_rql$sp_d, 1, function(x) x/sum(x))))  # proportion for each plot


par(mfrow = c(2, 3))
# thick root (short root) and high s:r ratio
boxplot(log(sp_d2$Eragrostis.curvula + 1) ~ treatval, main = "Eragrostis.curvula")
boxplot(log(sp_d2$Paspalum.dilitatum + 1) ~ treatval, main = "Paspalum.dilitatum")
boxplot(log(sp_d2$Microlaena.stipoides + 1) ~ treatval, main = "Microlaena.stipoides")

# thin root (long root) and low s:r ratio
boxplot(log(sp_d2$Lolium.perenne + 1)     ~ treatval, main = "Lolium.perenne")
boxplot(log(sp_d2$Setaria.parviflora + 1) ~ treatval, main = "Setaria.parviflora")
boxplot(log(sp_d2$Plantago.lanceolata + 1) ~ treatval, main = "Plantago.lanceolata")

