library(mvabund)
library(lattice)

summary(trate_d)
row.names(trate_d) <- treat_spp
trate_dd <- trate_d[, c("sr_ratio", "Tips", "total_SA")]
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

rql_m_l <- dlply(ab_spp_biom_ed, .(year, season), function(x){
  rm(d, sp_d, tr_d, env_d, prsent_spp, common_spp)
  d <<- x %>% 
    arrange(plot) %>% 
    select(-plot, -year, -month, -season, -herb) 
  prsent_spp <<- colnames(d[, -1])[colSums(d[, -1]) > 0]
  common_spp <<- intersect(prsent_spp, treat_spp)
  
  
  sp_d  <<-  droplevels(d[, common_spp])
  sp_d  <<- round(sp_d*100, 0)
  tr_d  <<-  droplevels(trate_dd[common_spp, ])
  env_d <<- droplevels(data.frame(d[, "treatment"]))
  ft <- traitglm(sp_d, env_d, tr_d)
  rql_res <- anova(ft, nBoot = 999)
  return(list(model = ft, anv = rql_res))
})

rql_models <- llply(rql_m_l, function(x) x$model)
rql_res <- ldply(rql_m_l, function(x){
  tbl <- x$anv$table[2, ]
  cbind(term = row.names(tbl), tbl)
})

plot(rql_models[[1]])
plot(rql_models[[2]])
plot(rql_models[[3]])
plot(rql_models[[4]])
plot(rql_models[[5]])


plot(rql_models[[1]], which = 2)
plot(rql_models[[2]], which = 2)
plot(rql_models[[3]], which = 2)
plot(rql_models[[4]], which = 2)
plot(rql_models[[5]], which = 2)


rql_plt_l <- llply(names(rql_models), function(x){
  m <- rql_models[[x]]
  a        = max(abs(m$fourth.corner))
  colort   = colorRampPalette(c("blue","white","red")) 
  
  mx <- as.matrix(m$fourth.corner)
  colnames(mx) <- gsub("^.*[.][.]", "",colnames(mx))
  
  plot.4th = levelplot(t(mx), xlab="Environmental Variables",
                       ylab="Species traits", 
                       col.regions=colort(100), 
                       at=seq(-a, a, length=100),
                       scales = list(x= list(rot = 45)),
                       main = x)
  return(plot.4th)
}) 

print(rql_plt_l[[1]])
print(rql_plt_l[[2]])
print(rql_plt_l[[3]])#
print(rql_plt_l[[4]])
print(rql_plt_l[[5]])#

