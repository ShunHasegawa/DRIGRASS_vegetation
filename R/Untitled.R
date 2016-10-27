summary(ab_spp_biom)  # df for each spp
names(ab_spp_biom)
unique(ab_spp_biom$season)

  

treat_d <- read.csv("Data/root_trait_smmry_bySpp.csv") %>% 
  gather(variable, value, -spp) %>% 
  mutate(type = ifelse(grepl("M$", variable), "M", 
                       ifelse(grepl("SE$", variable), "SE", "N")),
         trait = gsub("_M$|_SE$|_N$", "", variable)) %>% 
  select(-variable) %>% 
  spread(type, value) %>% 
  filter(N > 2) %>% 
  select(-N, -SE) %>% 
  spread(spp, M) %>% 
  rename(Axonopus.fissifolius     = Axonopus, 
         Cynodon.dactlyon         = Cynodon,
         Eragrostis.curvula       = Eragrostis,
         Hypochaeris.radicata     = Hypochaeris,
         Lolium.perenne           = Lolium,      
         Microlaena.stipoides     = Microlaena, 
         Paspalum.dilitatum       = Paspalum, 
         Plantago.lanceolata      = Plantago,
         Senecio.madagascariensis = Senecio,
         Setaria.parviflora       = Setaria,
         Vicia.sativa             = Vicia) %>% 
  gather(variable, value, -trait) %>% 
  spread(trait, value)

row.names(treat_d) <- treat_d$variable
treat_d <- treat_d[, -1]
trait_spp <- row.names(treat_d)

sp_d_16apr <- ab_spp_biom %>% 
  filter(year == "2016" & season == "Summer" & treatment != "No.shelter") %>% 
  arrange(plot) %>% 
  select(-plot, -year, -month, -season, -herb) 

prsent_spp <- colnames(sp_d_16apr[, -1])[colSums(sp_d_16apr[, -1]) > 0]
common_spp <- intersect(prsent_spp, trait_spp)


sp_d <-  droplevels(sp_d_16apr[, common_spp])
sp_d <- round(sp_d*10, 0)
absum <- apply(sp_d > 0, 2, mean)
sp_d <- sp_d[, absum > 0.3]
tr_d <-  droplevels(treat_d[common_spp, ])
tr_d <- tr_d[absum > 0.3, ]
env_d <- droplevels(data.frame(sp_d_16apr[, "treatment"]))

nrow(sp_d)
nrow(env_d)

library(mvabund)
library(lattice)

ft <- traitglm(sp_d, env_d, tr_d)
plot(ft)
plot(ft, which = 2)
ft$fourth.corner
anova(ft)


a        = max(abs(ft$fourth.corner))
colort   = colorRampPalette(c("blue","white","red")) 
plot.4th = levelplot(t(as.matrix(ft$fourth.corner)), xlab="Environmental Variables",
                     ylab="Species traits", col.regions=colort(100), at=seq(-a, a, length=100),
                     scales = list( x= list(rot = 45)))
print(plot.4th)

names(sp_d_16apr)
sp_d_16apr %>% 
  select(treatment, one_of(common_spp)) %>% 
  gather(variable, value, -treatment) %>% 
ggplot(., aes(x = variable, y = log(value + 1))) +
  facet_wrap( ~ treatment) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))


ftspp1 <- traitglm(sp_d, env_d)
plot(ftspp1)
ftspp1$fourth.corner



a        = max( abs(ftspp1$fourth.corner))
colort   = colorRampPalette(c("blue","white","red")) 
plot.spp = levelplot(t(as.matrix(ftspp1$fourth.corner)), xlab="Environmental Variables",
                     ylab="Species traits", col.regions=colort(100), at=seq(-a, a, length=100),
                     scales = list( x= list(rot = 45)))
print(plot.spp)



data(antTraits)
nrow(antTraits$abund)
ncol(antTraits$abund)
nrow(antTraits$env)
ncol(antTraits$env)
nrow(antTraits$traits)
ncol(antTraits$traits)




antTraits$env
