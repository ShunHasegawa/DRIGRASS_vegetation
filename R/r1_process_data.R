
# process data ------------------------------------------------------------


# load data
biom_summ2016 <- readWorksheetFromFile("Data/DRIGrass biomass March 2016 Corrected_Final.xlsx",
                                      sheet = 1)


# edit df
biom_summ2016 <- biom_summ2016 %>% 
  mutate(Date = "mar_2016",                                      # relabel Date
         Bothriochloa.macra = B.macra + Bothriochloa) %>%        # these are the same species
  select(-B.macra, -Bothriochloa, - Unknown) %>%                 # remove unnecessary columns
  rename(plot                     = Plot,                        # rename columns
         tot_biomass              = Total.harvested.weight..g.,
         Anagallis.sp             = Anagallis,
         Axonopus.affinis         = Axonopus,
         Bidens.pilosa            = Bidens,
         Commelina.cyanea         = Comelina,
         Conyza.bonariensis       = Conyza,
         Cympobogon.refractus     = Cympobogon,
         Cynodon.dactlyon         = Cynodon,
         Cyperus.sesquiflorus     = Cyperus,
         Dichelachne.sp           = Dichelachne,
         Digitaria.sp             = Digitaria,
         Eragrostis.curvula       = Eragrostis,
         Hypochaeris.radicata     = Hypochaeris.,
         Juncus.sp                = Juncus,
         Lotus.corniculatus       = Lotus,
         Microlaena.stipoides     = Microlaena,
         Ornothopus.compressus    = Ornothopus,
         Oxalis.corniculata       = Oxalis,
         Paspalidum.sp            = Paspalidum,
         Paspalum.dilitatum       = Paspalum,
         Plantago.lanceolata      = Plantago,
         Senecio.madagascariensis = Senecio,
         Setaria.parviflora       = Setaria,
         Sida.rhombifolia         = Sida.,
         Sporobolus.sp            = Sporobolus,
         Verbena.bonariensis      = Verbena,
         Wahlenbergia.sp          = Whalenbergia,
         Medicago.sp              = Medicago,
         Vicia.sp                 = Vicia) %>% 
  add.factors()                                                  # add treatments


# species names
spp_2016 <- biom_summ2016 %>% 
  select(-Date, -plot, -Dead, -tot_biomass, -treatment, -herb, -side) %>% # remove non-spp columns
  names() %>% 
  sort()
write.csv(spp_2016, "Output/Data/spp_2016.csv", row.names = FALSE)


# add live biomass (i.e. row sum for all spp)
biom_summ2016$Live <- rowSums(biom_summ2016[, spp_2016])




# prepare df for further analysis -----------------------------------------


# > total/live/dead biomass -----------------------------------------------

tld_biom_2016 <- select(biom_summ2016, Date, plot, treatment, herb, tot_biomass, Dead, Live)


# > biomass for each spp --------------------------------------------------

sp_biom_2016  <- select(biom_summ2016, Date, plot, treatment, herb, one_of(spp_2016))


# . diversity indices -----------------------------------------------------

div_2016 <- sp_biom_2016 %>% 
  transmute(Date, plot, treatment, herb,
            H = diversity(.[spp_2016]),  # shannon's index
            S = specnumber(.[spp_2016]), # number of spp
            J = H/log(S))                # evenness


# . PFG ratios ------------------------------------------------------------

# df for PFG
sp_pfg <- read.csv("Data/spp_PFG_2016.csv")

pfg_2016 <- sp_biom_2016 %>% 
  gather(spp, value, -Date, -plot, -treatment, -herb) %>%  # reshape: gather sp coumns to rows
  left_join(sp_pfg) %>%                                    # merge with PFG df
  group_by(Date, plot, treatment, herb) %>%                # get total biomass and c3 ratios for each plot
  summarise(total   = sum(value),
            c3ratio = sum(value[pfg == "c3"]) / total,
            c4grass = sum(value[pfg == "c4"])) %>% 
  ungroup()                                                # remove grouping factors
