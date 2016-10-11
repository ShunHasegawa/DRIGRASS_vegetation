
# process data ------------------------------------------------------------

treat_dd <- read.csv("Data/treatment_code.csv")  # dataframe for treatment


# > above-ground biomass ----------------------------------------------------


# . read data and arrange -------------------------------------------------


# October 2013
ab_oct2013 <- read.csv("Data/harvest/above_biomass_2013_oct.csv") %>%    # read data
  select(-X, -side, -treatment, -herb) %>%                               # remove unnecessary column
  mutate(year  = 2013, month = 10)                                       # add new columns: month and year


# October 2014
ab_oct2014 <- read.csv("Data/harvest/above_biomass_2014_oct.csv") %>% 
  select(-type) %>%
  filter(spp != "vortis") %>%                                            # not sure what it is....
  spread(key = spp, value = mass) %>% 
  mutate(year = 2014, month = 10)


# October 2015. It doen's have sp biomass data
ab_oct2015 <- read.csv("Data/harvest/above_biomass_2015_oct.csv") %>% 
  rename(total = mass) %>% 
  mutate(year = 2015, month = 10) %>% 
  left_join(treat_dd, by = "plot")


# April 2014
ab_apr2014 <- read.csv("Data/harvest/above_biomass_2014_apr.csv") %>% 
  select(-live, -proportion, -scaled, -X, -treatment) %>% 
  spread(key = spp, value = mass) %>% 
  mutate(year = 2014, month = 4)


# April 2015
ab_apr2015 <- read.csv("Data/harvest/above_biomass_2015_apr.csv") %>% 
  mutate(year = 2015, month = 4)


# April 2016
ab_apr2016 <- readWorksheetFromFile("Data/harvest/above_biomass_2016_mar.xlsx",
                                    sheet = 1) %>% 
  select(-Date, -Total.harvested.weight..g.) %>% 
  rename(plot = Plot) %>% 
  mutate(year = 2016, month = 4)


# merge the above dataframes

ab_list <- list(ab_oct2013, ab_oct2014, ab_apr2014, ab_apr2015, ab_apr2016)  # list of all data except for ab_oct2015 which doesn't have sp biomass  

ab_list <- llply(ab_list, function(x){                                       # fix species names
  l <- x
  names(l) <- gsub(pattern = " ", ".", names(l))                             # change space to period (.). e.g. Axonopus affinis -> Axonopus.affinis
  names(l) <- gsub(pattern = "spp|sp[.]", "sp", names(l))                    # change spp or sp. to sp
  return(l)
})

ab_biom <- rbind.fill(ab_list) %>%                                           # bind rows
  select(plot, year, month, order(colnames(.))) %>%                          # re-order columns
  left_join(treat_dd, by = "plot")                                           # combine with treatment coordinates

ab_biom[is.na(ab_biom)] <- 0                                                 # turn NA into 0





# . fix column (species) names --------------------------------------------

ab_biom_cols <- colnames(ab_biom)

# Anagallis.arvensis
aa <- ab_biom_cols[grepl("^Anag", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Anagallis.arvensis", CombineCol = aa)

# Axonopus.fissifolius
af <- ab_biom_cols[grepl("Axonopus", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Axonopus.fissifolius", CombineCol = af)

# Bothriochloa.macra
bm <- ab_biom_cols[grepl("Bothriochloa|macra", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Bothriochloa.macra", CombineCol = bm)

# Bidens.pilosa
bp <- ab_biom_cols[grepl("Bidens", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Bidens.pilosa", CombineCol = bp)

# Chicorium.intybus -> Cicorium.intybus
ab_biom <- rename(ab_biom, Cicorium.intybus = Chicorium.intybus)

# Commelina.cyanea
cc <- ab_biom_cols[grepl("Com*elina", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Commelina.cyanea", CombineCol = cc)

# Conyza.bonariensis
cb <- ab_biom_cols[grepl("Conyza", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Conyza.bonariensis", CombineCol = cb)

# Cymbopogon.refractus
cr <- ab_biom_cols[grepl("Cym.*gon", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Cymbopogon.refractus", CombineCol = cr)

# Cynodon.dactlyon
cd <- ab_biom_cols[grepl("Cynodon", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Cynodon.dactlyon", CombineCol = cd)

# Cyperus.sesquiflorus
cs <- ab_biom_cols[grepl("Cyperus", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Cyperus.sesquiflorus", CombineCol = cs)

# dead
dead <- ab_biom_cols[grepl("Dead", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "dead", CombineCol = dead)

# Dichelachne.sp
ds <- ab_biom_cols[grepl("Dichelachne", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Dichelachne.sp", CombineCol = ds)

# Digitaria.sp
ds2 <- ab_biom_cols[grepl("Digitaria", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Digitaria.sp", CombineCol = ds2)

# Eragrostis.curvula
ec <- ab_biom_cols[grepl("Eragrostis", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Eragrostis.curvula", CombineCol = ec)

# Hypochaeris.radicata
hr <- ab_biom_cols[grepl("Hypochaeris", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Hypochaeris.radicata", CombineCol = hr)

# Juncus.sp
js <- ab_biom_cols[grepl("Juncus", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Juncus.sp", CombineCol = js)

# Lotus.corniculatus
lc <- ab_biom_cols[grepl("Lotus", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Lotus.corniculatus", CombineCol = lc)

# Medicago.sativa
ms <- ab_biom_cols[grepl("Medicago", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Medicago.sativa", CombineCol = ms)

# Microlaena.stipoides
ms2 <- ab_biom_cols[grepl("Microlaena", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Microlaena.stipoides", CombineCol = ms2)

# Ornithopus.compressus
oc <- ab_biom_cols[grepl("Orn.*thopus", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Ornithopus.compressus", CombineCol = oc)

# Oxalis.corniculata
oc2 <- ab_biom_cols[grepl("Oxalis", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Oxalis.corniculata", CombineCol = oc2)

# Paspalidum.sp
ps <- ab_biom_cols[grepl("Paspalidum", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Paspalidum.sp", CombineCol = ps)

# Paspalum.dilitatum
pd <- ab_biom_cols[grepl("Paspalum", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Paspalum.dilitatum", CombineCol = pd)

# Plantago.lanceolata
pl <- ab_biom_cols[grepl("Plantago", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Plantago.lanceolata", CombineCol = pl)

# Senecio.madagascariensis
sm <- ab_biom_cols[grepl("Senecio", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol  = "Senecio.madagascariensis", CombineCol = sm)

# Setaria.parviflora
sp <- ab_biom_cols[grepl("Setaria", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Setaria.parviflora", CombineCol = sp)

# Sida.rhombifolia
sr <- ab_biom_cols[grepl("Sida", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Sida.rhombifolia", CombineCol = sr)

# Sporobolus.sp
ss <- ab_biom_cols[grepl("Sporobolus", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Sporobolus.sp", CombineCol = ss)

# Verbena.bonariensis
vb <- ab_biom_cols[grepl("Verb.na", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Verbena.bonariensis", CombineCol = vb)

# Vicia.sativa
vs <- ab_biom_cols[grepl("Vicia", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol = "Vicia.sativa", CombineCol = vs)

# Whalenbergia.sp
ws <- ab_biom_cols[grepl("W.*lenbergia", ab_biom_cols)]
ab_biom <- combine_cols(ab_biom, KeepCol  = "Whalenbergia.sp", CombineCol = ws)

# unknown
unk <- ab_biom_cols[grepl("unk|uns|Fennel-like|other", ab_biom_cols, ignore.case = TRUE)]
ab_biom <- combine_cols(ab_biom, KeepCol  = "unknown", CombineCol = unk)


# arrange dataframe
ab_biom <- ab_biom %>% 
  select(plot, year, month, treatment, herb, side, everything()) %>% 
  arrange(year, month, treatment)
head(ab_biom)




# > prepare df for further analysis -----------------------------------------


# . sp biomass ------------------------------------------------------------


ab_spp_biom <- select(ab_biom, -dead, -unknown)


# species names
spp_names <- ab_spp_biom %>% 
  select(-plot, -year, -month, -treatment, -herb, -side) %>%  # remove unnecessary columns
  names() %>%                                                 # get column names (i.e. sp names)
  sort()                                                      # order alphabetically


# . biomass: total/live/dead ----------------------------------------------


# get biomass (total/live/dead)
ab_tot_biom <-  ab_biom %>% 
  transmute(plot, year, month, treatment, herb, dead,           # keep those columns
            live  = rowSums(.[, c(spp_names, "unknown")]),      # compute row sums for species. "." is inherited data frame from right abvoe (i.e. ab_biom)
            total = live + dead) %>%                            # total biomass
  bind_rows(ab_oct2015) %>%                                     # combine with total biomass in October 2015
  select(-side) %>% 
  mutate(date   = as.Date(paste(year, month, "15", sep = "-")),
         time   = factor(date, labels = 1:length(unique(date))),
         season = paste(year, month.abb[month], sep = "-"))
some(ab_tot_biom)



# . diversity indices -----------------------------------------------------


div_2016 <- ab_biom %>%
  transmute(year, month, plot, treatment, herb,
            H = diversity(.[spp_names]),         # shannon's index
            S = specnumber(.[spp_names]),        # number of spp
            J = H/log(S))                        # evenness




# . PFG ratios ------------------------------------------------------------

# df for PFG
sp_pfg <- read.csv("Data/spp_PFG_2016.csv")


pfg_2016 <- ab_spp_biom %>% 
  gather(spp, value, -plot, -year, -month, -treatment, -herb, -side) %>%  # reshape: gather sp coumns to rows
  left_join(sp_pfg, by = "spp") %>%                                       # merge with PFG df
  group_by(plot, year, month, treatment, herb, side) %>%                  # get total biomass and c3 ratios for each plot
  summarise(total   = sum(value),
            c3ratio = sum(value[pfg == "c3"]) / total,
            c4grass = sum(value[pfg == "c4"])) %>% 
  ungroup()                                                               # remove grouping factors
