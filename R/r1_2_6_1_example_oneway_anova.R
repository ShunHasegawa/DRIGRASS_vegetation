#################################
# Example analysis -April 2014- #
#################################

# prepare data frame --------------------------------------------------------------

d         <- read.csv("Output/Data/formatted/formatted_Apr2014_harvest_SHUN.csv")  # read data
d         <- subset(d, treatment != "No.shelter")                                  # remove no-sheltered
treat_var <- c("plot", "year", "month", "season", "treatment", "herb")             # treatment colummn names
spp_var   <- setdiff(names(d), treat_var)                                          # species names
d$total   <- rowSums(d[, spp_var])                                                 # total biomass
d$live    <- d$total - d$Dead                                                      # live biomass


# anlysis -----------------------------------------------------------------


# . test Rain x Herb -----------------------------------------------------------

d_rxh <- subset(d, treatment %in% c("Pulsed.drought", "Ambient", "Drought"))
                    
create_trans_boxplot(total ~ treatment * herb, data = d_rxh)  # data visualisation                    
m_rxh <- lm(total ~ treatment * herb, data = d_rxh)           # anova model
anova(m_rxh)                                                  # anova result
    # no herb effects
par(mfrow = c(2, 2))                                          # model diagnosis
plot(m_rxh)


# test Rain ---------------------------------------------------------------

# The above test shows no herb effect so use the complete datset
create_trans_boxplot(total ~ treatment, data = d)
m_r <- lm(total ~ treatment, data = d)
anova(m_r)
    # There is a significant treatment effect
par(mfrow = c(2, 2))
plot(m_r)

# post-hoc test
symbols <- cld(glht(m_r, linfct = mcp(treatment = "Tukey")),                      # get symbols representing significant difference between treatmens 
               decreasing = TRUE)$mcletters$Letters
symbols
