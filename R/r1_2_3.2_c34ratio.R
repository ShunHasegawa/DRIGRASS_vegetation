
# > summer ------------------------------------------------------------------


# . test rain x herb ------------------------------------------------------

range(pfg_by_rxh_s$c34ratios)
par(mfrow = c(1, 2))
boxplot(logit(c34ratios) ~ treatment * herb * year, data = pfg_by_rxh_s)
boxplot(log(c34ratios + .001) ~ treatment * herb * year, data = pfg_by_rxh_s)
create_trans_boxplot(c34ratios + .001 ~ treatment * herb * year, data = pfg_by_rxh_s)


# use log
c34r_by_rh_s_m1  <- lmer(log(c34ratios + .001) ~ treatment * herb * year + (1|plot),
                         data = pfg_by_rxh_s)
c34r_by_rh_s_fin <- get_persim_lmer(c34r_by_rh_s_m1, show.model.res = TRUE)
Anova(c34r_by_rh_s_fin, test.statistic = "F")
plot(c34r_by_rh_s_fin)
qqnorm(resid(c34r_by_rh_s_fin))
qqline(resid(c34r_by_rh_s_fin))
## one outlier
rmval <- which.min(resid(c34r_by_rh_s_fin))
c34r_by_rh_s_m2 <- update(c34r_by_rh_s_m1, subset = -rmval)
qqnorm(resid(c34r_by_rh_s_m2))
qqline(resid(c34r_by_rh_s_m2))
get_persim_lmer(c34r_by_rh_s_m2, show.model.res = TRUE)
## no herb effect




# . rainfall treatment ----------------------------------------------------


# the above anlaysis showed no herb effect so use the complete dataset
par(mfrow = c(1, 2))
boxplot(logit(c34ratios) ~ treatment * year, data = pfg_ed_s)
boxplot(log(c34ratios + .001) ~ treatment * year, data = pfg_ed_s)
create_trans_boxplot(c34ratios + .001 ~ treatment * year, data = pfg_ed_s)

c34r_s_m1    <- lmer(log(c34ratios + .001) ~ treatment * year + (1 | plot), 
                     data = pfg_ed_s)
c34r_s_m_fin <- get_persim_lmer(c34r_s_m1)

Anova(c34r_s_m1, test.statistic = "F")
plot(c34r_s_m1)
qqnorm(resid(c34r_s_m1))
qqline(resid(c34r_s_m1))
## remove outliers
rmv <- which.min(resid(c34r_s_m1))
c34r_s_m2 <- update(c34r_s_m1, subset = -rmv)
c34r_s_m_fin_2 <- get_persim_lmer(c34r_s_m2)
plot(c34r_s_m_fin_2)
qqnorm(resid(c34r_s_m_fin_2))
qqline(resid(c34r_s_m_fin_2))
Anova(c34r_s_m_fin_2, test.statistic = "F")
## same result so just use the above



# > winter ------------------------------------------------------------------


# . rain x herb --------------------------------------------------------------

par(mfrow = c(1, 2))
create_trans_boxplot(c34ratios ~ treatment * herb * year, data = pfg_by_rxh_w)
c34r_by_rh_w_m1  <- lmer(log(c34ratios) ~ treatment * herb * year + (1|plot),
                         data = pfg_by_rxh_w)
c34r_by_rh_w_fin <- get_persim_lmer(c34r_by_rh_w_m1, show.model.res = TRUE)
Anova(c34r_by_rh_w_fin, test.statistic = "F")
plot(c34r_by_rh_w_fin)
qqnorm(resid(c34r_by_rh_w_fin))
qqline(resid(c34r_by_rh_w_fin))
## remove outlier
rmval <- which.min(resid(c34r_by_rh_w_m1))
c34r_by_rh_w_m2 <- update(c34r_by_rh_w_m1, subset = -rmval)
qqnorm(resid(c34r_by_rh_w_m2))
qqline(resid(c34r_by_rh_w_m2))
get_persim_lmer(c34r_by_rh_w_m2, show.model.res = TRUE)
## no herb effect




# . rain ------------------------------------------------------------------

# the above test showed no herb effect, so use the complete dataset
create_trans_boxplot(c34ratios ~ treatment * year, data = pfg_ed_w)

c34r_w_m1    <- lmer(log(c34ratios) ~ treatment * year + (1 | plot), data = pfg_ed_w)
c34r_w_m_fin <- get_persim_lmer(c34r_w_m1)
Anova(c34r_w_m_fin, test.statistic = "F")
plot(c34r_w_m_fin)
qqnorm(resid(c34r_w_m_fin))
qqline(resid(c34r_w_m_fin))
## remove an outliser
rmval <- which.min(resid(c34r_w_m_fin))
c34r_w_m2 <- update(c34r_w_m1, subset = -rmval)
qqnorm(resid(c34r_w_m2))
qqline(resid(c34r_w_m2))
Anova(c34r_w_m2, test.statistic = "F")
## same as above, so just use the above 
