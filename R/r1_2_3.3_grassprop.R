
# > summer ------------------------------------------------------------------


# . test rain x herb ------------------------------------------------------

range(pfg_by_rxh_s$grprop)

boxplot(logit(grprop) ~ treatment * herb * year, data = pfg_by_rxh_s)
boxplot(asin(grprop) ~ treatment * herb * year, data = pfg_by_rxh_s)
create_trans_boxplot(grprop ~ treatment * herb * year, data = pfg_by_rxh_s)
## remove one complete outlier
pfg_by_rxh_s2 <- pfg_by_rxh_s[-(which.min(pfg_by_rxh_s$grprop)), ]
boxplot(asin(grprop) ~ treatment * herb * year, data = pfg_by_rxh_s2)
grprop_by_rh_s_m1  <- lmer(asin(grprop) ~ treatment * herb * year + (1|plot),
                           data = pfg_by_rxh_s2)
grprop_by_rh_s_fin <- get_persim_lmer(grprop_by_rh_s_m1, show.model.res = TRUE)
Anova(grprop_by_rh_s_fin, test.statistic = "F")
plot(grprop_by_rh_s_fin)
qqnorm(resid(grprop_by_rh_s_fin))
qqline(resid(grprop_by_rh_s_fin))
## remove an outlier
rmval <- sort(resid(grprop_by_rh_s_fin), index.return = TRUE)$ix[1:2]
grprop_by_rh_s_m2 <- update(grprop_by_rh_s_m1, subset = -rmval)
qqnorm(resid(grprop_by_rh_s_m2))
qqline(resid(grprop_by_rh_s_m2))
Anova(grprop_by_rh_s_m2, test.statistic = "F")
grprop_by_rh_s_fin2 <- get_persim_lmer(grprop_by_rh_s_m2, show.model.res = TRUE)
## no herb effect




# . rainfall treatment ----------------------------------------------------


## the above anlaysis showed no herb effect so use the complete dataset
boxplot(logit(grprop) ~ treatment * year, data = pfg_ed_s)
boxplot(asin(grprop) ~ treatment * year, data = pfg_ed_s)
create_trans_boxplot(grprop ~ treatment * year, data = pfg_ed_s)
## remove two complete outliers
pfg_ed_s2 <- pfg_ed_s[-order(pfg_ed_s$grprop)[1:2],]
boxplot(asin(grprop) ~ treatment * year, data = pfg_ed_s2)

grprop_s_m1    <- lmer(asin(grprop) ~ treatment * year + (1 | plot),
                       data = pfg_ed_s2)
grprop_s_m_fin <- get_persim_lmer(grprop_s_m1, show.model.res = TRUE)
Anova(grprop_s_m1, test.statistic = "F")
Anova(grprop_s_m_fin, test.statistic = "F")
plot(grprop_s_m1)
qqnorm(resid(grprop_s_m1))
qqline(resid(grprop_s_m1))
## remove outliers
rmv <- sort(resid(grprop_s_m1), index.return = TRUE)$ix[1:4]
grprop_s_m2 <- update(grprop_s_m1, subset = -rmv)
grprop_s_m_fin_2 <- get_persim_lmer(grprop_s_m2)
plot(grprop_s_m_fin_2)
qqnorm(resid(grprop_s_m_fin_2))
qqline(resid(grprop_s_m_fin_2))
Anova(grprop_s_m2, test.statistic = "F")
Anova(grprop_s_m_fin_2, test.statistic = "F")
## Interaction was identified to be significant in the starting model but not
## included in the final model with the lowest AICc.
AIC(grprop_s_m2)
AIC(grprop_s_m_fin_2)
## AICc in the initial model is so much higher that the final model. This result
## was probably due to poort transformation and non-normal errors. Interaction
## was probably not significant.




# > winter ------------------------------------------------------------------


# . rain x herb --------------------------------------------------------------
par(mfrow = c(1, 2))
boxplot(logit(grprop) ~ treatment * herb * year, data = pfg_by_rxh_w)
boxplot(asin(grprop) ~ treatment * herb * year, data = pfg_by_rxh_w)

grprop_by_rh_w_m1  <- lmer(asin(grprop) ~ treatment * herb * year + (1|plot),
                           data = pfg_by_rxh_w)
grprop_by_rh_w_fin <- get_persim_lmer(grprop_by_rh_w_m1, show.model.res = TRUE)
Anova(grprop_by_rh_w_m1, test.statistic = "F")
plot(grprop_by_rh_w_m1)
qqnorm(resid(grprop_by_rh_w_m1))
qqline(resid(grprop_by_rh_w_m1))
## remove an outlier
rmval <- which.min(resid(grprop_by_rh_w_m1))
grprop_by_rh_w_m2 <- update(grprop_by_rh_w_m1, subset = -rmval)
plot(grprop_by_rh_w_m2)
qqnorm(resid(grprop_by_rh_w_m2))
qqline(resid(grprop_by_rh_w_m2))
Anova(grprop_by_rh_w_m2, test.statistic = "F")
grprop_by_rh_w_fin_2 <- get_persim_lmer(grprop_by_rh_w_m2, show.model.res = TRUE)
Anova(grprop_by_rh_w_fin_2, test.statistic = "F")
## no herb effect




# . rain ------------------------------------------------------------------

# the above test showed no herb effect, so use the complete dataset
boxplot(logit(grprop) ~ treatment * year, data = pfg_ed_w)
boxplot(asin(grprop) ~ treatment * year, data = pfg_ed_w)

grprop_w_m1    <- lmer(asin(grprop) ~ treatment * year + (1 | plot), data = pfg_ed_w)
grprop_w_m_fin <- get_persim_lmer(grprop_w_m1)
Anova(grprop_w_m1, test.statistic = "F")
plot(grprop_w_m1)
qqnorm(resid(grprop_w_m1))
qqline(resid(grprop_w_m1))
## remove an outlier
rmval <- which.min(resid(grprop_w_m1))
grprop_w_m2 <- update(grprop_w_m1, subset = -rmval)
plot(grprop_w_m2)
qqnorm(resid(grprop_w_m2))
qqline(resid(grprop_w_m2))
grprop_w_m_fin2 <- get_persim_lmer(grprop_w_m2)
Anova(grprop_w_m_fin2, test.statistic = "F")
## the above result may be driven by the outlier so use the one withought outlier
grprop_w_m_fin <- grprop_w_m_fin2
