
# > summer ------------------------------------------------------------------



# . test rain x herb ------------------------------------------------------

range(pfg_by_rxh_s$c3ratio)
boxplot(logit(c3ratio) ~ treatment * herb * year, data = pfg_by_rxh_s)
create_trans_boxplot(c3ratio + 1 ~ treatment * herb * year, data = pfg_by_rxh_s)


# use logig
c3_by_rh_s_m1  <- lmer(logit(c3ratio) ~ treatment * herb * year + (1|plot),
                       data = pfg_by_rxh_s)
c3_by_rh_s_fin <- get_persim_lmer(c3_by_rh_s_m1, show.model.res = TRUE)
Anova(c3_by_rh_s_fin, test.statistic = "F")
plot(c3_by_rh_s_fin)
qqnorm(resid(c3_by_rh_s_fin))
qqline(resid(c3_by_rh_s_fin))
# no herb effect




# . rainfall treatment ----------------------------------------------------


# the above anlaysis showed no herb effect so use the complete dataset
boxplot(logit(c3ratio) ~ treatment * year, data = pfg_ed_s)
c3_s_m1    <- lmer(logit(c3ratio) ~ treatment * year + (1 | plot), data = pfg_ed_s)
c3_s_m_fin <- get_persim_lmer(c3_s_m1)

Anova(c3_s_m1, test.statistic = "F")
plot(c3_s_m1)
qqnorm(resid(c3_s_m1))
qqline(resid(c3_s_m1))
# remove outliers
rmv <- sort(resid(c3_s_m1), index.return = TRUE)$ix[1:5]
c3_s_m2 <- update(c3_s_m1, subset = -rmv)
c3_s_m_fin_2 <- get_persim_lmer(c3_s_m2)
plot(c3_s_m_fin_2)
qqnorm(resid(c3_s_m_fin_2))
qqline(resid(c3_s_m_fin_2))
Anova(c3_s_m_fin_2, test.statistic = "F")
# there seem to be significant interaction so keep interactive terms.
c3_s_m_fin <- c3_s_m1 
Anova(c3_s_m_fin, test.statistic = "F")




# > winter ------------------------------------------------------------------


# . rain x herb --------------------------------------------------------------
boxplot(logit(c3ratio) ~ treatment * herb * year, data = pfg_by_rxh_w)
create_trans_boxplot(c3ratio ~ treatment * herb * year, data = pfg_by_rxh_w)
c3_by_rh_w_m1  <- lmer(sqrt(c3ratio) ~ treatment * herb * year + (1|plot),
                       data = pfg_by_rxh_w)
c3_by_rh_w_fin <- get_persim_lmer(c3_by_rh_w_m1, show.model.res = TRUE)
Anova(c3_by_rh_w_fin, test.statistic = "F")
plot(c3_by_rh_w_fin)
qqnorm(resid(c3_by_rh_w_fin))
qqline(resid(c3_by_rh_w_fin))
# no herb effect





# . rain ------------------------------------------------------------------

# the above test showed no herb effect, so use the complete dataset
boxplot(logit(c3ratio) ~ treatment * year, data = pfg_ed_w)
create_trans_boxplot(c3ratio ~ treatment * year, data = pfg_ed_w)

c3_w_m1    <- lmer(sqrt(c3ratio) ~ treatment * year + (1 | plot), data = pfg_ed_w)
c3_w_m_fin <- get_persim_lmer(c3_w_m1)
Anova(c3_w_m_fin, test.statistic = "F")
plot(c3_w_m_fin)
qqnorm(resid(c3_w_m_fin))
qqline(resid(c3_w_m_fin))




