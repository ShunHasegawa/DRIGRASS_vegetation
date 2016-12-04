add.factors <- function(x){
  treatment<-x$plot
  treatment<-recode(treatment, "c(2,10,12,18,24,27,36,39,44,46,53,55)='Ambient'")
  treatment<-recode(treatment, "c(1,8,13,16,26,29,31,33,47,50,52,54)='Reduced frequency'")
  treatment<-recode(treatment, "c(6,7,15,20,21,23,34,38,42,49,51,58)='No.shelter'")
  treatment<-recode(treatment, "c(3,17,22,40,45,59)='Summer drought'")
  treatment<-recode(treatment, "c(4,5,11,19,28,30,35,37,41,43,56,60)='Reduced'")
  treatment<-recode(treatment, "c(9,14,25,32,48,57)='Increased'")
  x$treatment<-treatment
  x$plot <- as.factor(x$plot)
  
  herb <- x$plot
  herb<-recode(herb, "c(4,6,8,10,12,13,19,20,23,26,27,28,31,35,36,38,41,42,44,50,51,54,55,60)='Added'")
  herb<- recode(herb, "c(1,11,14,15,16,17,18,2,21,22,24,25,29,3,30,32,33,34,37,39,40,43,45,46,47,48,49,5,52,53,56,57,58,59,7,9)='Control'")
  x$herb<-herb
  
  side <- x$plot
  side<-recode(side, "c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)='Vineyard'")
  side<-recode(side, "c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60)='Fence'")
  x$side <- side
  x$treatment <- factor(x$treatment)
  x$side <- factor(x$side)
  x$herb <- factor(x$herb)
  # x$treatment <- factor(x$treatment, levels = c("Ambient", "Ambient(no shelter)" ,"Increased","Reduced","Reduced frequency", "Summer drought"))
  x
}




# compute SE
se <- function(...){
  ci(...)[4]
}




# compute number of observations
get_n <- function(x){
  sum(!is.na(x))
}



# this returns Mean(SE)
get_mean_se <- function(x, round.val = 2, ...){
  M  <- round(mean(x, ...), round.val)  # mean
  SE <- round(se(x, ...), round.val)    # SE
  paste0(M, "(", SE, ")")
}


# This returns response ratios aginst RR
get_rr <- function(x, contr.pos = 1){
  # contr.pos: element position for control (i.e. Ambient)
  x / x[contr.pos]
}



# this function generates box-whisker plots with common transformations

create_trans_boxplot <- function(x, data, ...){ # x = formula
  
  # get box-cox value
  a <- boxcox(x, data = data, plotit = FALSE, ...)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black")  # use red color for negative values
  
  # create formulas for each transformation
  f_cha     <- as.character(x)
  f_log     <- as.formula(paste("log(", f_cha[2], ") ~ ", f_cha[3]))
  f_sqrt    <- as.formula(paste("sqrt(", f_cha[2], ") ~ ", f_cha[3]))
  f_pw      <- as.formula(paste("(", f_cha[2], ")^(1/3) ~ ", f_cha[3]))
  f_inv     <- as.formula(paste("1 / (", f_cha[2], ") ~ ", f_cha[3]))
  f_boxcox  <- as.formula(paste("(", f_cha[2], ")^(BCmax) ~ ", f_cha[3]))
  f_list    <- list('raw' = x,
                    'log' = f_log,
                    'sqrt' = f_sqrt,
                    'power(1/3)' = f_pw,
                    'inverse' = f_inv)
  par(mfrow = c(2, 3))
  l_ply(names(f_list), function(x) boxplot(f_list[[x]], data = data, main = x))
  boxplot(f_boxcox, main = "", sep = "=", data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), col.main = texcol)
  par(mfrow = c(1,1))
}




ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}




save_png600 <- function(...) png(..., res = 600, units = "in")




combine_cols <- function(.data, KeepCol, CombineCol){
  # This function combine (sum) multiple coolumns to a single column
  # KeepCol: column name to be kept
  # CombineCol: column names to be combined
  
  .data[KeepCol] <- rowSums(.data[CombineCol], na.rm = TRUE)  # sum of columns to be combined
  RemoveCol      <- CombineCol[CombineCol != KeepCol]         # remove columns after combining
  .data          <- .data[!names(.data) %in% RemoveCol]
  return(.data)
}




# this transforms P values to star maks
get_star <- function(pval, dagger = TRUE){
  
  # dagger mark for p < 0.1
  dg <- ifelse(dagger, expression("\u2020"), ".")
  
  cut(pval, right = FALSE,
      breaks = c(0, .1, .05, .01, .001, 1.1),  
      labels = c("***", "**", "*", dg, ""))
}




# set graphic parameters --------------------------------------------------


theme_set(theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

# define graphic background
science_theme <- theme(panel.border      = element_rect(color = "black"),
                       panel.grid.major  = element_blank(), 
                       panel.grid.minor  = element_blank(), 
                       legend.position   = c(.91, .91),
                       legend.title      = element_blank(),
                       legend.background = element_blank(),
                       legend.key        = element_blank(),
                       legend.text  = element_text(size = 8),
                       legend.key.width  = unit(1, "lines"),
                       legend.key.height = unit(.8, "lines"),
                       axis.ticks.length = unit(-.2, "lines"),
                       axis.text.x       = element_text(margin = margin(5),
                                                        size = 8),
                       axis.text.y       = element_text(margin = margin(0, 5),
                                                        size = 8),
                       axis.title.y      = element_text(margin = margin(0, 10),
                                                        size = 10),
                       axis.title.x      = element_text(size = 10))



# This function returns the persimonious model chosen among mdoels with dAIC <=
# 2. (dAIC; difference in AICc from the lowest AICc)
get_persim_lmer <- function(lmermod, show.model.res = FALSE){
  options(na.action = "na.fail")                                      # na.action option has to "na.fail" to run dredge function below
  m         <- update(lmermod, REML = FALSE)                          # Models have to be estimated by ML instead of REML to be compared with one another by dredge below.
  all_m     <- dredge(m)                                              # dredge lists models with all combination of fixed terms, includ. interactions, cantined in the passed model. Then it computes AICc and dAICc.
  cat("Models with dAICc < 2...\n\n")                                 # print models with dAIC<2
  print(subset(all_m, delta <= 2))
  best_ms   <- get.models(all_m, subset = delta <=2)                  # choose best models (i.e. ones with dAIC < 2)
  if(show.model.res){                                                 # if show.model.res = TRUE, print the results of anova for the best models. it may takes some time
    allres <- llply(best_ms, function(x){
      Anova(update(x, REML = TRUE), test.statistic = "F")             # get Anova result, follwing switing back to REML
    })
    cat("\n\nAnova tables with F test for the above models...\n\n")
    print(allres)
  }
  term_n    <- laply(best_ms, function(x) nrow(anova(x)))             # get number of fixed terms in each mdoel
  fin_m     <- best_ms[[which.min(term_n)]]                           # choose the model with the smallest number of fixed terms
  fin_m     <- update(fin_m, REML = TRUE)                             # switch the estimate back to REML from ML
  return(fin_m)
}


# PRC related functions ---------------------------------------------------


# this function returns a df with effect size (canonical coeeficience) from prc
# results
get_prc_effect_df <- function(prc.mod, site.df){
  
  # prc.mod: prc model
  # site.df: site df used for prc
  
  prc_res  <- summary(prc.mod, scaling = 3)        # summary of prc analys
  prc_site <- data.frame(prc_res$sites, site.df)  # site scores given by prc; merged with site.df
  effect_d <- prc_site %>%                         # get traetment effect size
    group_by(year, treatment) %>% 
    summarise(value = mean(CAP1)) %>% 
    group_by(year) %>% 
    mutate(eff = value - value[treatment == "Ambient"], 
           colval = as.character(mapvalues(treatment, treatment, rain_cols)))
  return(effect_d)
}


# This creates a PRC plot using a df with effect size of PRC (see
# get_prc_effect_df)
get_prc_fig <- function(effect.df, sp.df, mai = c(1, .8, .2, 1.5), ...){
  
  # effect.df: df with effect size computed from PRC result (get_prc_effect_df)
  # sp.df    : species and species scores to be plotted together with the effect size
  # mai      : figure margin
  # ...      : other graphical parameters
  
  par(mai = mai)
  plot(eff ~ as.numeric(year), data = effect.df, axes = F, xlab = "Year", 
       ylab = "Effect", type = "n", ...)
  year_lab <- levels(effect.df$year)
  axis(side = 1, at = 1:length(year_lab), labels = year_lab)
  axis(side = 2, las = 2)
  box()
  d_ply(effect.df, .(treatment), function(x){
    lines(eff ~ as.numeric(year), data = x, col = colval)
    points(eff ~ as.numeric(year), data = x, col = colval, pch = 19)
  })
  legend("bottomleft", col = rain_cols, legend = levels(effect.df$treatment), pch = 19, bty = "n")
  axis(side = 4, tck = .03, at = sp.df[, 1], 
       labels = row.names(sp.df), las = 2, cex.axis = .7)
}

