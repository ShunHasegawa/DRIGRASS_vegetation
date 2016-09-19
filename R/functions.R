add.factors <- function(x){
  treatment<-x$plot
  treatment<-recode(treatment, "c(2,10,12,18,24,27,36,39,44,46,53,55)='Ambient'")
  treatment<-recode(treatment, "c(1,8,13,16,26,29,31,33,47,50,52,54)='Pulsed drought'")
  treatment<-recode(treatment, "c(6,7,15,20,21,23,34,38,42,49,51,58)='Ambient(no shelter)'")
  treatment<-recode(treatment, "c(3,17,22,40,45,59)='Seasonal'")
  treatment<-recode(treatment, "c(4,5,11,19,28,30,35,37,41,43,56,60)='Drought'")
  treatment<-recode(treatment, "c(9,14,25,32,48,57)='Increased'")
  x$treatment<-treatment
  x$plot <- as.factor(x$plot)
  
  herb <- x$plot
  herb<-recode(herb, "c(4,6,8,10,12,13,19,20,23,26,27,28,31,35,36,38,41,42,44,50,51,54,55,60)='Added'")
  herb<- recode(herb, "c(1,11,14,15,16,17,18,2,21,22,24,25,29,3,30,32,33,34,37,39,40,43,45,46,47,48,49,5,52,53,56,57,58,59,7,9)='Ambient'")
  x$herb<-herb
  
  side <- x$plot
  side<-recode(side, "c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)='Vineyard'")
  side<-recode(side, "c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60)='Fence'")
  x$side <- side
  x$treatment <- factor(x$treatment)
  x$side <- factor(x$side)
  x$herb <- factor(x$herb)
  x$treatment <- factor(x$treatment, levels = c("Ambient", "Ambient(no shelter)" ,"Increased","Drought","Pulsed drought", "Seasonal"))
  x
}



# compute SE
se <- function(...) ci(...)[4]




# compute number of observations
get_n <- function(x) sum(!is.na(x))




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
