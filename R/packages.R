library(XLConnect)    # read, write excel files
library(MASS)         # box-cox
library(plyr)         # organise data
library(dplyr)        # organise dataframes
library(tidyr)        # rehape df
library(car)          # recode, Anova, etc
library(gmodels)      # compute CI and SE
library(vegan)        # multivariate analysis
library(visreg)       # visualise anova results
library(multcomp)     # tukey-hsd post-hoc test
library(ggplot2)      # ggplot
library(RColorBrewer) # generates color palletes for plot
library(lsmeans)      # run lease squrare mean tests
library(lme4)         # run LMM analysis using lmer
library(grid)         # bind ggplots
library(lmerTest)     # F test for lmer objects
library(MuMIn)        # find a model with the lowest AICc
library(broom)        # export an anova object as a data frame
library(parallel)     # set up multicores to run permutaion quickly using parallel processing
library(foreach)      # set up multicores to run permutaion quickly using parallel processing
library(doParallel)   # set up multicores to run permutaion quickly using parallel processing
library(mvabund)      # forth coner analysi
library(lattice)      # create levelplots
require(gridExtra)    # arrange multiple levelplot from lattice
library(data.table)   # tstrsplit