### cluster runs for testing SAD out now that it has stochasticity

# set working directory
# setwd('~/Projects/iliketurtles3/code/')

# remove all variables from environment
rm(list = ls())

# load libraries
library(parallel)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)
library(tictoc)
library(beepr)
library(slider)

# source functions
source('initialize_population_test2.R')
source('mating function/OSRs_to_betas.R')
source('emergence_success.R')
source('probability_male.R')
source('mating function/OSRs_to_betas.R')
source('SAD_cluster_stitch.R')

# arguments <- data.frame(Var1 = 'GM_base', 
#                         Var2 = 5.02, 
#                         Var3 = 1000, 
#                         Var4 = FALSE)

# models
# models <- c('P_base', 'GM_base')
models <- c('P_base')

# beta values to get full fertilization of females
# OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
OSRs <- c(0.05, 0.1, 0.15, 0.2)

betas <- as.numeric(OSRs_to_betas(OSRs))

# years to run the model for
burn_ins <- c(800)

# temperature stochasticity?
# temp_stochasticities <- c(FALSE, TRUE)
temp_stochasticities <- c(TRUE)

# number of sims to run
nsims <- 10

# make dataframe of all combinations of arguments
DF <- expand.grid(models,
                  betas,
                  burn_ins,
                  temp_stochasticities, 
                  nsims) %>%
  arrange(Var3, desc(Var2))

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {

  arguments[i] <- list(DF[i, ])

}

########### do the runs ########################################################

# # machine runs
# tic()
# lapply(X = arguments,
#        FUN = initialize_population_test2)
# toc()

# make a noise when done
# beep()


# cluster runs
mclapply(X = arguments,
         FUN = initialize_population_test2,
         mc.cores = 50)

# OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
# betas <- as.numeric(OSRs_to_betas(OSRs))

# stitch things together
SAD_cluster_stitch(burn_ins, nsims, betas, temp_stochasticities)
