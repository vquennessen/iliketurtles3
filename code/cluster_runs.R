### cluster runs

# load libraries
library(parallel)
library(dplyr)

# set working directory
# setwd('~/Projects/iliketurtles3/code')

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OSRs_to_betas.R')

# total temp increases
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
# scenarios <- c(0.5)

# OSR values to get full fertilization of females
OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
# OSRs <- c(0.05)

# mating function beta values
betas <- as.numeric(OSRs_to_betas(OSRs))

# number of simulations to run
num_sims <- c(10000)

# models
model <- c('P_base', 'P_evol', 'P_evol_high_H', 
'GM_base', 'GM_evol', 'GM_evol_high_H')

# make dataframe of all combinations of arguments
DF <- expand.grid(scenarios, betas, num_sims, model)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(as.numeric(DF[i, ]))
  
}

########### do the runs ########################################################
mclapply(X = arguments, 
         FUN = run_base_model, 
         mc.cores = 50)

# mclapply(X = scenarios, 
#          FUN = run_base_model, 
#          mc.cores = mc, 
#          num_sims, 
#          betas)  

##### MS thesis version of cluster runs ########################################

# library(remotes)
# remotes::install_github('vquennessen/densityratio')
# library(densityratio)
# library(parallel)
# source('run_base_model.R')
# 
# # species_list_1 = c('BR_OR_2015', 'LING_OW_2017', 'CR_OR_2015')
# # species_list_2 = c('CAB_OR_2019')
# # Final_DRs_2 <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
# 
# species_list_1 <- c('LING_OW_2017', 'CR_OR_2015')
# species_list_2 = c('CAB_OR_2019')
# 
# Final_DRs_1 <- c(0.6, 0.7, 0.8, 0.9)
# Final_DRs_2 <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
# 
# NS1 <- 100
# mc <- 10
# 
#  Recruitment ######################################################
# 
# Scenario = 'Recruitment'
# 
# mclapply(species_list_1, run_base_model, mc.cores = mc, num_sims = NS1,
#          Scenario, Final_DRs_1)
# mclapply(species_list_2, run_base_model, mc.cores = mc, num_sims = NS1,
#          Scenario, Final_DRs_2)
# 
# Sampling #########################################################
# 
# Scenario = 'Sampling'
# 
# mclapply(species_list_1, run_base_model, mc.cores = mc, num_sims = NS1,
#          Scenario, Final_DRs_1)
# mclapply(species_list_2, run_base_model, mc.cores = mc, num_sims = NS1,
#          Scenario, Final_DRs_2)
# 
# Both ###############################################################
# 
# Scenario = 'Both'
# 
# mclapply(species_list_1, run_base_model, mc.cores = mc, num_sims = NS1,
#          Scenario, Final_DRs_1)
# mclapply(species_list_2, run_base_model, mc.cores = mc, num_sims = NS1,
#          Scenario, Final_DRs_2)
# 
# 
##### MS thesis version of run base model ######################################

# run_base_model <- function(Species, num_sims, Scenario, Final_DRs) {
