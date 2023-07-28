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

# number of simulations to run
num_sims <- 300

# number of cores to run
mc <- 24

# scenarios <- c(3.5)  # total temp increases
scenarios <- c(1.5)  # total temp increases

betas <- c(8.31, 11.19, 16.94, 34.14)
# betas <- c(34.14)

########### do the runs ########################################################
mclapply(X = scenarios, 
         FUN = run_base_model, 
         mc.cores = mc, 
         num_sims, 
         betas)  

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
