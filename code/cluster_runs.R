### cluster runs

# TO DO:
# - average temperature during incubation period
# - stable age distribution of population for each mating function at current 
#          average temperature
# - initialize population with that for 50 - 80 males and 180 - 300 females

# load libraries
library(parallel)

# set working directory
# setwd('~/Projects/iliketurtles3/code')

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population2.R')
source('reproduction.R')
source('pop_dynamics.R')

# number of simulations to run
num_sims <- 10000

# number of cores to run
mc <- 16

# scenarios <- c(3.5)  # total temp increases
scenarios <- c(1.5, 3, 4.5, 6, 7.5, 9, 10.5, 12)  # total temp increases

betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
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
