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

# scenarios <- c(3.5)  # total temp increases
scenarios <- c(1.5)  # total temp increases

betas <- c(34.14)
# betas <- c(34.14)

# number of simulations to run
num_sims <- c(10, 13, 20, 30, 40, 50, 60, 70)

#make dataframe of all combinations of arguments
DF <- expand.grid(scenarios, betas, num_sims)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(as.numeric(DF[i, ]))
  
}

########### do the runs ########################################################
mclapply(X = arguments, 
         FUN = run_base_model, 
         mc.cores = 24)

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
