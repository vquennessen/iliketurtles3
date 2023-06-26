### cluster runs

library(remotes)
remotes::install_github('vquennessen/iliketurtles3')
library(densityratio)
library(parallel)

# set working directory
setwd('~/Projects/iliketurtles3/code')

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population2.R')
source('reproduction.R')
source('pop_dynamics.R')

# species_list_1 = c('BR_OR_2015', 'LING_OW_2017', 'CR_OR_2015')
# species_list_2 = c('CAB_OR_2019')
# Final_DRs_2 <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

species_list_1 <- c('LING_OW_2017', 'CR_OR_2015')
species_list_2 = c('CAB_OR_2019')

Final_DRs_1 <- c(0.6, 0.7, 0.8, 0.9)
Final_DRs_2 <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

# number of simulations to run
num_sims <- 10000

# number of cores to run
mc <- 10

# scenarios <- c(3.5)  # total temp increases
scenarios <- c(1, 2, 3, 4, 5, 6, 7, 8)  # total temp increases

betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
# betas <- c(34.14)

########### do the runs ########################################################

for (s in 1:length(scenarios)) {
  
  for (b in 1:length(betas)) {
    
    mclapply(num_sims = num_sims, 
             run_base_model, 
             mc.cores = mc, 
             scenario = scenarios[s], 
             beta = betas[b])    
  }
  
}
