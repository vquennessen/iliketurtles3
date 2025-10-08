### cluster runs for testing SAD out now that it has stochasticity

# load libraries
library(parallel)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)
library(tictoc)

# source functions
source('initialize_population_test.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OSRs_to_betas.R')
source('emergence_success.R')
source('probability_male.R')
source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population.R')
source('mating function/OSRs_to_betas.R')
source('evolution.R')
source('conservation.R')

# models
models <- c('P_base', 'GM_base')

# beta values to get full fertilization of females
OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
betas <- as.numeric(OSRs_to_betas(OSRs))

# number of simulations to run
num_sims <- c(100, 1000)
# num_sims <- c(1, 2)

# years to run the model for
burn_ins <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
# burn_ins <- c(10, 20)

# make dataframe of all combinations of arguments
DF <- expand.grid(models,
                  betas,
                  num_sims,
                  burn_ins) %>%
  arrange(desc(Var4), desc(betas))

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(DF[i, ])
  
}

########### do the runs ########################################################

# cluster runs
mclapply(X = arguments,
         FUN = initialize_population_test,
         mc.cores = 20)

# # machine runs
# tic()
# lapply(X = arguments,
#        FUN = initialize_population_test)
# toc()

##### stitch them together #####################################################

for (bi in 1:length(burn_ins)) {
  
  for (ns in 1:length(num_sims)) {
    
    # initialize results dataframe
    SADdf <- data.frame(Model = NULL, 
                        Beta = NULL,
                        Sex = NULL,
                        Age = NULL,
                        Abundance = NULL, 
                        Proportion = NULL
    )
    
    for (b in 1:length(betas)) {
      
      P_base <- paste('../output/SAD_n', num_sims[ns], '_b', 
                      burn_ins[bi], '_P_base_beta', betas[b], '.Rdata', sep = '')
      
      load(P_base) 
      
      SAD_P <- SAD
      
      GM_base <- paste('../output/SAD_n', num_sims[ns], '_b', 
                       burn_ins[bi], '_GM_base_beta', betas[b], '.Rdata', sep = '')
      
      load(GM_base) 
      
      SAD_GM <- SAD
      
      SADdf <- rbind(SADdf, SAD_P, SAD_GM)
      
      file.remove(P_base)
      file.remove(GM_base)
      
    }
    
    save(SADdf, 
         file = paste('../output/SAD_n', num_sims[ns], 
                      '_b', burn_ins[bi], '.Rdata', sep = ''))    
    
  }
  
}
