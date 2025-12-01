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
source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('mating function/OSRs_to_betas.R')
source('evolution.R')

# models
TRTs <- c('narrow', 'wide')

# beta values to get full fertilization of females
OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
betas <- as.numeric(OSRs_to_betas(OSRs))

# evolution
evolution <- c(TRUE)
trait <- c('T_piv', 'emergence_success_t0')
rate <- c('effective', 'high')

# number of simulations to run
num_sims <- c(1)

# years to run the model for
burn_ins <- c(500)

# make dataframe of all combinations of arguments
DF <- expand.grid(TRTs,
                  betas,
                  evolution, 
                  trait, 
                  rate, 
                  num_sims,
                  burn_ins) %>%
  arrange(Var3, desc(Var2))

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
         mc.cores = 50)

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
