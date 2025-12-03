### cluster runs for testing SAD out now that it has stochasticity

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
library(purrr)
library(slider)
library(abind)

# source functions
source('mating function/OSRs_to_betas.R')
source('initialize_population_test2.R')

# stricter sample function
resample <- function(x, ...) x[sample.int(length(x), ...)]

# date for output folder
date_to_use <- gsub('-', '_', Sys.Date())

# models
TRTs <- c('narrow', 'wide')

# beta values to get full fertilization of females
OSRs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
# OSRs <- c(0.1)
betas <- as.numeric(OSRs_to_betas(OSRs))

# evolution
evolve <- c(TRUE)
traits <- c('T_piv', 'emergence_success_t0')
rates <- c('effective', 'high')

# number of simulations to run
num_sims <- c(1)

# years to run the model for
burn_ins <- c(750)

# make dataframe of all combinations of arguments
DF <- expand.grid(TRTs,
                  betas,
                  evolve, 
                  traits, 
                  rates, 
                  num_sims,
                  burn_ins, 
                  date_to_use) %>%
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
         FUN = initialize_population_test2,
         mc.cores = 25)

# # machine runs
# tic()
# lapply(X = arguments,
#        FUN = initialize_population_test2)
# toc()

##### stitch them together #####################################################

for (bi in 1:length(burn_ins)) {
  
  for (ns in 1:length(num_sims)) {
    
    # initialize results dataframe
    SADdf <- data.frame(TRT = NULL, 
                      Beta = NULL,
                      Evolution = NULL,
                      Trait = NULL,
                      Rate = NULL,
                      Year = NULL,
                      Sex = NULL,
                      Age = NULL,
                      Abundance = NULL,
                      Proportion = NULL, 
                      PSR = NULL,
                      OSR = NULL, 
                      G_mean = NULL, 
                      P_mean = NULL
    )
    
    for (b in 1:length(betas)) {
      
      if (evolve == TRUE) {
        
        for (t in 1:length(TRTs)) {
          
          for (r in 1:length(rates)) {
            
            tpiv <- paste('../output/SAD/', date_to_use, '_evolution_T_piv_', 
                          rates[r], '_n', num_sims[ns], '_b', burn_ins[bi], '_', 
                          TRTs[t], '_beta', betas[b], '.Rdata', sep = '')
            
            load(tpiv)
            SAD_tpiv <- SAD            
            
            ESt0 <- paste('../output/SAD/', date_to_use, 
                          '_evolution_emergence_success_t0_', rates[r], '_n', 
                          num_sims[ns], '_b', burn_ins[bi], '_', TRTs[t], 
                          '_beta', betas[b], '.Rdata', sep = '')
            
            load(ESt0)
            SAD_ESt0 <- SAD
            
            SADdf <- rbind(SADdf, SAD_tpiv, SAD_ESt0)
            
            file.remove(tpiv)
            file.remove(ESt0)
            
          }
          
        } 
        
      } else {
        
        narrow <- paste('../output/SAD/', date_to_use, '_n', num_sims[ns], 
                        '_b', burn_ins[bi], '_narrow_beta', betas[b], '.Rdata', 
                        sep = '')
        
        load(narrow)
        SAD_narrow <- SAD
        
        wide <- paste('../output/SAD/', date_to_use, '_n', num_sims[ns], 
                      '_b', burn_ins[bi], '_wide_beta', betas[b], '.Rdata', 
                      sep = '')
        
        load(wide)
        SAD_wide <- SAD
        
        SADdf <- rbind(SADdf, SAD_narrow, SAD_wide)
        
        file.remove(SAD_narrow)
        file.remove(SAD_wide)
        
      }
      
    }
  
  }
  
}

if (evolve == TRUE) { f1 <- '_evolution' } else { f1 <- '' }
    
    save(SADdf, 
         file = paste('../output/SAD/', date_to_use, f1, '_n', num_sims[ns], 
                      '_b', burn_ins[bi], '.Rdata', sep = ''))   