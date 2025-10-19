### cluster runs

# load libraries
library(parallel)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)

# set working directory
# setwd('~/Projects/iliketurtles3/code')

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OSRs_to_betas.R')
source('evolution.R')
source('emergence_success.R')
source('probability_male.R')
source('conservation.R')

# load in SAD object
load("/home/quennessenv/iliketurtles3/output/SAD_n100_b400.Rdata")

init_age_distribution <- SADdf %>%
  filter(!is.na(Proportion)) %>%
  filter(!is.na(Abundance)) %>%
  group_by(Model, Beta, Sex, Age) %>%
  summarise(prop_median = median(Proportion, na.rm = TRUE),
            prop_Q25 = quantile(Proportion, probs = 0.25, na.rm = TRUE),
            prop_Q75 = quantile(Proportion, probs = 0.75, na.rm = TRUE),
            abundance_median = median(Abundance, na.rm = TRUE),
            abundance_Q25 = quantile(Abundance, probs = 0.25, na.rm = TRUE),
            abundance_Q75 = quantile(Abundance, probs = 0.75, na.rm = TRUE),
            adj_abun_median = median(Adjusted_Abundance, na.rm = TRUE),
            adj_abun_Q25 = quantile(Adjusted_Abundance, probs = 0.25,
                                    na.rm = TRUE),
            adj_abun_Q75 = quantile(Adjusted_Abundance, probs = 0.75,
                                    na.rm = TRUE),
            .groups = 'drop')

# models
models <- c('P_base', 'GM_base')
# models <- c('P_base', 'GM_base')
# models <- c('P_evol_piv', 'P_evol_piv_high_H',
#             'P_evol_threshold', 'P_evol_threshold_high_H',
#             'GM_evol_piv', 'GM_evol_piv_high_H',
#             'GM_evol_threshold', 'GM_evol_threshold_high_H')
# models <- c('P_conservation', 'GM_conservation')


# years to run the model for
years <- 100

# total temp increases
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
# scenarios <- c(0.5, 1, 1.5)

# OSR values to get full fertilization of females
OSRs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
# OSRs <- c(0.45)

# mating function beta values
betas <- as.numeric(OSRs_to_betas(OSRs))
# betas <- c(3.82, 3.82, 3.82, 5.02, 6.64, 9.01, 3.82, 5.02, 6.64, 3.82, 5.02, 
#            6.64, 9.01, 3.82, 5.02, 6.64, 9.01, 12.91, 5.02, 20.63, 43.7)

# how many clutches to do conservation action on
# intensity <- c(0.1, 0.2, 0.3, 0.4, 0.5)
intensity <- c(1)

# how often to do the conservation action (years)
# frequency <- c(1, 2, 3, 4, 5)
frequency <- c(1)

# number of simulations to run
nsims <- c(1000)

# make dataframe of all combinations of arguments
DF <- expand.grid(models,
                  scenarios,
                  betas,
                  years,
                  nsims,
                  intensity,
                  frequency) %>%
  arrange(Var2, desc(Var3))

# # dataframe with only specific combinations
# DF <- data.frame(Var1 = models, 
#                  Var2 = scenarios, 
#                  Var3 = betas, 
#                  Var4 = years, 
#                  Var5 = num_sims, 
#                  Var6 = intensity, 
#                  Var7 = frequency) %>%
#   arrange(Var2, Var3)

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {
  
  arguments[i] <- list(DF[i, ])
  
}

########### do the runs ########################################################

result <- tryCatch({})
mclapply(X = arguments, 
         FUN = run_base_model, 
         mc.cores = 30)
