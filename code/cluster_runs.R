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
setwd('~/Projects/iliketurtles3/code')

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OSRs_to_betas.R')
source('evolution.R')
source('emergence_success.R')
source('probability_male.R')
source('conservation.R')

# load in SAD object
load("../output/SAD_deterministic_TS_b800_medians.Rdata")

# folder to save results to
folder <- c('2025_11_24_new_evolution/')

# initial total population size
init_total <- 20000

init_age_distribution <- SADdf %>%
  filter(!is.na(Prop_10yr_median)) %>%
  filter(Year == max(Year)) %>%
  mutate(Abundance = round(Prop_10yr_median * init_total)) %>%
  mutate(TRangeT = ifelse(Model == 'P_base', 'narrow', 'wide'))

save(init_age_distribution, 
     file = '../output/init_age_distribution.Rdata')

# models
TRT <- c('narrow')
# TRT <- c('narrow', 'wide')

# evolution
evolve <- c(TRUE)
trait <- c('T_piv')
# trait <- c('T_piv', 'emergence_success_t0')
rate <- c('effective')
# rate <- c('effective', 'high')

# conservation?
conservation_action <- c(FALSE)

# years to run the model for
yrs <- 100

# total temp increases
# scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
scenarios <- c(0.5, 0.35)

# OSR values to get full fertilization of females
# OSRs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
OSRs <- c(0.45, 0.25)

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
nsims <- c(2)

# maximum population size for any sex, age, year
max_N <- 20000

# white or red noise
noise <- 'White'

# make dataframe of all combinations of arguments
DF <- expand.grid(folder, 
                  noise,
                  TRT, 
                  scenarios,
                  betas,
                  yrs,
                  nsims,
                  max_N,
                  evolve,
                  trait, 
                  rate, 
                  conservation_action,
                  intensity,
                  frequency) %>%
  arrange(Var4, desc(Var5))

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {

  arguments[i] <- list(DF[i, ])

}

########### do the runs ########################################################

begin <- lubridate::now()

# result <- tryCatch({})
# mclapply(X = arguments,
#          FUN = run_base_model,
#          mc.cores = 20)

lapply(X = arguments,
         FUN = run_base_model)

finish <- lubridate::now()

Total_time <- format(round(finish - begin), 3)

final_update <- paste('Models: ', models, '\n', 'Betas: ', betas, '\n', 
                      nsims, ' sims \n ', yrs, ' years \n total time: ', 
                      Total_time, '\n', sep = '')

write(final_update, 
      file = '../output/progress.txt', append = TRUE)

