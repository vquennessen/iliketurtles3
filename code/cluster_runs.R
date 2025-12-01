### cluster runs

rm(list = ls())

# load libraries
library(parallel)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)
library(purrr)

# stricter sample function
resample <- function(x, ...) x[sample.int(length(x), ...)]

# # set working directory
# setwd('~/Projects/iliketurtles3/code')

source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OSRs_to_betas.R')
source('evolution.R')
source('conservation.R')

# load in SAD object
load("../output/SAD_deterministic_TS_b800_medians.Rdata")

# initial total population size
init_total <- 20000

init_age_distribution <- SADdf %>%
  filter(!is.na(Prop_10yr_median)) %>%
  filter(Year == max(Year)) %>%
  mutate(Abundance = round(Prop_10yr_median * init_total)) %>%
  mutate(TRangeT = ifelse(Model == 'P_base', 'narrow', 'wide'))

save(init_age_distribution, 
     file = '../output/init_age_distribution.Rdata')

# # testing
TRT <- c('narrow')
# TRT <- c('narrow', 'wide')
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
# scenarios <- c(2.5)
OSRs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
# OSRs <- c(0.25)
betas <- as.numeric(OSRs_to_betas(OSRs))

# evolution
evolve <- c(FALSE)
trait <- c('T_piv')
rate <- c('high')

# conservation
conserve <- c(TRUE)
intensity <- c(0.1, 0.2, 0.3, 0.4, 0.5)
# intensity <- c(0.1, 0.2)
frequency <- c(1, 2, 3, 4, 5)
# frequency <- c(1, 2)

# years to run the model for
yrs <- 100

# number of simulations to run
nsims <- c(1000)

# white or red noise
noise <- 'white'

# make dataframe of all combinations of arguments
DF <- expand.grid(noise,
                  TRT, 
                  scenarios,
                  betas,
                  yrs,
                  nsims,
                  evolve,
                  trait, 
                  rate, 
                  conserve,
                  intensity,
                  frequency) %>%
  mutate(Var13 = paste(gsub('-', '_', Sys.Date()), 
                       ifelse(Var7 == TRUE, 
                              paste('_evolution', Var8, Var9, sep = '/'), 
                              ''), 
                       ifelse(Var10 == TRUE, 
                              paste('_conservation/i', Var11, '/F', Var12, sep = ''), 
                              ''), 
                       sep = '')) %>%
  arrange(Var5, Var3, desc(Var4))

# initialize empty arguments list
arguments <- list()

# fill in arguments list with dataframe rows
for (i in 1:nrow(DF)) {

  arguments[i] <- list(DF[i, ])

}

########### do the runs ########################################################

TIME1 <- lubridate::now()

result <- tryCatch({})
mclapply(X = arguments,
         FUN = run_base_model,
         mc.cores = 25)

# lapply(X = arguments,
#        FUN = run_base_model)

# TIME4 <- lubridate::now()
# 
# Total_time <- format(round(TIME4 - TIME1), 3)
# 
# if (evolve == TRUE) {
#   
#   final_update <- paste(time2.5, ' - evolution - ', trait, ' - ', rate, ' - ', 
#                    TRT, ' - ', scenario, 'C - beta ', beta, ' - ', nsims, 
#                    ' sims - ', yrs, ' years - ',  i/nsims*100, '% done!', 
#                    sep = '')
#   
# } else {
# 
# final_update <- paste('Models: ', TRT, '\n', 'Betas: ', betas, '\n', 
#                       nsims, ' sims \n ', yrs, ' years \n total time: ', 
#                       Total_time, '\n', sep = '')
# }
# 
# write(final_update, 
#       file = '../output/progress.txt', append = TRUE)

