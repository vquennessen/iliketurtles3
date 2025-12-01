# test run base model for Will

rm(list = ls())

# set working directory to wherever you saved the codes
setwd('~/Projects/iliketurtles3/code')

# uncomment whichever you'd like to do
task <- 'test_evolution'
# task <- 'test_whole_thing'

# load libraries
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)
library(purrr)

# stricter sample function
resample <- function(x, ...) x[sample.int(length(x), ...)]

# source functions
source('base_model.R')
source('initialize_arrays.R')
source('reproduction.R')
source('pop_dynamics.R')
source('evolution.R')

# load in SAD object
load("../output/SAD_deterministic_TS_b800_medians.Rdata")

# initial total population size
init_total <- 20000

init_age_distribution <- SADdf %>%
  filter(!is.na(Prop_10yr_median)) %>%
  filter(Year == max(Year)) %>%
  mutate(Abundance = round(Prop_10yr_median * init_total)) %>%
  mutate(TRangeT = ifelse(Model == 'P_base', 'narrow', 'wide'))

# testing
TRT <- c('narrow')
evolve <- c(TRUE)
trait <- c('T_piv')
rate <- c('high')
scenario <- c(3.5)
beta <- 2.86

# conservation?
conservation_action <- c(FALSE)
intensity <- c(1)
frequency <- c(1)

# years to run the model for
yrs <- 100

# number of simulations to run
nsims <- c(2)

# white or red noise
noise <- 'white'  

folder <- paste('test_evolution', trait, rate, sep = '/')


###### model inputs ##########################################################

# model parameters to modulate
temp_mu <- 31.8                         # base incubation temp mean
climate_stochasticity <- TRUE           # whether or not to add in
season_temp_sd <- 0.364                 # variance in temp at season level
clutch_temp_sd <- 0.790                 # variance in temp at clutch level
AC <- 0.5                               # autocorrelation coefficient

# turtle demographics
max_age <- 85
F_survival_values <- c(0.35, 0.8, 0.8, 0.85, 0.799)
M_survival_values <- c(0.35, 0.8, 0.8, 0.85, 0.799)

F_years_in_stage <- c(1, 2, 7, 12, 63)# lifespan
M_years_in_stage <- c(1, 2, 7, 12, 63)# lifespan

# max_age - (1 + 2 + 7 + 12)              # years for last ageclass
IF_survival <- c(rep(F_survival_values[1], F_years_in_stage[1]),
                 rep(F_survival_values[2], F_years_in_stage[2]),
                 rep(F_survival_values[3], F_years_in_stage[3]),
                 rep(F_survival_values[4],
                     (F_years_in_stage[4] + F_years_in_stage[5])))
IM_survival <- c(rep(M_survival_values[1], M_years_in_stage[1]),
                 rep(M_survival_values[2], M_years_in_stage[2]),
                 rep(M_survival_values[3], M_years_in_stage[3]),
                 rep(M_survival_values[4],
                     (M_years_in_stage[4] + M_years_in_stage[5])))
MF_survival <- 0.799
MM_survival <- 0.799

age_maturity_mu <- 25                     # age at first reproduction, mean
age_maturity_sd <- 2.5                    # age at first reproduction, SD
M <- round(pnorm(q = 1:max_age,           # maturity ogive
                 mean = age_maturity_mu,
                 sd = age_maturity_sd),
           3)

F_remigration_int <- 3.87                 # remigration interval - females
M_remigration_int <- 1.82                 # remigration interval - males
clutches_mu <- 4.95                       # mean # of clutches/F/season
clutches_sd <- 2.09                       # sd of # of clutches/F/season
eggs_mu <- 100.58                         # mean number of eggs/clutch - 100.58
eggs_sd <- 22.61                          # sd of number of eggs/clutch - 22.61
emergence_success_A <- 0.86               # logistic by temp - A
emergence_success_k <- -1.7               # logistic by temp - beta
emergence_success_t0 <- 32.7              # logistic by temp - t0
T_threshold <- 35                         # lethal temperature threshold
k_piv <- ifelse(TRT == 'narrow',
                -1.54,
                -0.77)  
T_piv <- 29.4                             # thermal reaction norm midpoint

# stable age distributions to start with
IAD <- init_age_distribution %>%
  filter(Beta == beta) %>%
  filter(TRangeT == TRT)

IF_init <- IAD %>%
  filter(Sex == 'IF') %>%
  pull(Abundance)

IM_init <- IAD %>%
  filter(Sex == 'IM') %>%
  pull(Abundance)

MF_init <- IAD %>%
  filter(Sex == 'MF') %>%
  pull(Abundance)

MM_init <- IAD %>%
  filter(Sex == 'MM') %>%
  pull(Abundance)

if (evolve == TRUE) {
  
  # probabilities of females mating with 1-10 males
  male_probs <- c(0.188, 0.280, 0.236, 0.150, 0.080, 0.038, 0.017, 0.007, 0.003, 0.001)
  
  # male fertilization contributions
  raw_contributions <- 0.687 * (c(1:10))^(-1.710)
  
  contributions <- list(1)
  
  for (i in 2:length(raw_contributions)) {
    
    contributions[i] <- list(c(
      raw_contributions[1:i]/sum(raw_contributions[1:i])))
    
  }
  
  if (trait == 'T_piv') {
    
    value <- T_piv
    
    if (rate == 'effective') { 
      h2 <- 0.221
      varGenetic <- 0.926 } 
    
    if (rate == 'high') { 
      h2 <- 0.576
      varGenetic <- 2.41 }
    
    # or, if the evolvable trait is the emergence success midpoint (t0)
  } 
  
  if (trait == 'emergence_success_t0') {
    
    value <- emergence_success_t0
    
    if (rate == 'effective') { 
      h2 <- 0.75
      varGenetic <- 1.19 } 
    
    if (rate == 'high') { 
      h2 <- 0.88
      varGenetic <- 1.39 }
    
  }
  
  # phenotypic variance, error term for offspring phenotype, one for each year
  varPhenotypic <- varGenetic / h2  
  
  # genotype and phenotype summary stats, dimensions sex * age * year * # stats
  sims_G_stats <- array(rep(NA, times = 4 * max_age * yrs * 3 * nsims), 
                        dim = c(4, max_age, yrs, 3, nsims))
  
  sims_P_stats <- array(rep(NA, times = 4 * max_age * yrs * 3 * nsims), 
                        dim = c(4, max_age, yrs, 3, nsims))
  
} else {
  
  value                 <- NULL
  h2                    <- NULL
  varGenetic            <- NULL
  varPhenotypic         <- NULL
  G                     <- NULL
  P                     <- NULL
  male_probs            <- NULL
  contributions         <- NULL
  
}

if (task == 'test_evolution') {
  
  init_output <- initialize_arrays(scenario, yrs, max_age, 
                                   IF_init, IM_init, MF_init, MM_init,
                                   M, F_remigration_int, M_remigration_int, 
                                   T_piv, k_piv, T_threshold, 
                                   temp_mu, climate_stochasticity, 
                                   season_temp_sd, clutch_temp_sd, noise, AC, 
                                   evolve, trait, value, 
                                   varGenetic, varPhenotypic,
                                   conservation_action, frequency)
  
  N                  <- init_output[[1]]   # population size array
  season_temp_mus    <- init_output[[2]]   # mean temps at the season level
  OSRs               <- init_output[[3]]   # operational sex ratio
  G                  <- init_output[[4]]
  P                  <- init_output[[5]]
  G_stats            <- init_output[[6]]   # genotype stats, to keep
  P_stats            <- init_output[[7]]   # phenotype stats, to keep  
  conservation_years <- init_output[[8]]   # years for conservation action
  
  
  y = 2
  
  popdy_output <- pop_dynamics(N, max_age, y, M,
                               IF_survival, IM_survival, 
                               MF_survival, MM_survival, 
                               evolve, G, P, G_stats, P_stats)
  
  N       <- popdy_output[[1]]
  G       <- popdy_output[[2]]
  P       <- popdy_output[[3]]
  G_stats <- popdy_output[[4]]
  P_stats <- popdy_output[[5]]
  
  # breeding females this year
  n_available_F <- sum(rbinom(n = max_age, 
                              size = N[3, , y], 
                              prob = 1 / F_remigration_int), na.rm = TRUE)
  
  # breeding males this year
  n_available_M <- sum(rbinom(n = max_age, 
                              size = N[4, , y], 
                              prob = 1 / M_remigration_int), na.rm = TRUE)
  
  # operational sex ratio - proportion of males
  # multiply by 2 to transform to beta function with x from 0 to 0.5 instead 
  # of 0 to 1
  OSR <- n_available_M / (n_available_M + n_available_F)
  
  # calculate reproductive success
  # use beta function to calculate breeding success multiply OSR by 2 to 
  # transform to beta function with x from 0 to 0.5 instead of 0 to 1
  breeding_success <- pbeta(2 * OSR, 
                            shape1 = 1, 
                            shape2 = beta) 
  
  # how many females actually find a male to mate with and then nest
  # set.seed(seed)
  n_breeding_F <- sum(rbinom(n = n_available_F, 
                             size = 1, 
                             prob = breeding_success), na.rm = TRUE)
  
  # vector of number of clutches per female (round to nearest integer)
  # set.seed(seed)
  clutches <- round(rnorm(n = n_breeding_F, 
                          mean = clutches_mu, 
                          sd = clutches_sd)) 
  
  # replace any number < 1 with +1
  clutches[which(clutches < 1)] <- 1
  
  # eggs list, one number for each clutch
  # set.seed(seed)
  eggs <- lapply(clutches,
                 rnorm,
                 mean = eggs_mu,
                 sd = eggs_sd) %>%
    lapply(pmax, 0) %>%
    lapply(round)
  
  # list of clutch temperatures, one number for each clutch 
  # set.seed(seed)
  clutch_temps <- lapply(clutches, 
                         rnorm, 
                         mean = season_temp_mus[y], 
                         sd = clutch_temp_sd) %>%
    lapply(pmax, 0)
  
  ##### now you can go into the evolution function itself ########################
  
  # evolution <- function(max_age, G, P, 
  #                       n_breeding_F, n_available_M, 
  #                       trait, male_probs, contributions, 
  #                       h2, varGenetic, varPhenotypic,
  #                       clutches, eggs, clutch_temps, 
  #                       emergence_success_A, emergence_success_k, 
  #                       emergence_success_t0, 
  #                       T_threshold, k_piv, T_piv) {
  
  # extract maternal genotypes
  GM <- as.list(resample(unlist(G[3, 2:max_age]), size = n_breeding_F))
  
  # extract potential paternal genotypes
  potential_GP <- resample(unlist(G[4, 2:max_age]), size = n_available_M)       
  
  # how many males does each female mate with
  nMales <- as.list(resample(1:length(male_probs), 
                             size = n_breeding_F, 
                             prob = male_probs, 
                             replace = TRUE))
  
  # if there are more males assigned to a female than there are available, 
  # reduce it with the maximum number of males available
  nMales[nMales > n_available_M] <- n_available_M
  
  # assign male genotypes to each female
  GP <- map(nMales, ~ resample(potential_GP, size = .x))
  
  # initialize offspring genotypes and phenotypes
  G_females <- list()
  G_males <- list()
  P_females <- list()
  P_males <- list()
  
  
  for (i in 1:n_breeding_F) {
    
    # for each egg in each clutch, assign maternal genotypes to offspring
    GM_eggs <- lapply(eggs[[i]], function(x) rep(GM[[i]], times = x))
    
    # for each egg in each clutch, assign paternal genotypes to offspring
    GP_eggs <- lapply(eggs[[i]], 
                      function(x) {
                        resample(resample(GP[[i]]), 
                                 size = x, 
                                 prob = contributions[[nMales[[i]]]], 
                                 replace = TRUE)
                      }
    )
    
    # calculate offspring genotypes
    G_eggs <- lapply(Map('+', GM_eggs, GP_eggs), 
                     function(x) rnorm(n = length(x), 
                                       mean = x/2, 
                                       sd = sqrt(varGenetic / 2)))
    
    # calculate offspring phenotypes
    P_eggs <- lapply(G_eggs, 
                     function(x) rnorm(n = length(x), 
                                       mean = x, 
                                       sd = sqrt(varGenetic*(1 - h2)/h2))
    )
    
    if (trait == 'emergence_success_t0') {
      
      # list of probability of emergence, one for each egg 
      probs_emerged <- map2(clutch_temps[[i]], P_eggs, 
                            ~ emergence_success_A / (
                              1 + exp(-emergence_success_k * (.x - .y)))) %>%
        lapply(pmax, 0)
      
    } else {
      
      probs_emerged <- lapply(
        clutch_temps[[i]], 
        function(x) {
          if (x < T_threshold) {
            emergence_success_A / (
              1 + exp(-emergence_success_k * (x - emergence_success_t0)))
          } else { 0 }
        }
      ) %>% lapply(pmax, 0)
      
    }
    
    # which eggs emerge as hatchlings?
    indices_hatchlings <- map2(eggs[[i]], probs_emerged, 
                               ~ as.logical(
                                 rbinom(n = .x, size = 1, prob = .y)))
    
    # how many hatchlings are there?
    hatchlings <- unlist(lapply(indices_hatchlings, sum, na.rm = TRUE))
    
    # hatchling genotypes and phenotypes
    G_hatchlings <- map2(G_eggs, indices_hatchlings, ~ .x[as.logical(.y)])
    P_hatchlings <- map2(P_eggs, indices_hatchlings, ~ .x[as.logical(.y)])
    
    if (trait == 'T_piv') {
      
      # probability of developing as male, one for each egg
      probs_male <- map2(clutch_temps[[i]], 
                         P_hatchlings, 
                         ~ 1 / (1 + exp(-k_piv * (.x - .y)))) %>%
        lapply(pmax, 0)
      
    } else {
      
      # list of probability of developing as male, one for each clutch 
      probs_male <- lapply(clutch_temps[[i]], 
                           function(x) {
                             1 / (1 + exp(-k_piv * (x - T_piv)))
                           }) %>%
        lapply(pmax, 0)
      
    }
    
    # which hatchlings developed as male?
    indices_males <- map2(hatchlings, probs_male, 
                          ~ as.logical(rbinom(n = .x, size = 1, prob = .y)))
    indices_females <- map(indices_males, ~ as.logical(Map(`-`, 1, .x)))
    
    # genotypes of females and males
    G_females[[i]] <- map2(G_hatchlings, indices_females, ~ .x[as.logical(.y)])
    G_males[[i]] <- map2(G_hatchlings, indices_males, ~ .x[as.logical(.y)])
    
    # phenotypes of females and males
    P_females[[i]] <- map2(P_hatchlings, indices_females, ~ .x[as.logical(.y)])
    P_males[[i]] <- map2(P_hatchlings, indices_males, ~ .x[as.logical(.y)])
    
  }
  
}

if (task == 'test_whole_thing') {
  
  ##### conservation #############################################################
  
  # if conservation is TRUE
  effect_size <- ifelse(conservation_action == TRUE,
                        1.3,
                        NA)
  
  ##### initialize output ########################################################
  
  # initialize population size array by sex/maturity, age, years, sims
  sims_N <- array(rep(NA, times = 4 * max_age * yrs * nsims),
                  dim = c(4, max_age, yrs, nsims))
  
  sims_OSR <- array(rep(NA, times = yrs * nsims),
                    dim = c(yrs, nsims))
  
  ##### run sims and save output #################################################
  
  # run the model for each simulation
  for (i in 1:nsims) {
    
    output <- base_model(scenario, beta, yrs, max_age,
                         IF_survival, IM_survival, MF_survival, MM_survival,
                         IF_init, IM_init, MF_init, MM_init,
                         M, F_remigration_int, M_remigration_int,
                         clutches_mu, clutches_sd, eggs_mu, eggs_sd,
                         emergence_success_A, emergence_success_k,
                         emergence_success_t0, T_piv, k_piv, T_threshold,
                         evolve, trait, value, male_probs, contributions,
                         h2, varGenetic, varPhenotypic,
                         temp_mu, climate_stochasticity,
                         season_temp_sd, clutch_temp_sd, noise, AC,
                         conservation_action, frequency, intensity,
                         effect_size)
    
    # save the output arrays
    sims_N[, , , i]             <- output[[1]]
    sims_OSR[, i]               <- output[[2]]
    
    if (evolve == TRUE) {
      sims_G_stats[, , , , i]       <- output[[3]]
      sims_P_stats[, , , , i]       <- output[[4]]
    }
    
  }
  
  if (conservation_action == TRUE) {
    
    folder2 <- paste('/freq_', frequency, '_intensity_', intensity, sep = '')
    
  } else { folder2 <- ''}
  
  # get filepaths to save objects to
  filepath1 = paste('../output/', folder, '/',  TRT, '/', scenario, 'C/beta',
                    beta, '/', nsims, '_N.Rda', sep = '')
  filepath2 = paste('../output/', folder, '/',  TRT, '/', scenario, 'C/beta',
                    beta, '/', nsims, '_OSR.Rda', sep = '')
  
  # save objects
  save(sims_N, file = filepath1)
  save(sims_OSR, file = filepath2)
  
  if (evolve == TRUE) {
    
    filepath3 = paste('../output/', folder, '/',  TRT, '/', scenario, 'C/beta',
                      beta, '/', nsims, '_G_stats.Rda', sep = '')
    filepath4 = paste('../output/', folder, '/',  TRT, '/', scenario, 'C/beta',
                      beta, '/', nsims, '_P_stats.Rda', sep = '')
    
    save(sims_G_stats, file = filepath3)
    save(sims_P_stats, file = filepath4)
    
  }
  
}