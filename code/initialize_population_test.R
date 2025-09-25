### machine runs

# set working directory
setwd('~/Projects/iliketurtles3/code')

# load libraries
library(parallel)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)

# source functions
source('run_base_model.R')
source('base_model.R')
source('initialize_arrays.R')
source('initialize_population.R')
source('reproduction.R')
source('pop_dynamics.R')
source('mating function/OSRs_to_betas.R')
source('evolution.R')
source('emergence_success.R')
source('proportion_male.R')
source('conservation.R')

# troubleshooting
model     <- 'P_base'
scenario  <- 0.5
beta      <- 9.01
years     <- 100
nsims     <- 5
intensity <- 1
frequency <- 1

# model parameters to modulate
temp_mu <- 31.8                         # base incubation temp mean
climate_stochasticity <- TRUE           # whether or not to add in
season_temp_sd <- 0.364                 # variance in temp at season level
clutch_temp_sd <- 0.790                 # variance in temp at clutch level
noise <- 'Red'                          # noise: White or Red
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
F_remigration_int <- 3.87                 # remigration interval - females
M_remigration_int <- 1.47                 # remigration interval - males
clutches_mu <- 4.95                       # mean # of clutches/F/season
clutches_sd <- 2.09                       # sd of # of clutches/F/season
eggs_mu <- 100.58                         # mean number of eggs/clutch - 100.58
eggs_sd <- 22.61                          # sd of number of eggs/clutch - 22.61
emergence_success_A <- 0.86                   # logistic by temp - A
emergence_success_k <- -1.7                   # logistic by temp - beta
emergence_success_t0 <- 32.7                  # logistic by temp - t0
T_piv <- 29.2                             # thermal reaction norm midpoint
T_threshold <- 35                         # lethal temperature threshold

# pivotal temperature and evolution stats
k_piv <- ifelse(model %in% c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
                             'P_evol_threshold', 'P_evol_threshold_high_H', 
                             'P_conservation'), 
                -1.34, 
                -0.561)

evolution_piv <- ifelse(model %in% c('P_evol_piv', 'P_evol_piv_high_H', 
                                     'GM_evol_piv', 'GM_evol_piv_high_H'), 
                        TRUE, 
                        FALSE)

h2_piv <- ifelse(model %in% c('P_evol_piv', 'GM_evol_piv'), 
                 0.135, 
                 ifelse(model %in% c('P_evol_piv_high_H', 
                                     'GM_evol_piv_high_H'), 
                        0.351, 
                        NA))

ag_var_piv <- ifelse(evolution_piv == TRUE, 
                     0.017, 
                     NA)

# threshold evolution stats
evolution_threshold <- ifelse(model %in% c('P_evol_threshold', 
                                           'P_evol_threshold_high_H', 
                                           'GM_evol_threshold', 
                                           'GM_evol_threshold_high_H'), 
                              TRUE, 
                              FALSE)

h2_threshold <- ifelse(model %in% c('P_evol_threshold', 'GM_evol_threshold'), 
                       0.2, ifelse(model %in% c('P_evol_threshold_high_H', 
                                                'GM_evol_threshold_high_H'), 
                                   0.38, 
                                   NA))

ag_var_threshold <- ifelse(evolution_threshold == TRUE, 
                           0.017, 
                           NA)

# conservation values
conservation_action <- ifelse(model %in% c('P_conservation', 
                                           'GM_conservation'), 
                              TRUE, 
                              FALSE)

effect_size <- ifelse(conservation_action == TRUE, 
                      1.3, 
                      NA)

##### initialize output ######################################################

# initialize yield and biomass arrays

# initialize population size array by sex/maturity, age, years, sims
sims_N <- array(rep(NA, times = 4 * max_age * years * nsims), 
                dim = c(4, max_age, years, nsims))

sims_OSR <- array(rep(NA, times = years * nsims), 
                  dim = c(years, nsims)) 

sims_piv <- array(rep(NA, times = years * nsims), 
                  dim = c(years, nsims))

sims_threshold <- array(rep(NA, times = years * nsims), 
                        dim = c(years, nsims))

##### initialize population ##################################################

# initial numbers of breeding adults by sex to find starting population size
# based on the stable age distribution
F_initial <- 170                          # initial adult F
M_initial <- 30                           # initial adult M

##### maturity ogive
M <- round(pnorm(q = 1:max_age, 
                 mean = age_maturity_mu, 
                 sd = age_maturity_sd), 
           3)
  
  # initialize N dataframe
  N <- array(rep(0, times = 4 * max_age * burn_in), 
             dim = c(4, max_age, burn_in))
  
  # immature ogive
  Mi <- 1 - M
  
  # initial pop size
  # hatchlings - immature populations
  N[1:2, 1, 1] <- 10000
  
  # everyone else - immature populations
  N[1, 2:max_age, 1] <- round(100 * Mi[2:max_age])
  N[2, 2:max_age, 1] <- round(100 * Mi[2:max_age])
  
  # everyone else - mature populations
  N[3, 2:max_age, 1] <- round(100 * M[2:max_age])
  N[4, 2:max_age, 1] <- round(100 * M[2:max_age])
  
  # move population forward in time burn_in years
  for (y in 2:burn_in) {
    
    # immature females that survived
    all_immature_F <- rbinom(n = max_age - 1, 
                             size = round(N[1, 1:(max_age - 1), y - 1]), 
                             prob = IF_survival[1:(max_age - 1)])
    
    # immature females that matured
    new_mature_F <- rbinom(n = max_age - 1, 
                           size = all_immature_F, 
                           prob = M)
    
    # updated immature female population
    N[1, 2:max_age, y] <- as.numeric(all_immature_F) - as.numeric(new_mature_F)
    
    # mature females that survived
    mature_survived_F <- rbinom(n = max_age - 1, 
                                size = round(N[3, 1:(max_age - 1), y - 1]), 
                                prob = MF_survival)
    
    # updated mature female population
    N[3, 2:max_age, y] <- as.numeric(mature_survived_F) + as.numeric(new_mature_F)
    
    # immature males that survived
    all_immature_M <- rbinom(n = max_age - 1, 
                             size = round(N[2, 1:(max_age - 1), y - 1]), 
                             prob = IM_survival[1:(max_age - 1)])
    
    # immature males that matured
    new_mature_M <- rbinom(n = max_age - 1, 
                           size = all_immature_M, 
                           prob = M)
    
    # updated immature male population
    N[2, 2:max_age, y] <- as.numeric(all_immature_M) - as.numeric(new_mature_M)
    
    # mature males that survived
    mature_survived_M <- rbinom(n = max_age - 1, 
                                size = round(N[4, 1:(max_age - 1), y - 1]), 
                                prob = MM_survival)
    
    # updated mature male population
    N[4, 2:max_age, y] <- as.numeric(mature_survived_M) + as.numeric(new_mature_M)
    
    ##### reproduction
    
    # breeding females this year
    available_F <- rbinom(n = max_age, 
                         size = N[3, , y], 
                         prob = 1 / F_remigration_int)
    
    n_available_F <- sum(as.numeric(available_F, na.rm = TRUE))
    
    # breeding males this year
    available_M <- rbinom(n = max_age, 
                         size = N[4, , y], 
                         prob = 1 / M_remigration_int)
    
    n_available_M <- sum(as.numeric(available_M, na.rm = TRUE))
    
    # as long as there is at least one mature female and one mature male:
    if (n_available_F > 0.5 & n_available_M > 0.5) {
      
      # operational sex ratio - proportion of males
      OSR <- n_available_M / (n_available_M + n_available_F)
      
      # calculate reproductive success
      # if 50% males or fewer, use beta function to calculate breeding success
      if (OSR <= 0.5) {
        breeding_success <- pbeta(2 * OSR, shape1 = 1, shape2 = beta) 
        
        # else, if there are more than 50% males, all the females get to mate
      } else { breeding_success <- 1 }
      
      # how many females actually find a male to mate with and then nest
      n_breeding_F <- round(n_available_F * breeding_success)
      
      # number of potential eggs, assuming full reproductive success
      eggs <- sum(n_breeding_F * round(clutches_mu) * round(eggs_mu))
      
      # emergence success
      emergence_success <- emergence_success_A / 
        (1 + exp(-emergence_success_k * (temp_mu - emergence_success_t0)))
      
      # total hatchlings = breeding_success * potential eggs * emergence success
      hatchlings <- round(eggs * emergence_success)
      
      # determine proportion of male hatchlings based on temperature
      prop_male <- 1 / (1 + exp(-k_piv * (temp_mu - T_piv)))
      
      # number of male and female hatchlings
      # female hatchlings
      N[1, 1, y] <- round(hatchlings * (1 - prop_male))
      # male hatchlings
      N[2, 1, y] <- round(hatchlings * prop_male)
      
    } else { 
      
      N[1, 1, y] <- 0
      N[2, 1, y] <- 0 
      
    }
    
  }
  
  # raw male and female counts
  females_immature_raw <- as.data.frame(N[1, , ])
  males_immature_raw <- as.data.frame(N[2, , ])
  females_mature_raw <- as.data.frame(N[3, , ])
  males_mature_raw <- as.data.frame(N[4, , ])
  
  females_immature <- females_immature_raw %>%
    mutate(Age = 1:max_age) %>%
    pivot_longer(cols = 1:burn_in,
                 names_to = "Year", 
                 values_to = "Abundance") %>%
    mutate(Year = parse_number(Year)) %>%
    rename(Female_Immature = Abundance) %>%
    group_by(Year)
  
  males_immature <- males_immature_raw %>%
    mutate(Age = 1:max_age) %>%
    pivot_longer(cols = 1:burn_in,
                 names_to = "Year", 
                 values_to = "Abundance") %>%
    mutate(Year = parse_number(Year)) %>%
    rename(Male_Immature = Abundance) %>%
    group_by(Year)
  
  females_mature <- females_mature_raw %>%
    mutate(Age = 1:max_age) %>%
    pivot_longer(cols = 1:burn_in,
                 names_to = "Year", 
                 values_to = "Abundance") %>%
    mutate(Year = parse_number(Year)) %>%
    rename(Female_Mature = Abundance) %>%
    group_by(Year)
  
  males_mature <- males_mature_raw %>%
    mutate(Age = 1:max_age) %>%
    pivot_longer(cols = 1:burn_in,
                 names_to = "Year", 
                 values_to = "Abundance") %>%
    mutate(Year = parse_number(Year)) %>%
    rename(Male_Mature = Abundance) %>%
    group_by(Year)
  
  females <- left_join(females_immature, females_mature)
  males <- left_join(males_immature, males_mature)
  
  abundances <- left_join(females, males) %>%
    mutate(Total = Female_Immature + Male_Immature + Female_Mature + Male_Mature) %>% 
    mutate(Age = factor(Age)) %>%
    group_by(Year) %>%
    mutate(Prop_Female_Immature = Female_Immature / sum(Total)) %>%
    mutate(Prop_Male_Immature = Male_Immature / sum(Total)) %>%
    mutate(Prop_Female_Mature = Female_Mature / sum(Total)) %>%
    mutate(Prop_Male_Mature = Male_Mature / sum(Total))

  # save(abundances, file = '../output/initial_population.Rda')
  
  # final SAD for total population (both sexes)
  final_total_SAD <- abundances %>%
    filter(Year == burn_in) %>%
    .$Total
  
  # sex proportions of total SAD
  Final_Prop_Female_Immature <- abundances %>%
    filter(Year == burn_in) %>%
    .$Prop_Female_Immature
  
  Final_Prop_Male_Immature <- abundances %>%
    filter(Year == burn_in) %>%
    .$Prop_Male_Immature 
  
  Final_Prop_Female_Mature <- abundances %>%
    filter(Year == burn_in) %>%
    .$Prop_Female_Mature 
  
  Final_Prop_Male_Mature <- abundances %>%
    filter(Year == burn_in) %>%
    .$Prop_Male_Mature
  
  # multipliers to get minimum adults to match data
  F_Mature_multiplier <- F_initial / sum(Final_Prop_Female_Mature * M)
  M_Mature_multiplier <- M_initial / sum(Final_Prop_Male_Mature * M)
  
  # which multiplier to use
  multiplier <- max(F_Mature_multiplier, M_Mature_multiplier)
  
  # initial population size
  IF_init <- round(Final_Prop_Female_Immature * multiplier)
  IM_init <- round(Final_Prop_Male_Immature * multiplier)
  MF_init <- round(Final_Prop_Female_Mature * multiplier)
  MM_init <- round(Final_Prop_Male_Mature * multiplier)
  
  IF_init
  IM_init
  MF_init
  MM_init
