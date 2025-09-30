### machine runs

# # set working directory
# setwd('~/Projects/iliketurtles3/code')

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
source('probability_male.R')
source('conservation.R')

# model parameters
models  <- c('P_base', 'GM_base')
betas   <- c(1.17, 2, 2.86, 3.82, 5.02, 6.64, 9.01, 12.91, 20.63, 43.7)
nsims   <- 1000
sexes   <- c('IF', 'IM', 'MF', 'MM')
burn_in <- 100

################################################################################

# indices
M <- length(models)
B <- length(betas)
S <- length(sexes)

# model parameters to modulate
temp_mu <- 31.8                         # base incubation temp mean
season_temp_sd <- 0.364                 # variance in temp at season level
clutch_temp_sd <- 0.790                 # variance in temp at clutch level

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
emergence_success_A <- 0.86               # logistic by temp - A
emergence_success_k <- -1.7               # logistic by temp - beta
emergence_success_t0 <- 32.7              # logistic by temp - t0
T_piv <- 29.2                             # thermal reaction norm midpoint
Pivotal_temps <- rep(T_piv, burn_in)
T_threshold <- 35                         # lethal temperature threshold
Threshold_temps <- rep(T_threshold, burn_in)

##### initialize output ######################################################

# initialize yield and biomass arrays

# initialize population size array by sex/maturity, age, years, sims
SAD_N <- array(rep(NA, times = 4 * max_age * nsims), 
               dim = c(4, max_age, nsims))

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

start_N <- N

##### actually do the thing ####################################################

# initialize results dataframe
SAD <- data.frame(Model = NULL, 
                  Beta = NULL,
                  Sex = NULL,
                  Age = NULL,
                  Abundance = NULL, 
                  Proportion = NULL
)

for (m in 1:length(models)) {
  
  model <- models[m]
  
  # thermal reaction norm values
  k_piv <- ifelse(model %in% c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
                               'P_evol_threshold', 'P_evol_threshold_high_H', 
                               'P_conservation'), 
                  -1.34, 
                  -0.561)
  
  for (b in 1:length(betas)) {
    
    beta <- betas[b]
    
    for (i in 1:nsims) {
      
      # initialize results dataframe
      sub_SAD <- data.frame(Model = rep(model, each = S * max_age), 
                            Beta = rep(beta, each = S * max_age),
                            Sex = rep(sexes, each = max_age),
                            Age = rep(1:max_age, times = S),
                            Abundance = NA, 
                            Proportion = NA, 
                            Adjusted_Abundance = NA
      )
      
      # start from N with only 1 year
      N <- start_N
      
      # variation around the seasonal temperature
        season_temp_mus <- rnorm(n = burn_in, 
                                 mean = temp_mu, 
                                 sd = season_temp_sd)    
        
      # move population forward in time burn_in years
      for (y in 2:burn_in) {      
        
        # population dynamics
        pop_output <- pop_dynamics(N, max_age, y, M,
                                   IF_survival, IM_survival, 
                                   MF_survival, MM_survival,
                                   F_remigration_int, M_remigration_int)
        
        N              <- pop_output[[1]]
        breeding_F     <- pop_output[[2]]
        breeding_M     <- pop_output[[3]]
      
        #   # immature females that survived
        #   all_immature_F <- rbinom(n = max_age - 1, 
        #                            size = round(N[1, 1:(max_age - 1), y - 1]), 
        #                            prob = IF_survival[1:(max_age - 1)])
        #   
        #   # immature females that matured
        #   new_mature_F <- rbinom(n = max_age - 1, 
        #                          size = all_immature_F, 
        #                          prob = M)
        #   
        #   # updated immature female population
        #   N[1, 2:max_age, y] <- as.numeric(all_immature_F) - as.numeric(new_mature_F)
        #   
        #   # mature females that survived
        #   mature_survived_F <- rbinom(n = max_age - 1, 
        #                               size = round(N[3, 1:(max_age - 1), y - 1]), 
        #                               prob = MF_survival)
        #   
        #   # updated mature female population
        #   N[3, 2:max_age, y] <- as.numeric(mature_survived_F) + as.numeric(new_mature_F)
        #   
        #   # immature males that survived
        #   all_immature_M <- rbinom(n = max_age - 1, 
        #                            size = round(N[2, 1:(max_age - 1), y - 1]), 
        #                            prob = IM_survival[1:(max_age - 1)])
        #   
        #   # immature males that matured
        #   new_mature_M <- rbinom(n = max_age - 1, 
        #                          size = all_immature_M, 
        #                          prob = M)
        #   
        #   # updated immature male population
        #   N[2, 2:max_age, y] <- as.numeric(all_immature_M) - as.numeric(new_mature_M)
        #   
        #   # mature males that survived
        #   mature_survived_M <- rbinom(n = max_age - 1, 
        #                               size = round(N[4, 1:(max_age - 1), y - 1]), 
        #                               prob = MM_survival)
        #   
        #   # updated mature male population
        #   N[4, 2:max_age, y] <- as.numeric(mature_survived_M) + as.numeric(new_mature_M)
        
        ##### reproduction
        
        rep_output <- reproduction(N, M, y, beta, max_age, breeding_F, breeding_M, 
                                   clutches_mu, clutches_sd, 
                                   eggs_mu, eggs_sd, emergence_success_A, 
                                   emergence_success_k, emergence_success_t0, 
                                   season_temp_mus, clutch_temp_sd,
                                   k_piv, Pivotal_temps, Threshold_temps, 
                                   conservation_action = FALSE, 
                                   conservation_years = 0, intensity = 0, 
                                   effect_size = 0)
        
        # add recruits to population size array
        N[1, 1, y]          <- rep_output[[1]]
        N[2, 1, y]          <- rep_output[[2]]
        # OSRs[y]             <- rep_output[[3]]
        
        # break out of loop if there are zero males or females at any age
        if (sum(N[1, , y], na.rm = TRUE) < 1 || 
            sum(N[2, , y], na.rm = TRUE) < 1) { break }
        
      }
      
      # # raw male and female counts
      # IF_raw <- as.data.frame(N[1, , ])
      # IM_raw <- as.data.frame(N[2, , ])
      # MF_raw <- as.data.frame(N[3, , ])
      # MM_raw <- as.data.frame(N[4, , ])
      
      # final abundances
      f_IF_raw <- N[1, , burn_in]
      f_IM_raw <- N[2, , burn_in]
      f_MF_raw <- N[3, , burn_in]
      f_MM_raw <- N[4, , burn_in]
      
      # add final abundances by sex
      sub_SAD$Abundance <- c(f_IF_raw, f_IM_raw, f_MF_raw, f_MM_raw)
      
      # final total abundance across all sexes
      # total_raw <- sum(f_IF_raw) + sum(f_IM_raw) + sum(f_MF_raw) + sum(f_MM_raw) 
      total_raw <- sum(f_IF_raw, f_IM_raw, f_MF_raw, f_MM_raw) 
      
      # add proportions to subset
      sub_SAD$Proportion <- sub_SAD$Abundance / total_raw    
      
      # final abundances by sex
      IF_prop <- f_IF_raw / total_raw
      IM_prop <- f_IM_raw / total_raw
      MF_prop <- f_MF_raw / total_raw
      MM_prop <- f_MM_raw / total_raw
      
      # # calculate proportions
      # IF <- IF_raw %>%
      #   mutate(Age = 1:max_age) %>%
      #   pivot_longer(cols = 1:burn_in,
      #                names_to = "Year", 
      #                values_to = "Abundance") %>%
      #   mutate(Year = parse_number(Year)) %>%
      #   rename(Female_Immature = Abundance) %>%
      #   group_by(Year)
      # 
      # IM <- IM_raw %>%
      #   mutate(Age = 1:max_age) %>%
      #   pivot_longer(cols = 1:burn_in,
      #                names_to = "Year", 
      #                values_to = "Abundance") %>%
      #   mutate(Year = parse_number(Year)) %>%
      #   rename(Male_Immature = Abundance) %>%
      #   group_by(Year)
      # 
      # MF <- MF_raw %>%
      #   mutate(Age = 1:max_age) %>%
      #   pivot_longer(cols = 1:burn_in,
      #                names_to = "Year", 
      #                values_to = "Abundance") %>%
      #   mutate(Year = parse_number(Year)) %>%
      #   rename(Female_Mature = Abundance) %>%
      #   group_by(Year)
      # 
      # MM <- MM_raw %>%
      #   mutate(Age = 1:max_age) %>%
      #   pivot_longer(cols = 1:burn_in,
      #                names_to = "Year", 
      #                values_to = "Abundance") %>%
      #   mutate(Year = parse_number(Year)) %>%
      #   rename(Male_Mature = Abundance) %>%
      #   group_by(Year)
      # 
      # females <- left_join(IF, MF)
      # males <- left_join(IM, MM)
      # 
      # abundances <- left_join(females, males) %>%
      #   mutate(Total = Female_Immature + Male_Immature + Female_Mature + Male_Mature) %>% 
      #   mutate(Age = factor(Age)) %>%
      #   group_by(Year) %>%
      #   mutate(Prop_Female_Immature = Female_Immature / sum(Total)) %>%
      #   mutate(Prop_Male_Immature = Male_Immature / sum(Total)) %>%
      #   mutate(Prop_Female_Mature = Female_Mature / sum(Total)) %>%
      #   mutate(Prop_Male_Mature = Male_Mature / sum(Total))
      # 
      # # save(abundances, file = '../output/initial_population.Rda')
      # 
      # # final SAD for total population (both sexes)
      # final_total_SAD <- abundances %>%
      #   filter(Year == burn_in) %>%
      #   .$Total
      # 
      # # sex proportions of total SAD
      # Final_Prop_Female_Immature <- abundances %>%
      #   filter(Year == burn_in) %>%
      #   .$Prop_Female_Immature
      # 
      # Final_Prop_Male_Immature <- abundances %>%
      #   filter(Year == burn_in) %>%
      #   .$Prop_Male_Immature 
      # 
      # Final_Prop_Female_Mature <- abundances %>%
      #   filter(Year == burn_in) %>%
      #   .$Prop_Female_Mature 
      # 
      # Final_Prop_Male_Mature <- abundances %>%
      #   filter(Year == burn_in) %>%
      #   .$Prop_Male_Mature
      # 
      # # multipliers to get minimum adults to match data
      # F_Mature_multiplier <- F_initial / sum(MF_raw[, burn_in])
      # M_Mature_multiplier <- M_initial / sum(MM_raw[, burn_in])
      
      F_Mature_multiplier <- F_initial / sum(f_MF_raw)
      M_Mature_multiplier <- M_initial / sum(f_MM_raw)    
      
      # which multiplier to use
      multiplier <- max(F_Mature_multiplier, M_Mature_multiplier)
      
      # # initial population size
      # IF_init <- round(Final_Prop_Female_Immature * multiplier)
      # IM_init <- round(Final_Prop_Male_Immature * multiplier)
      # MF_init <- round(Final_Prop_Female_Mature * multiplier)
      # MM_init <- round(Final_Prop_Male_Mature * multiplier)
      
      # initial population size
      IF_init <- round(f_IF_raw * multiplier)
      IM_init <- round(f_IM_raw * multiplier)
      MF_init <- round(f_MF_raw * multiplier)
      MM_init <- round(f_MM_raw * multiplier)
      
      # add final initial abundances by sex
      sub_SAD$Adjusted_Abundance <- c(IF_init, IM_init, MF_init, MM_init)
      
      # add to big dataframe 
      SAD <- rbind(SAD, sub_SAD)
      
    }
    
  }
  
}

# save object
save(SAD, 
     file = paste('../output/SAD_n', nsims, '_b', burn_in, '.Rdata', sep = ''))

