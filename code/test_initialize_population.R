# testing initialize population script

# load libraries
library(magrittr)
library(tidyr)

# scenario parameters
model <- 'P_base'
scenario <- '0.5C'
beta <- 20.63
burn_in <- 100

# model parameters to modulate
temp_mu <- 31.80                        # base incubation temp mean
climate_stochasticity <- TRUE           # whether or not to add in
noise <- 'White'                          # noise: White or Red
temp_sd <- 0.84                         # base incubation temp sd
AC <- 0.5                               # autocorrelation coefficient

# turtle demographics
max_age <- 85                                         # lifespan
F_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - F
F_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - F
M_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - M
M_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - M
age_maturity_mu <- 25                     # age at first reproduction, mean
age_maturity_sd <- 2.5                    # age at first reproduction, SD
F_remigration_int <- 3.87                 # remigration interval - females
M_remigration_int <- 1.47                 # remigration interval - males
nests_mu <- 4.95                          # mean # of nests/F/season
nests_sd <- 2.09                          # sd of # of nests/F/season
eggs_mu <- 100.58                         # mean number of eggs/nest - 100.58
eggs_sd <- 22.61                          # sd of number of eggs/nest - 22.61
hatch_success_A <- 0.86                   # logistic by temp - A
hatch_success_k <- -1.7                   # logistic by temp - beta
hatch_success_t0 <- 32.7                  # logistic by temp - t0
T_piv <- 29.2                             # thermal reaction norm midpoint
ag_var_piv <- 0.017                       # phenotypic variance - pivotal temp
T_threshold <- 35                         # lethal temperature threshold
ag_var_threshold <- 0.017                 # phenotypic variance - threshold
k_piv <- -1.4
h2_piv <- 0.135
h2_threshold <- 0.20
evolution_piv <- FALSE 
evolution_threshold <- FALSE
conservation <- FALSE


F_initial <- 170                          # initial adult F
M_initial <- 30                           # initial adult M

##### maturity ogive
M <- pnorm(q = 1:max_age, mean = age_maturity_mu, sd = age_maturity_sd)

##### initial population size

# survival values vector - females
F_survival <- rep(F_survival_values, times = F_survival_years)

# survival values vector - males
M_survival <- rep(M_survival_values, times = M_survival_years)

# check it's long enough, and if not, add the last survival_value until it is
# females
if (length(F_survival) < max_age) {
  F_survival <- c(F_survival, rep(F_survival_values[length(F_survival_values)], 
                                  max_age - length(F_survival)))
}

# males
if (length(M_survival) < max_age) {
  M_survival <- c(M_survival, 
                  rep(M_survival_values[length(M_survival_values)], 
                      max_age - length(M_survival)))
}

################################################################################

# initialize N dataframe
N <- array(rep(0, times = 2 * max_age * burn_in), 
           dim = c(2, max_age, burn_in))

# initial pop size
# hatchlings
N[ , 1, 1] <- 1000
# not hatchlings
N[ , 2:max_age, 1] <- 10

# move population forward in time burn_in years
for (y in 2:burn_in) {
  
  # for each age 
  for (a in 2:max_age) {
    
    # make sure there are more than 0 individuals in the previous age class
    if (N[1, a - 1, y - 1] > 0.5) {
      
      # annual survival - females
      N[1, a, y] <- round(sum(rbinom(n = round(N[1, a - 1, y - 1]),
                                     size = 1, 
                                     prob = F_survival[a - 1]), 
                              na.rm = TRUE))
      
      # annual survival - males
      N[2, a, y] <- round(sum(rbinom(n = round(N[2, a - 1, y - 1]),    
                                     size = 1, 
                                     prob = M_survival[a - 1]), 
                              na.rm = TRUE))
      
    }
    
  }
  
  ##### reproduction
  
  # females only breed every F_remigration_int years
  n_breeding_F <- round(sum(round(N[1, , y] * M), na.rm = TRUE) / F_remigration_int)
  
  # males only breed every M_remigration_int years
  n_breeding_M <- round(sum(round(N[2, , y] * M), na.rm = TRUE) / M_remigration_int)
  
  # as long as there is at least one mature female and one mature male:
  if (n_breeding_F > 0.5 & n_breeding_M > 0.5) {
    
    # operational sex ratio - proportion of males
    OSR <- n_breeding_M / (n_breeding_M + n_breeding_F)
    
    # calculate reproductive success
    # if 50% males or fewer, use beta function to calculate breeding success
    if (OSR <= 0.5) {
      breeding_success <- pbeta(2 * OSR, shape1 = 1, shape2 = beta) 
      
      # else, if there are more than 50% males, all the females get to mate
    } else { breeding_success <- 1 }
    
    # number of eggs
    eggs <- sum(n_breeding_F * round(nests_mu) * round(eggs_mu))
    
    # hatching success
    hatch_success <- hatch_success_A / (1 + exp(-hatch_success_k * (temp_mu - hatch_success_t0)))
    
    # total hatchlings = breeding_success * total eggs * hatching success
    hatchlings <- round(breeding_success * eggs * hatch_success)
    
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
  
  print(y)
  
}

# plot results - females
females_raw <- N[1, , ]
View(females_raw)

females <- data.frame(N[1, , ]) %>%
  pivot_longer(cols = 1:(dim(N)[3]),
               names_to = "Year", 
               values_to = "Abundance")
View(females)

females <- data.frame(Age = rep(1:max_age, times = burn_in), 
                      Year = rep(1:burn_ing), 
                      Value = pivot_longer(N[1, , ], 
                                           names_to = "Year", 
                                           values_to = "Value"))

ggplot(data.frame(Age = 1:max_age))



# normalize
SAD_F <- N[1, , burn_in] / sum(N[1, , burn_in])
SAD_M <- N[2, , burn_in] / sum(N[2, , burn_in])


