# testing initialize population script

# load libraries
library(magrittr)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

# scenario parameters
model <- 'P_base'
scenario <- '0.5C'
beta <- 2.86
burn_in <- 5000

# model parameters to modulate
temp_mu <- 31.80                        # base incubation temp mean
temp_sd <- 0.84

# turtle demographics
max_age <- 85                                         # lifespan
F_survival_years <- c(1, 2, 7, 12, (max_age - (1 + 2 + 7 + 12)))                 # years per stage - F
F_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - F
M_survival_years <- c(1, 2, 7, 12, (max_age - (1 + 2 + 7 + 12)))                 # years per stage - M
M_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - M
age_maturity_mu <- 25                     # age at first reproduction, mean
age_maturity_sd <- 2.5                    # age at first reproduction, SD
F_remigration_int <- 3.87                 # remigration interval - females
M_remigration_int <- 1.47                 # remigration interval - males
nests_mu <- 4.95                          # mean # of nests/F/season
eggs_mu <- 100.58                         # mean number of eggs/nest - 100.58
hatch_success_A <- 0.86                   # logistic by temp - A
hatch_success_k <- -1.7                   # logistic by temp - beta
hatch_success_t0 <- 32.7                  # logistic by temp - t0
T_piv <- 29.2                             # thermal reaction norm midpoint

F_initial <- 170                          # initial adult F
M_initial <- 30                           # initial adult M

if (model == 'P_base') { k_piv <- -1.4 }
if (model == 'GM_base') { k_piv <- -0.561 }

##### maturity ogive
M <- pnorm(q = 1:max_age, mean = age_maturity_mu, sd = age_maturity_sd)

##### initial population size

# survival values vector - females
F_survival <- rep(F_survival_values, times = F_survival_years)

# survival values vector - males
M_survival <- rep(M_survival_values, times = M_survival_years)

################################################################################

# initialize N dataframe
N <- array(rep(0, times = 2 * max_age * burn_in), 
           dim = c(2, max_age, burn_in))

# initial pop size
# hatchlings
N[ , 1, 1] <- 10000
# not hatchlings
N[ , 2:max_age, 1] <- 10
 
   # temperatures
    white_noise <- rnorm(n = burn_in, mean = 0, sd = temp_sd)
    temperatures <- temp_mu + white_noise

    # move population forward in time burn_in years
for (y in 2:burn_in) {
  
  # females
  N[1, 2:max_age, y] <- rbinom(n = max_age - 1, 
                               size = round(N[1, 1:(max_age - 1), y - 1]), 
                               prob = F_survival[1:(max_age - 1)])
  
  # males
  N[2, 2:max_age, y] <- rbinom(n = max_age - 1, 
                               size = round(N[2, 1:(max_age - 1), y - 1]), 
                               prob = M_survival[1:(max_age - 1)])
  
  ##### reproduction
  # females only breed every F_remigration_int years
  # all_mature_F <- rbinom(n = max_age, 
  #                        size = round(N[1, , y-1]), 
  #                        prob = M)
  # 
  # n_breeding_F <- round(sum(all_mature_F) / F_remigration_int)
  # 
  # all_mature_M <- rbinom(n = max_age, 
  #                        size = round(N[2, , y-1]), 
  #                        prob = M)
  # 
  # n_breeding_M <- round(sum(all_mature_M) / M_remigration_int)
  
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
    hatch_success <- hatch_success_A / (1 + exp(-hatch_success_k * (temperatures[y] - hatch_success_t0)))
    
    # total hatchlings = breeding_success * total eggs * hatching success
    hatchlings <- round(breeding_success * eggs * hatch_success)
    
    # determine proportion of male hatchlings based on temperature
    prop_male <- 1 / (1 + exp(-k_piv * (temperatures[y] - T_piv)))
    
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
females_raw <- as.data.frame(N[1, , ])
View(females_raw)

males_raw <- as.data.frame(N[2, , ])
View(males_raw)

females <- females_raw %>%
  mutate(Age = 1:max_age) %>%
  pivot_longer(cols = 1:burn_in,
               names_to = "Year", 
               values_to = "Abundance") %>%
  mutate(Year = parse_number(Year)) %>%
  rename(Female = Abundance) %>%
  group_by(Year)

males <- males_raw %>%
  mutate(Age = 1:max_age) %>%
  pivot_longer(cols = 1:burn_in,
               names_to = "Year", 
               values_to = "Abundance") %>%
  mutate(Year = parse_number(Year)) %>%
  rename(Male = Abundance)

abundances <- left_join(females, males) %>%
  mutate(Total = Female + Male) %>%
  mutate(Age = factor(Age)) %>%
  group_by(Year) %>%
  mutate(PropTotal = Total/sum(Total)) %>%
  mutate(PropFemale = Female/sum(Female)) %>%
  mutate(PropMale = Male/sum(Male)) %>%
  mutate(TotalPropFemale = Female/Total) %>%
  mutate(TotalPropMale = Male/Total)

# save(abundances, file = 'output/initial_population.Rda')

ggplot(abundances, aes(x = Year, y = PropTotal, col = Age)) +
  geom_path() +
  ggtitle('total abundance')

ggplot(abundances, aes(x = Year, y = PropFemale, col = Age)) +
  geom_path() +
  ggtitle('female abundance') 

ggplot(abundances, aes(x = Year, y = PropMale, col = Age)) +
  geom_path() +
  ggtitle('male abundance')

ggplot(abundances, aes(x = Year, y = TotalPropMale, col = Age)) +
  geom_path() +
  ggtitle('male proportion of total pop size')

# final SAD for total population (both sexes)
final_total_SAD <- abundances %>%
  filter(Year == burn_in) %>%
  .$PropTotal

# sex proportions of total SAD
FemaleProp <- abundances %>%
  filter(Year == burn_in) %>%
  .$TotalPropFemale %>%
  tail(1)

MaleProp <- abundances %>%
  filter(Year == burn_in) %>%
  .$TotalPropMale %>%
  tail(1)

# sex SADs
FemaleSAD <- final_total_SAD * FemaleProp
MaleSAD <- final_total_SAD * MaleProp

# multipliers to get minimum adults to match data
F_multiplier <- F_initial / sum(FemaleSAD * M)
M_multiplier <- M_initial / sum(MaleSAD * M)

# which multiplier to use
multiplier <- min(F_multiplier, M_multiplier)

# initial population size
F_init <- ceiling(FemaleSAD * multiplier)
M_init <- ceiling(MaleSAD * multiplier)

# checks - good to go
sum(year1_females * M)
sum(year1_males * M)



