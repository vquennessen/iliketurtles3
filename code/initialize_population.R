initialize_population <- function(beta, burn_in, max_age, 
                                  F_survival, M_survival, M, 
                                  F_remigration_int, M_remigration_int,
                                  nests_mu, eggs_mu, hatch_success_A,
                                  hatch_success_k, hatch_success_t0,
                                  k_piv, T_piv, temp_mu, 
                                  F_initial, M_initial) {
  
  # initialize N dataframe
  N <- array(rep(0, times = 2 * max_age * burn_in), 
             dim = c(2, max_age, burn_in))
  
  # initial pop size
# hatchlings
  N[ , 1, 1] <- 10000
# not hatchlings
  N[ , 2:max_age, 1] <- 100
  
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
    all_mature_F <- rbinom(n = max_age, 
                           size = round(N[1, , y-1]), 
                           prob = M)
    
    n_breeding_F <- round(all_mature_F / F_remigration_int)
    
    all_mature_M <- rbinom(n = max_age, 
                           size = round(N[2, , y-1]), 
                           prob = M)
    
    n_breeding_M <- round(all_mature_M / M_remigration_int)
    
    # n_breeding_F <- round(sum(round(N[1, , y] * M), na.rm = TRUE) / F_remigration_int)
    # 
    # # males only breed every M_remigration_int years
    # n_breeding_M <- round(sum(round(N[2, , y] * M), na.rm = TRUE) / M_remigration_int)
    
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
      
      # number of potential eggs, assuming full reproductive success
      eggs <- sum(n_breeding_F * round(nests_mu) * round(eggs_mu))
      
      # hatching success
      hatch_success <- hatch_success_A / (1 + exp(-hatch_success_k * (temp_mu - hatch_success_t0)))
      
      # total hatchlings = breeding_success * total potential eggs * hatching success
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
    
  }
  
  # raw male and female counts
  females_raw <- as.data.frame(N[1, , ])
  males_raw <- as.data.frame(N[2, , ])
  
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
  
  save(abundances, file = '../output/initial_population.Rda')
  
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
  
  output <- list(F_init, M_init)
  
  return(output)
  
}
