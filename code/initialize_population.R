initialize_population <- function(beta, burn_in, max_age, 
                                  F_survival_immature, F_survival_mature, 
                                  M_survival_immature, M_survival_mature, 
                                  M, F_remigration_int, M_remigration_int,
                                  clutches_mu, eggs_mu, hatch_success_A, 
                                  hatch_success_k, hatch_success_t0, 
                                  k_piv, T_piv, temp_mu, 
                                  F_initial, M_initial) {
  
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
                             prob = F_survival_immature[1:(max_age - 1)])
    
    # immature females that matured
    new_mature_F <- rbinom(n = max_age - 1, 
                           size = all_immature_F, 
                           prob = M)
    
    # updated immature female population
    N[1, 2:max_age, y] <- as.numeric(all_immature_F) - as.numeric(new_mature_F)
    
    # mature females that survived
    mature_survived_F <- rbinom(n = max_age - 1, 
                                size = round(N[3, 1:(max_age - 1), y - 1]), 
                                prob = F_survival_mature)
    
    # updated mature female population
    N[3, 2:max_age, y] <- as.numeric(mature_survived_F) + as.numeric(new_mature_F)
    
    # immature males that survived
    all_immature_M <- rbinom(n = max_age - 1, 
                             size = round(N[2, 1:(max_age - 1), y - 1]), 
                             prob = M_survival_immature[1:(max_age - 1)])
    
    # immature males that matured
    new_mature_M <- rbinom(n = max_age - 1, 
                           size = all_immature_M, 
                           prob = M)
    
    # updated immature male population
    N[2, 2:max_age, y] <- as.numeric(all_immature_M) - as.numeric(new_mature_M)
    
    # mature males that survived
    mature_survived_M <- rbinom(n = max_age - 1, 
                                size = round(N[4, 1:(max_age - 1), y - 1]), 
                                prob = M_survival_mature)
    
    # updated mature male population
    N[4, 2:max_age, y] <- as.numeric(mature_survived_M) + as.numeric(new_mature_M)
    
    ##### reproduction
    
    # breeding females this year
    breeding_F <- rbinom(n = max_age, 
                         size = N[3, , y], 
                         prob = 1 / F_remigration_int)
    
    n_breeding_F <- sum(as.numeric(breeding_F, na.rm = TRUE))
    
    # breeding males this year
    breeding_M <- rbinom(n = max_age, 
                         size = N[4, , y], 
                         prob = 1 / M_remigration_int)
    
    n_breeding_M <- sum(as.numeric(breeding_M, na.rm = TRUE))
    
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
      eggs <- sum(n_breeding_F * round(clutches_mu) * round(eggs_mu))
      
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
    # mutate(Total_Immature = Female_Immature + Male_Immature) %>%
    # mutate(Total_Mature = Female_Mature + Male_Mature) %>%
    # mutate(Total_Female = Female_Immature + Female_Mature) %>%
    # mutate(Total_Male = Male_Immature + Male_Mature) %>%
    mutate(Total = Female_Immature + Male_Immature + Female_Mature + Male_Mature) %>% 
    mutate(Age = factor(Age)) %>%
    group_by(Year) %>%
    mutate(Prop_Total = Total / sum(Total)) %>%
    mutate(Total_Female_Immature = sum(Female_Immature)) %>%
    mutate(Total_Male_Immature = sum(Male_Immature)) %>%
    mutate(Total_Female_Mature = sum(Female_Mature)) %>%
    mutate(Total_Male_Mature = sum(Male_Mature)) %>%
    mutate(Prop_Female_Immature = Female_Immature / Total_Female_Immature) %>%
    mutate(Prop_Male_Immature = Male_Immature / Total_Male_Immature) %>%
    mutate(Prop_Female_Mature = Female_Mature / Total_Female_Mature) %>%
    mutate(Prop_Male_Mature = Male_Mature / Total_Male_Mature) %>%
    mutate(Total_Prop_Female_Immature = Total_Female_Immature / Total) %>%
    mutate(Total_Prop_Male_Immature = Total_Male_Immature / Total) %>%
    mutate(Total_Prop_Female_Mature = Total_Female_Mature / Total) %>%
    mutate(Total_Prop_Male_Mature = Total_Male_Mature / Total)

  # save(abundances, file = '../output/initial_population.Rda')
  
  # final SAD for total population (both sexes)
  final_total_SAD <- abundances %>%
    filter(Year == burn_in) %>%
    .$Prop_Total
  
  # sex proportions of total SAD
  Female_Prop_Immature <- abundances %>%
    filter(Year == burn_in) %>%
    .$Total_Prop_Female_Immature %>%
    tail(1)
  
  Male_Prop_Immature <- abundances %>%
    filter(Year == burn_in) %>%
    .$Total_Prop_Male_Immature %>%
    tail(1)
  
  Female_Prop_Mature <- abundances %>%
    filter(Year == burn_in) %>%
    .$Total_Prop_Female_Mature %>%
    tail(1)
  
  Male_Prop_Mature <- abundances %>%
    filter(Year == burn_in) %>%
    .$Total_Prop_Male_Mature %>%
    tail(1)
  
  # sex SADs
  Female_Immature_SAD <- final_total_SAD * Female_Prop_Immature
  Male_Immature_SAD <- final_total_SAD * Male_Prop_Immature
  Female_Mature_SAD <- final_total_SAD * Female_Prop_Mature
  Male_Mature_SAD <- final_total_SAD * Male_Prop_Mature
  
  # multipliers to get minimum adults to match data
  F_Immature_multiplier <- F_initial / sum(Female_Immature_SAD * M)
  M_Immature_multiplier <- M_initial / sum(Male_Immature_SAD * M)
  F_Mature_multiplier <- F_initial / sum(Female_Mature_SAD * M)
  M_Mature_multiplier <- M_initial / sum(Male_Mature_SAD * M)
  
  # which multiplier to use
  multiplier <- min(F_Immature_multiplier, M_Immature_multiplier, 
                    F_Mature_multiplier, M_Mature_multiplier)
  
  # initial population size
  F_Immature_init <- round(Female_Immature_SAD * multiplier)
  M_Immature_init <- round(Male_Immature_SAD * multiplier)
  F_Mature_init <- round(Female_Mature_SAD * multiplier)
  M_Mature_init <- round(Male_Mature_SAD * multiplier)
  
  output <- list(F_Immature_init, M_Immature_init, F_Mature_init, M_Mature_init)
  
  return(output)
  
}
