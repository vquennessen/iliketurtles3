# population dynamics with demographic stochasticity

pop_dynamics <- function(N, max_age, y, M,
                         F_survival_immature, F_survival_mature, 
                         M_survival_immature, M_survival_mature) {
  
  #survival based on binomial distribution with survival rates as probabilities
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
  
  # breeding females this year
  breeding_F <- rbinom(n = max_age, 
                       size = N[3, , y], 
                       prob = 1 / F_remigration_int)
  
  # breeding males this year
  breeding_M <- rbinom(n = max_age, 
                       size = N[4, , y], 
                       prob = 1 / M_remigration_int)
  
  # output
  return(N, breeding_F, breeding_M)
  
}