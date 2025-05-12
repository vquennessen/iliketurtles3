initialize_population <- function(beta, burn_in, max_age, M, 
                                  F_remigration_int, M_remigration_int,
                                  nests_mu, eggs_mu, hatch_success_A,
                                  hatch_success_k, hatch_success_t0,
                                  k_piv, T_piv, temp_mu, f_Leslie, m_Leslie) {
  
  # initialize N dataframe
  N <- array(rep(0, times = 2 * max_age * burn_in), 
             dim = c(2, max_age, burn_in))
  
  # initial pop size
# hatchlings
  N[ , 1, 1] <- 100
# not hatchlings
  N[ , 2:max_age, 1] <- 1
  
  # move population forward in time burn_in years
  for (y in 2:burn_in) {
    
    # step females forward
    N[1, , y] <- round(f_Leslie %*% N[1, , y - 1])
    
    # step males forward
    N[2, , y] <- round(m_Leslie %*% N[2, , y - 1])
    
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
      hatchlings <- breeding_success * eggs * hatch_success
      
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
  
  # normalize
  SAD_F <- N[1, , burn_in] / sum(N[1, , burn_in])
  SAD_M <- N[2, , burn_in] / sum(N[2, , burn_in])
  
  output <- list(SAD_F, SAD_M)
  
  return(output)
  
}
