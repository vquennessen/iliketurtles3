# reproduction

reproduction <- function(N, M, y, beta, max_age, breeding_F, breeding_M,
                         clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                         emergence_success_A, emergence_success_k, 
                         emergence_success_t0, 
                         season_temp_mus, clutch_temp_sd,
                         k_piv, Pivotal_temps, Threshold_temps, T_threshold, 
                         conservation_action, conservation_years, intensity, 
                         effect_size) {
  
  # breeding females this year
  n_available_F <- sum(as.numeric(breeding_F, na.rm = TRUE))
  
  # breeding males this year
  n_available_M <- sum(as.numeric(breeding_M, na.rm = TRUE))
  
  if (n_available_F > 0 & n_available_M > 0) {
    
    # operational sex ratio - proportion of males
    # multiply by 2 to transform to beta function with x from 0 to 0.5 instead 
    # of 0 to 1
    OSR <- n_available_M / (n_available_M + n_available_F)
    
    # calculate reproductive success
    # if 50% males or fewer, use beta function to calculate breeding success
    # multiply OSR by 2 to transform to beta function with x from 0 to 0.5 
    # instead of 0 to 1
    if (OSR < 0.5) {
      breeding_success <- pbeta(2 * OSR, 
                                shape1 = 1, 
                                shape2 = beta) 
      
      # if OSR > 0.5 all the females get to mate
    } else { breeding_success <- 1 }
    
    # how many females actually find a male to mate with and then nest
    n_breeding_F <- sum(rbinom(n = n_available_F, 
                               size = 1, 
                               prob = breeding_success))
    
    # number of clutches per female (round to nearest integer)
    clutches <- round(rnorm(n = round(n_breeding_F), 
                            mean = clutches_mu, 
                            sd = clutches_sd))
    
    # replace any number < 1 with +1
    clutches[which(clutches < 1)] <- 1
    
    # potential eggs list, one number for each clutch
    eggs_raw <- lapply(clutches, 
                       rnorm, 
                       mean = eggs_mu, 
                       sd = eggs_sd)
    
    eggs <- lapply(eggs_raw, 
                   round)
    
    # clutch temperature list, one number for each clutch 
    clutch_temps <- lapply(clutches, 
                           rnorm, 
                           mean = season_temp_mus[y], 
                           sd = clutch_temp_sd)
    
    if (conservation_action == TRUE & y %in% conservation_years) {
      
      clutch_temps <- conservation(initial_temps = clutch_temps, 
                                   intensity = intensity, 
                                   effect_size = effect_size)
      
    }
    
    # emergence successes vector, one number for each clutch 
    prop_emerged <- lapply(clutch_temps, 
                           emergence_success, 
                           A = emergence_success_A, 
                           k = emergence_success_k, 
                           t0 = emergence_success_t0, 
                           thermal_limit = T_threshold)
    
    # hatchlings that emerged list, one for each clutch
    hatchlings_raw <- lapply(multiply.list(eggs, prop_emerged), 
                             round)
    
    # rounding
    hatchlings <- lapply(hatchlings_raw, 
                         round)
    
    # proportions male list, one number for each clutch 
    props_male <- lapply(clutch_temps, 
                         proportion_male,
                         k = k_piv, 
                         pivotal_temp = Pivotal_temps[y])
    
    # proportions female list, one number for each clutch: 1 - props_male
    props_female <- mapply('-', 1, props_male, SIMPLIFY = FALSE)
    
    # male and female hatchlings summed across all clutches
    female_hatchlings_raw <- multiply.list(hatchlings, props_female)
    female_hatchlings <- sum(unlist(lapply(female_hatchlings_raw, round)))
    male_hatchlings_raw <- multiply.list(hatchlings, props_male)
    male_hatchlings <- sum(unlist(lapply(male_hatchlings_raw, round)))
    
  } else {     
    
    OSR <- NA
    female_hatchlings <- 0
    male_hatchlings <- 0  
    
  }
  
  # output
  output <- list(female_hatchlings, male_hatchlings, OSR)
  
  return(output)
  
}
