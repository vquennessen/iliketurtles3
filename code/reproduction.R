# reproduction

reproduction <- function(N, M, y, beta, max_age, 
                         F_remigration_interval, M_remigration_interval,
                         clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                         emergence_success_A, emergence_success_k, 
                         emergence_success_t0, 
                         season_temp_mus, clutch_temp_sd,
                         k_piv, T_piv, T_threshold, evolution, 
                         trait, varSegregation, varPhenotypic, 
                         G, G_stats, P, P_stats,  
                         conservation_action, conservation_years, 
                         intensity, effect_size) {
  
  # breeding females this year
  available_F <- rbinom(n = max_age, 
                        size = N[3, , y], 
                        prob = 1 / F_remigration_int)
  
  # breeding males this year
  available_M <- rbinom(n = max_age, 
                        size = N[4, , y], 
                        prob = 1 / M_remigration_int)
  
  # breeding females this year
  n_available_F <- sum(as.numeric(available_F, na.rm = TRUE))
  
  # breeding males this year
  n_available_M <- sum(as.numeric(available_M, na.rm = TRUE))
  
  # check that there is at least one available female and one available male
  if (n_available_F < 1 | n_available_M < 1) {
    
    OSR <- NA
    female_hatchlings <- 0
    male_hatchlings <- 0  
    
  } else {
    
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
                               prob = breeding_success))
    
    if (n_breeding_F < 1) {
      
      female_hatchlings <- 0
      male_hatchlings <- 0  
      
    } else {
      
      # vector of number of clutches per female (round to nearest integer)
      # set.seed(seed)
      clutches <- round(rnorm(n = round(n_breeding_F), 
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
      
      # adjust clutch temperatures based on conservation measures
      if (conservation_action == TRUE & y %in% conservation_years) {
        
        clutch_temps <- conservation(initial_temps = clutch_temps, 
                                     intensity = intensity, 
                                     effect_size = effect_size)
        
      }
      
      # if evolution
      if (evolution == TRUE) {
        
        
        
      }
      
      # list of probability of emergence, one number for each clutch 
      # set.seed(seed)
      probs_emerged <- lapply(clutch_temps, 
                              emergence_success, 
                              A = emergence_success_A, 
                              k = emergence_success_k, 
                              t0 = emergence_success_t0, 
                              thermal_limit = Threshold_temps[y]) %>%
        lapply(pmax, 0)
      
      # vector of eggs
      v_eggs <- as.integer(unlist(eggs))
      
      # vector of prop_emerged
      v_probs_emerged <- unlist(probs_emerged)
      
      # list of numbers of emerged hatchlings, one for each clutch
      # set.seed(seed)
      hatchlings <- length(v_eggs) %>%
        rbinom(size = as.integer(v_eggs), prob = v_probs_emerged) %>%
        split(rep(1:length(clutches), times = clutches))  %>%
        lapply(pmax, 0) %>%
        lapply(round)
      
      # list of probabilities of developing as male, one for each clutch
      # set.seed(seed)
      probs_male <- lapply(clutch_temps, 
                           probability_male,
                           k = k_piv, 
                           pivotal_temp = Pivotal_temps[y]) %>%
        lapply(pmax, 0)
      
      # vector of hatchlings
      v_hatchlings <- unlist(hatchlings)
      
      # vector of probs_male
      v_probs_male <- unlist(probs_male)
      
      # list of number of males, one for each clutch
      # set.seed(seed)
      males <- rbinom(n = length(v_hatchlings), 
                      size = as.integer(v_hatchlings), 
                      prob = v_probs_male) %>%
        split(rep(1:length(clutches), times = clutches)) %>%
        lapply(pmax, 0) %>%
        lapply(round)
      
      # list of number of females, one for each clutch
      females <- Map('-', hatchlings, males) %>%
        lapply(round)
      
      # total number of male hatchlings
      male_hatchlings <- sum(unlist(males))
      
      # total number of female hatchlings
      female_hatchlings <- sum(unlist(females))
      
    }
    
  }
  
  # output
  output <- list(female_hatchlings, male_hatchlings, OSR)
  
  return(output)
  
}
