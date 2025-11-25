# reproduction

reproduction <- function(N, M, y, beta, max_age, 
                         F_remigration_interval, M_remigration_interval,
                         clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                         emergence_success_A, emergence_success_k, 
                         emergence_success_t0, 
                         season_temp_mus, clutch_temp_sd,
                         k_piv, T_piv, T_threshold, 
                         evolution, trait, max_N, male_probs, contributions,
                         h2, varGenetic, varPhenotypic, G, P, G_stats, P_stats,
                         conservation_action, conservation_years, 
                         intensity, effect_size) {
  
  # breeding females this year
  available_F <- rbinom(n = max_age, 
                        size = N[3, 2:max_age, y], 
                        prob = 1 / F_remigration_int)
  
  # breeding males this year
  available_M <- rbinom(n = max_age, 
                        size = N[4, 2:max_age, y], 
                        prob = 1 / M_remigration_int)
  
  # number of breeding females this year
  n_available_F <- sum(as.numeric(available_F), na.rm = TRUE)
  
  # number of breeding males this year
  n_available_M <- sum(as.numeric(available_M), na.rm = TRUE)
  
  # check that there is at least one available female and one available male
  if (n_available_F < 1 | n_available_M < 1) {
    
    OSR <- NA
    female_hatchlings <- 0
    male_hatchlings <- 0 
    G[1, 1] <- list(NA)
    G[2, 1] <- list(NA)
    P[1, 1] <- list(NA)
    P[2, 1] <- list(NA)
    G_stats[1:2, 1, y, ] <- rep(NA, 6)
    P_stats[1:2, 1, y, ] <- rep(NA, 6)
    
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
    breeding_F <- rbinom(n = n_available_F, 
                         size = 1, 
                         prob = breeding_success)
    
    n_breeding_F <- sum(breeding_F)
    
    if (n_breeding_F < 1) {
      
      female_hatchlings <- 0
      male_hatchlings <- 0 
      G[1, 1] <- list(NA)
      G[2, 1] <- list(NA)
      P[1, 1] <- list(NA)
      P[2, 1] <- list(NA)
      G_stats[1:2, 1, y, ] <- rep(NA, 6)
      P_stats[1:2, 1, y, ] <- rep(NA, 6)
      
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
      if (evolve == TRUE) {
        
        evo_output <- evolution(G, P, n_breeding_F, n_available_M, 
                                male_probs, contributions, trait,
                                clutches, eggs, clutch_temps, 
                                emergence_success_A, emergence_success_k, 
                                emergence_success_t0, 
                                T_threshold, k_piv, T_piv)
        
        G[1, 1] <- evo_output[[1]]
        G[2, 1] <- evo_output[[2]]
        
        P[1, 1] <- evo_output[[3]]
        P[2, 1] <- evo_output[[4]]
        
        G_stats[1:2, 1, y, ] <- evo_output[[5]]
        P_stats[1:2, 1, y, ] <- evo_output[[6]]
        
        # numbers of hatchlings produced
        female_hatchlings <- length(unlist(G[1, 1]))
        male_hatchlings <- length(unlist(G[2, 1]))
        
       } else {
        
        # list of probability of emergence, one number for each clutch 
        # set.seed(seed)
        probs_emerged <- lapply(clutch_temps, 
                                emergence_success, 
                                A = emergence_success_A, 
                                k = emergence_success_k, 
                                t0 = emergence_success_t0, 
                                thermal_limit = T_threshold) %>%
          lapply(pmax, 0)
        
        # v_probs_emerged <- unlist(probs_emerged)
        
        # list of probabilities of developing as male, one for each clutch
        # set.seed(seed)
        probs_male <- lapply(clutch_temps, 
                             probability_male,
                             k = k_piv, 
                             pivotal_temp = T_piv) %>%
          lapply(pmax, 0)
        
        # list of numbers of emerged hatchlings, one for each clutch
        # set.seed(seed)
        hatchlings <- length(as.integer(unlist(eggs))) %>%
          rbinom(size = as.integer(unlist(eggs)), 
                 prob = unlist(probs_emerged)) %>%
          split(rep(1:length(clutches), times = clutches))  %>%
          lapply(pmax, 0) %>%
          lapply(round)
        
        # list of number of males, one for each clutch
        males <- rbinom(n = length(unlist(hatchlings)), 
                        size = as.integer(unlist(hatchlings)), 
                        prob = unlist(probs_male)) %>%
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
    
  }
  
  # output
  output <- list(OSR, female_hatchlings, male_hatchlings, 
                 G, P, G_stats, P_stats)
  
  return(output)
  
}
