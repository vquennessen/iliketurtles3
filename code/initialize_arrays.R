# initialize arrays

initialize_arrays <- function(scenario, yrs, max_age, 
                              IF_init, IM_init, MF_init, MM_init,
                              M, F_remigration_int, M_remigration_int, 
                              T_piv, k_piv, T_threshold, 
                              temp_mu, climate_stochasticity, 
                              season_temp_sd, clutch_temp_sd, noise, AC, 
                              evolution, trait, h2, varGenetic, 
                              conservation_action, frequency) {
  
  ##### population size ########################################################
  
  # initialize population size array
  # dimensions = sexes * ages  * years
  N <- array(rep(0, times = 4 * max_age * yrs), 
             dim = c(4, max_age, yrs))  
  
  # initial population size
  N[1, , 1] <- round(IF_init)
  N[2, , 1] <- round(IM_init)
  N[3, , 1] <- round(MF_init)
  N[4, , 1] <- round(MM_init)
  
  ##### incubation temperatures ################################################
  
  # generate mean temperature values that go up linearly 
  temp_mus <- seq(from = temp_mu, to = temp_mu + scenario, length = yrs)
  
  # if we're including climate stochasticity in the model
  if (climate_stochasticity == TRUE) {
    
    # white noise for average season temperature
    white_noise <- rnorm(n = yrs, mean = 0, sd = season_temp_sd)
    
    if (noise == 'White') {
      
      # generate stochastic temperatures from means given temp_sd
      season_temp_mus <- temp_mus + white_noise
      
    }
    
    if (noise == 'Red') {
      
      # initialize deviations
      deviations <- rep(NA, times = yrs)
      
      # first deviation term
      deviations[1] <- rnorm(n = 1, 
                             mean = white_noise[1], 
                             sd = season_temp_sd)
      
      # autocorrelated deviation series
      for (i in 2:yrs) {
        
        deviations[i] <- AC * deviations[i - 1] + white_noise[i]
        
      }
      
      season_temp_mus <- temp_mus + deviations
      
    }
    
    # if no climate stochasticity, the temperatures are just the means
  } else { season_temp_mus <- temp_mus }
  
  ##### OSR ####################################################################
  
  # OSR vector - 1 for each year
  OSRs <- rep(NA, times = yrs)
  
  # breeding females this year
  # breeding males this year
  available_F <- rbinom(n = max_age, 
                        size = as.integer(MF_init), 
                        prob = 1 / F_remigration_int)  
  
  n_available_F <- sum(as.numeric(available_F, na.rm = TRUE))
  
  # breeding males this year
  available_M <- rbinom(n = max_age, 
                        size = as.integer(MM_init), 
                        prob = 1 / M_remigration_int)
  
  n_available_M <- sum(as.numeric(available_M, na.rm = TRUE))
  
  # check to make sure there is at least one available F and M
  if (n_available_F < 1 | n_available_M < 1) { OSRs[1] <- NA
  
  # calculate first operational sex ratio
  } else { OSRs[1] <- n_available_M / (n_available_M + n_available_F) }
  
  ##### evolution ##############################################################
  
  # if evolution_piv is turned on, create gamma, epsilon, and delta vectors
  if (evolution == TRUE) {  
    
    # segregation variance, error term for offspring genotype, one for each year
    varSegregation <- varGenetic / 2 
    
    # phenotypic variance, error term for offspring phenotype, one for each year
    varPhenotypic <- varGenetic / h2  
    
    # genotype array, dimensions sex * age * max pop size
    G <- array(rep(NA, times = 4 * max_age * max_N), 
               dim = c(4, max_age, max_N))
    
    # genotype summary stats to save, dimensions sex * age * year * # stats
    # G[, , , 1] = mean
    # G[, , , 2] = median
    # G[, , , 3] = variance
    G_stats <- array(rep(NA, times = 4 * max_age * yrs * 3), 
                    dim = c(4, max_age, yrs, 3))
    
    # which trait is evolving?
    value <- ifelse(trait == 'T_piv', 
                    T_piv, 
                    emergence_succcess_t0)
    
    # year 1
    A <- apply(N[, , 1], c(1, 2), rnorm, mean = value, sd = sqrt(varGenetic))
    
    # https://stackoverflow.com/questions/43415577/equalizing-the-lengths-of-all-the-lists-within-a-list
    G[, , ] <- aperm(array(unlist(lapply(lapply(sapply(A, unlist), 
                                       "length<-", max_N), 
                                as.list)), 
                           dim = c(max_N, 4, max_age)), 
                       c(2, 3, 1))
    
    # genotype stats
    G_stats[, , 1, 1] <- apply(G, c(1, 2), mean, na.rm = TRUE)
    G_stats[, , 1, 2] <- apply(G, c(1, 2), median, na.rm = TRUE)
    G_stats[, , 1, 3] <- apply(G, c(1, 2), var, na.rm = TRUE)
    
    # phenotype array, dimensions sex * age * years
    P <- rnorm(n = c(4 * max_age * max_N), 
                   mean = G, 
                   sd = sqrt(varPhenotypic))
    
    # phenotype summary stats to save, dimensions sex * age * year * # stats
    # P_stats[, , , 1] = mean
    # P_stats[, , , 2] = median
    # P_stats[, , , 3] = variance
    P_stats <- array(rep(NA, times = 4 * max_age * yrs * 3), 
                     dim = c(4, max_age, yrs, 3))
    
    # phenotype stats
    P_stats[, , 1, 1] <- apply(P, c(1, 2), mean, na.rm = TRUE)
    P_stats[, , 1, 2] <- apply(P, c(1, 2), median, na.rm = TRUE)
    P_stats[, , 1, 3] <- apply(P, c(1, 2), var, na.rm = TRUE)
    
  } else {
    
    # pivotal temperatures without evolution
    varSegregation <- NULL
    varPhenotypic  <- NULL
    G              <- NULL
    G_stats        <- NULL
    P              <- NULL
    P_stats        <- NULL
    
  }
  
  ##### conservation ###########################################################
  
  if (conservation_action == TRUE) {
    
    conservation_years <- seq(from = 1, by = frequency, length = yrs)
    
  } else { conservation_years <- NULL }
  
  ##### output #################################################################
  
  # output
  output <- list(N, season_temp_mus, OSRs, 
                 varSegregation, varPhenotypic, G, G_stats, P, P_stats, 
                 conservation_years)
  
  return(output)
  
}
