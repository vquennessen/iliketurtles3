# initialize arrays

initialize_arrays <- function(scenario, yrs, max_age, 
                              IF_init, IM_init, MF_init, MM_init,
                              M, F_remigration_int, M_remigration_int, 
                              T_piv, k_piv, T_threshold, 
                              temp_mu, climate_stochasticity, 
                              season_temp_sd, clutch_temp_sd, noise, AC, 
                              evolve, trait, value, varGenetic, varPhenotypic,
                              conserve, frequency) {
  
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
    
    if (noise == 'white') {
      
      # generate stochastic temperatures from means given temp_sd
      season_temp_mus <- temp_mus + white_noise
      
    }
    
    if (noise == 'red') {
      
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
  
  n_available_F <- sum(as.numeric(available_F), na.rm = TRUE)
  
  # breeding males this year
  available_M <- rbinom(n = max_age, 
                        size = as.integer(MM_init), 
                        prob = 1 / M_remigration_int)
  
  n_available_M <- sum(as.numeric(available_M), na.rm = TRUE)
  
  # check to make sure there is at least one available F and M
  if (n_available_F < 1 | n_available_M < 1) { OSRs[1] <- NA
  
  # calculate first operational sex ratio
  } else { OSRs[1] <- n_available_M / (n_available_M + n_available_F) }
  
  ##### evolution ##############################################################
  
  if (evolve == TRUE) {
    
    G <- asplit(array(rep(NA, times = c(4 * max_age * 1)), 
                      dim = c(4, max_age, 1)), 
                c(1, 2))
    
    G[1, ] <- lapply(IF_init, 
                     function(x) rnorm(n = x, 
                                       mean = value, 
                                       sd = sqrt(varGenetic)))
    G[2, ] <- lapply(IM_init, 
                     function(x) rnorm(n = x, 
                                       mean = value, 
                                       sd = sqrt(varGenetic)))
    G[3, ] <- lapply(MF_init, 
                     function(x) rnorm(n = x, 
                                       mean = value, 
                                       sd = sqrt(varGenetic)))
    G[4, ] <- lapply(MM_init, 
                     function(x) rnorm(n = x, 
                                       mean = value, 
                                       sd = sqrt(varGenetic)))
    
    P  <- asplit(array(rep(NA, times = c(4 * max_age * 1)), 
                       dim = c(4, max_age, 1)), 
                 c(1, 2))
    
    P[1, ] <- lapply(G[1, ], 
                     function(x) rnorm(n = length(unlist(x)), 
                                       mean = x, 
                                       sd = sqrt(varPhenotypic)))
    P[2, ] <- lapply(G[2, ], 
                     function(x) rnorm(n = length(unlist(x)), 
                                       mean = x, 
                                       sd = sqrt(varPhenotypic)))
    P[3, ] <- lapply(G[3, ], 
                     function(x) rnorm(n = length(unlist(x)), 
                                       mean = x, 
                                       sd = sqrt(varPhenotypic)))
    P[4, ] <- lapply(G[4, ], 
                     function(x) rnorm(n = length(unlist(x)), 
                                       mean = x, 
                                       sd = sqrt(varPhenotypic)))
    
    G_stats <- array(rep(NA, times = 4 * max_age * yrs * 3), 
                     dim = c(4, max_age, yrs, 3))
    P_stats <- array(rep(NA, times = 4 * max_age * yrs * 3), 
                     dim = c(4, max_age, yrs, 3))
    
    # genotype and phenotype summary stats, dimensions sex * age * year * # stats
    # genotype stats
    G_stats[, , 1, 1] <- apply(G, c(1, 2), 
                               function(x) mean(unlist(x), na.rm = TRUE))    
    G_stats[, , 1, 2] <- apply(G, c(1, 2), 
                               function(x) median(unlist(x), na.rm = TRUE))
    G_stats[, , 1, 3] <- apply(G, c(1, 2), 
                               function(x) var(unlist(x), na.rm = TRUE))
    
    # phenotype stats
    P_stats[, , 1, 1] <- apply(P, c(1, 2), 
                               function(x) mean(unlist(x), na.rm = TRUE))    
    P_stats[, , 1, 2] <- apply(P, c(1, 2), 
                               function(x) median(unlist(x), na.rm = TRUE))
    P_stats[, , 1, 3] <- apply(P, c(1, 2), 
                               function(x) var(unlist(x), na.rm = TRUE))
    
  } else {
    
    G_stats <- NULL
    P_stats <- NULL
    
  }
  
  ##### conservation ###########################################################
  
  if (conserve == TRUE) {
    
    conservation_years <- seq(from = 1, by = frequency, length = yrs)
    
  } else { conservation_years <- NULL }
  
  ##### output #################################################################
  
  # output
  output <- list(N, 
                 season_temp_mus, 
                 OSRs, 
                 G, P, G_stats, P_stats, 
                 conservation_years)
  
  return(output)
  
}
