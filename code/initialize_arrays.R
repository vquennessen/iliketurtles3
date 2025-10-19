# initialize arrays

initialize_arrays <- function(scenario, yrs, max_age, 
                              IF_init, IM_init, MF_init, MM_init,
                              M, F_remigration_int, M_remigration_int, 
                              T_piv, k_piv, h2_piv, ag_var_piv, 
                              evolution_piv,
                              T_threshold, h2_threshold, ag_var_threshold, 
                              evolution_threshold,
                              temp_mu, climate_stochasticity, 
                              season_temp_sd, clutch_temp_sd, noise, AC, 
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
  
  ##### pivotal temperatures ###################################################
  
  # if evolution_piv is turned on, create gamma, epsilon, and delta vectors
  if (evolution_piv == TRUE) {  
    
    # gamma, error term for the expected genotype, one for each year
    Gamma_piv <- rnorm(n = yrs, 
                       mean = 0, 
                       sd = sqrt(ag_var_piv / 2))     
    
    # delta, error term for the expected pivotal temperature, one for each year
    Delta_piv <- rnorm(n = yrs, 
                       mean = 0, 
                       sd = sqrt((ag_var_piv / h2_piv - ag_var_piv)))  
    
    # epsilon, error term for the observed pivotal temperature (phenotypic 
    # variation), one for each year
    Epsilon_piv <- rnorm(n = yrs, 
                         mean = 0, 
                         sd = sqrt(ag_var_piv / h2_piv)) 
    
    # initial genes arrays, mu and sd, dimensions sex * age * years
    genes_piv_mu <- array(rep(NA, times = 4 * max_age * yrs), 
                          dim = c(4, max_age, yrs))
    genes_piv_sd <- array(rep(NA, times = 4 * max_age * yrs), 
                          dim = c(4, max_age, yrs))
    # year 1
    genes_piv_mu[, , 1] <- rnorm(n = 4*max_age, 
                                 mean = T_piv, 
                                 sd = sqrt(ag_var_piv / 2))
    
    # G_piv <- rnorm(n = max_age, 
    #                mean = T_piv, 
    #                sd = sqrt(ag_var_piv / 2))
    
    # initial phenotype array, dimensions = sex * max_age * years
    # phenotype = expected pivotal temperature
    phen_piv <- array(rep(NA, times = 4 * max_age * yrs), 
                      dim = c(4, max_age, yrs))
    
    # year 1
    
    
    # # initial expected phenotypes vector, one for each age, updated each year
    # P_piv <- G_piv + rnorm(n = max_age, 
    #                        mean = 0, 
    #                        sd = sqrt((ag_var_piv / h2_piv - ag_var_piv)))
    
    # initialize observed hatchling pivotal temperatures vector, one for each 
    # year
    Pivotal_temps <- rep(NA, yrs)
    Pivotal_temps[1] <- P_piv[1] + Epsilon_piv[1]
    
    
  } else {
    
    # pivotal temperatures without evolution
    Pivotal_temps <- rep(T_piv, times = yrs)
    
    # no Delta, G, P, epsilon,  or gamma vectors
    Delta_piv         <- NULL
    G_piv             <- NULL
    P_piv             <- NULL
    Epsilon_piv       <- NULL 
    Gamma_piv         <- NULL
    
  }
  
  ##### threshold temperatures ################################################# 
  
  # if evolution_threshold is turned on, create gamma, epsilon, and delta vectors
  if (evolution_threshold == TRUE) {  
    
    # distribution of G - starting genotypes plus genotypic variation
    G_threshold <- rnorm(n = max_age, 
                         mean = T_threshold, 
                         sd = sqrt(ag_var_threshold))
    
    # distribution of P - starting expected phenotypes
    # starting genotypes plus environmental variation
    P_threshold<- G_threshold + rnorm(n = max_age, 
                                      mean = 0, 
                                      sd = sqrt((ag_var_threshold / 
                                                   h2_threshold - 
                                                   ag_var_threshold)))
    
    # intialize actual threshold temperatures vector
    # expected phenotypes plus phenotypic variation
    Threshold_temps <- rep(NA, times = yrs)
    Threshold_temps[1] <- P_threshold[1] + rnorm(n = 1, 
                                                 mean = 0, 
                                                 sd = sqrt(ag_var_threshold / 
                                                             h2_threshold))
    
    # gamma, error term for the expected genotype
    Gamma_threshold <- rnorm(n = yrs, 
                             mean = 0, 
                             sd = sqrt(ag_var_threshold / 2))       
    
    # epsilon, error term for the expected threshold temperature
    Epsilon_threshold <- rnorm(n = yrs, 
                               mean = 0, 
                               sd = sqrt((ag_var_threshold / 
                                            h2_threshold - 
                                            ag_var_threshold)))
    # phenotypic variation - threshold temperatures
    Delta_threshold <- rnorm(n = yrs, 
                             mean = 0, 
                             sd = sqrt(ag_var_threshold / h2_threshold))
    
  } else {
    
    # threshold temperatures without evolution
    Threshold_temps <- rep(T_threshold, times = yrs)
    
    # no G, P, epsilon, gamma, or delta vectors
    G_threshold             <- NULL
    P_threshold             <- NULL
    Epsilon_threshold       <- NULL 
    Gamma_threshold         <- NULL
    Delta_threshold         <- NULL
    
  }
  
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
  
  ##### conservation ###########################################################
  if (conservation_action == TRUE) {
    
    conservation_years <- seq(from = 1, by = frequency, length = yrs)
    
  } else { conservation_years <- NULL }
  
  ##### output #################################################################
  
  # output
  output <- list(N, season_temp_mus,
                 G_piv, P_piv, Gamma_piv, Epsilon_piv, Delta_piv, Pivotal_temps, 
                 G_threshold, P_threshold, Gamma_threshold, Epsilon_threshold, 
                 Delta_threshold, Threshold_temps, 
                 OSRs, 
                 conservation_years)
  
  return(output)
  
}
