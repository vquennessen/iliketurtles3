# initialize arrays

initialize_arrays <- function(scenario, years, max_age, 
                              F_Immature_init, M_Immature_init, 
                              F_Mature_init, M_Mature_init,
                              M, F_remigration_int, M_remigration_int, 
                              T_piv, k_piv, h2_piv, ag_var_piv, 
                              evolution_piv,
                              T_threshold, h2_threshold, ag_var_threshold, 
                              evolution_threshold,
                              temp_mu, climate_stochasticity, 
                              season_temp_sd, clutch_temp_sd, noise, AC, 
                              conservation, frequency) {
  
  ##### population size ########################################################
  
  # initialize population size array
  # dimensions = sexes * ages  * years
  N <- array(rep(0, times = 4 * max_age * years), 
             dim = c(4, max_age, years))  
  
  # initial population size
  N[1, , 1] <- round(F_Immature_init)
  N[2, , 1] <- round(M_Immature_init)
  N[3, , 1] <- round(F_Mature_init)
  N[4, , 1] <- round(M_Mature_init)
  
  ##### incubation temperatures ################################################
  
  # generate mean temperature values that go up linearly 
  temp_mus <- seq(from = temp_mu, to = temp_mu + scenario, length = years)
  
  # if we're including climate stochasticity in the model
  if (climate_stochasticity == TRUE) {
    
    # white noise for average season temperature
    white_noise <- rnorm(n = years, mean = 0, sd = season_temp_sd)
    
    if (noise == 'White') {
      
      # generate stochastic temperatures from means given temp_sd
      season_temp_mus <- temp_mus + white_noise
      
    }
    
    if (noise == 'Red') {
      
      # initialize deviations
      deviations <- rep(NA, times = years)
      
      # first deviation term
      deviations[1] <- rnorm(n = 1, 
                             mean = white_noise[1], 
                             sd = season_temp_sd)
      
      # autocorrelated deviation series
      for (i in 2:years) {
        
        deviations[i] <- AC * deviations[i - 1] + white_noise[i]
        
      }
      
      season_temp_mus <- temp_mus + deviations
      
    }
    
    # if no climate stochasticity, the temperatures are just the means
  } else { season_temp_mus <- temp_mus }
  
  ##### pivotal temperatures ###################################################
  
  # if evolution_piv is turned on, create gamma, epsilon, and delta vectors
  if (evolution_piv == TRUE) {  
    
    # distribution of G - starting genotypes plus genotypic variation - one for 
    # each cohort
    G_piv <- rnorm(n = max_age, 
                   mean = T_piv, 
                   sd = sqrt(ag_var_piv))
    
    # distribution of P - starting expected phenotypes - one for each age
    # starting genotypes plus environmental variation
    P_piv <- G_piv + rnorm(n = max_age, 
                           mean = 0, 
                           sd = sqrt((ag_var_piv / h2_piv - ag_var_piv)))
    
    # intialize actual pivotal temperatures - one for each year
    # expected phenotypes plus phenotypic variation
    Pivotal_temps <- rep(NA, years)
    Pivotal_temps[1] <- P_piv[1] + rnorm(n = 1, 
                                         mean = 0, 
                                         sd = sqrt(ag_var_piv / h2_piv))
    
    # gamma, error term for the expected genotype
    Gamma_piv <- rnorm(n = years, 
                       mean = 0, 
                       sd = sqrt(ag_var_piv / 2))       
    
    # epsilon, error term for the expected pivotal temperature
    Epsilon_piv <- rnorm(n = years, 
                         mean = 0, 
                         sd = sqrt((ag_var_piv / h2_piv - ag_var_piv)))
    
    # phenotypic variation - pivotal temperatures
    Delta_piv <- rnorm(n = years, 
                       mean = 0, 
                       sd = sqrt(ag_var_piv / h2_piv))
    
  } else {
    
    # pivotal temperatures without evolution
    Pivotal_temps <- rep(T_piv, times = years)
    
    # no Delta, G, P, epsilon,  or gamma vectors
    Delta_piv         <- NULL
    G_piv             <- NULL
    P_piv             <- NULL
    Epsilon_piv       <- NULL 
    Gamma_piv         <- NULL
    
  }
  
  ##### threshold temperatures ###################################################  
  
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
    Threshold_temps <- rep(NA, times = years)
    Threshold_temps[1] <- P_threshold[1] + rnorm(n = 1, 
                                                 mean = 0, 
                                                 sd = sqrt(ag_var_threshold / 
                                                             h2_threshold))
    
    # gamma, error term for the expected genotype
    Gamma_threshold <- rnorm(n = years, 
                             mean = 0, 
                             sd = sqrt(ag_var_threshold / 2))       
    
    # epsilon, error term for the expected threshold temperature
    Epsilon_threshold <- rnorm(n = years, 
                               mean = 0, 
                               sd = sqrt((ag_var_threshold / 
                                            h2_threshold - 
                                            ag_var_threshold)))
    # phenotypic variation - threshold temperatures
    Delta_threshold <- rnorm(n = years, 
                             mean = 0, 
                             sd = sqrt(ag_var_threshold / h2_threshold))
    
  } else {
    
    # threshold temperatures without evolution
    Threshold_temps <- rep(T_threshold, times = years)
    
    # no G, P, epsilon, gamma, or delta vectors
    G_threshold             <- NULL
    P_threshold             <- NULL
    Epsilon_threshold       <- NULL 
    Gamma_threshold         <- NULL
    Delta_threshold         <- NULL
    
  }
  
  ##### OSR ####################################################################
  
  # OSR vector - 1 for each year
  OSRs <- rep(NA, times = years)
  
  # breeding females this year
  # breeding males this year
  breeding_F <- rbinom(n = max_age, 
                       size = F_Mature_init, 
                       prob = 1 / F_remigration_int)  
  
  n_breeding_F <- sum(as.numeric(breeding_F, na.rm = TRUE))
  
  # breeding males this year
  breeding_M <- rbinom(n = max_age, 
                       size = M_Mature_init, 
                       prob = 1 / M_remigration_int)
  
  n_breeding_M <- sum(as.numeric(breeding_M, na.rm = TRUE))
  
  # as long as there is at least one mature female and one mature male:
  if (n_breeding_F > 0.5 & n_breeding_M > 0.5) {
    
    # operational sex ratio - proportion of males
    OSRs[1] <- n_breeding_M / (n_breeding_M + n_breeding_F)
    
  }
  
  if (conservation == TRUE) {
    
    conservation_years <- seq(from = 1, by = frequency, length = years)
    
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
