# base model

base_model <- function(scenario, beta, years, max_age,
                       IF_survival, IM_survival, MF_survival, MM_survival,
                       IF_init, IM_init, MF_init, MM_init,
                       M, F_remigration_int, M_remigration_int,
                       clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                       emergence_success_A, emergence_success_k, 
                       emergence_success_t0, 
                       T_piv, k_piv, h2_piv, ag_var_piv, evolution_piv,
                       T_threshold, h2_threshold, ag_var_threshold, 
                       evolution_threshold,
                       temp_mu, climate_stochasticity, 
                       season_temp_sd, clutch_temp_sd, noise, AC, 
                       conservation_action, frequency, intensity, effect_size) {
  
  ##### source initialized arrays ##############################################
  
  init_output <- initialize_arrays(scenario, years, max_age, 
                                   IF_init, IM_init, MF_init, MM_init,
                                   M, F_remigration_int, M_remigration_int, 
                                   T_piv, k_piv, h2_piv, ag_var_piv, 
                                   evolution_piv,
                                   T_threshold, h2_threshold, ag_var_threshold, 
                                   evolution_threshold,
                                   temp_mu, climate_stochasticity, 
                                   season_temp_sd, clutch_temp_sd, noise, AC, 
                                   conservation_action, frequency)
  
  N                  <- init_output[[1]]    # population size array
  season_temp_mus    <- init_output[[2]]    # mean temps at the season level
  G_piv              <- init_output[[3]]    # genotypes array
  P_piv              <- init_output[[4]]    # expected phenotypes array
  Gamma_piv          <- init_output[[5]]    # error around expected genotype
  Epsilon_piv        <- init_output[[6]]    # error around expected phenotype
  Delta_piv          <- init_output[[7]]    # phenotypic variance
  Pivotal_temps      <- init_output[[8]]    # pivotal temperature
  G_threshold        <- init_output[[9]]    # genotypes array
  P_threshold        <- init_output[[10]]   # expected phenotypes array
  Gamma_threshold    <- init_output[[11]]   # error around expected genotype
  Epsilon_threshold  <- init_output[[12]]   # error around expected phenotype  
  Delta_threshold    <- init_output[[13]]   # phenotypic variance
  Threshold_temps    <- init_output[[14]]   # threshold temperature
  OSRs               <- init_output[[15]]   # operational sex ratio
  conservation_years <- init_output[[16]]   # years for conservation action
  
  ##### model ##################################################################
  for (y in 2:years) {
    
    # population dynamics
    # survival for each age 
    output_pd <- pop_dynamics(N, max_age, y, M,
                              IF_survival, IM_survival, 
                              MF_survival, MM_survival,
                              F_remigration_int, M_remigration_int)
    
    N                    <- output_pd[[1]]
    breeding_F           <- output_pd[[2]]
    breeding_M           <- output_pd[[3]]
    
    # evolution (if applicable)
    if (evolution_piv == TRUE || evolution_threshold == TRUE) {
      
      evol_output <- evolution(N, max_age, y, breeding_F, breeding_M,
                               G_piv, P_piv, Delta_piv, Pivotal_temps,
                               Gamma_piv, Epsilon_piv, evolution_piv, 
                               G_threshold, P_threshold, Delta_threshold, 
                               Threshold_temps, Gamma_threshold, 
                               Epsilon_threshold, evolution_threshold)
      
      Pivotal_temps[y]   <- evol_output[[1]]
      G_piv              <- evol_output[[2]]
      P_piv              <- evol_output[[3]]
      Threshold_temps[y] <- evol_output[[4]]
      G_threshold        <- evol_output[[5]]
      P_threshold        <- evol_output[[6]]
      
    }
    
    # reproduction
    rep_output <- reproduction(N, M, y, beta, max_age, breeding_F, breeding_M,
                               clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                               emergence_success_A, emergence_success_k, 
                               emergence_success_t0, 
                               season_temp_mus, clutch_temp_sd,
                               k_piv, Pivotal_temps, Threshold_temps, 
                               T_threshold, 
                               conservation_action, conservation_years, 
                               intensity, effect_size)
    
    # add recruits to population size array
    N[1, 1, y]          <- rep_output[[1]]
    N[2, 1, y]          <- rep_output[[2]]
    OSRs[y]             <- rep_output[[3]]
    
    # break out of loop if there are zero males or females at any age
    if (sum(N[1, , y], na.rm = TRUE) < 0.5 || sum(N[2, , y], 
                                                  na.rm = TRUE) < 0.5) {
      break }
  }
  
  ##### output #################################################################
  
  if (evolution_piv == FALSE) { Pivotal_temps <- NA }
  
  if (evolution_threshold == FALSE) { Threshold_temps <- NA }
  
  output <- list(N, 
                 OSRs,
                 Pivotal_temps, 
                 Threshold_temps)
  
  return(output)
  
}
