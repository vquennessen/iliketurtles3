# base model

base_model <- function(scenario, beta, yrs, max_age,
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
  # seed <- 1325633
  # set.seed(seed)
  init_output <- initialize_arrays(scenario, yrs, max_age, 
                                   IF_init, IM_init, MF_init, MM_init,
                                   M, F_remigration_int, M_remigration_int, 
                                   T_piv, k_piv, T_threshold, 
                                   temp_mu, climate_stochasticity, 
                                   season_temp_sd, clutch_temp_sd, noise, AC, 
                                   evolution, trait, h2, varGenetic, 
                                   conservation_action, frequency)
  
  N                  <- init_output[[1]]   # population size array
  season_temp_mus    <- init_output[[2]]   # mean temps at the season level
  OSRs               <- init_output[[3]]   # operational sex ratio
  sdSegregation      <- init_output[[4]]   # evolution segregation variance
  sdPhenotypic       <- init_output[[5]]   # evolution phenotypic variance
  G                  <- init_output[[6]]   # genotypes
  P                  <- init_output[[7]]   # phenotypes
  conservation_years <- init_output[[8]]   # years for conservation action
  
  ##### model ##################################################################
  # set.seed(seed)
  for (y in 2:yrs) {
    
    # population dynamics
    # survival for each age 
    # set.seed(seed)
    N <- pop_dynamics(N, max_age, y, M,
                      IF_survival, IM_survival, MF_survival, MM_survival, 
                      G, P)
    
    # # evolution (if applicable)
    # if (evolution_piv == TRUE || evolution_threshold == TRUE) {
    #   
    #   evol_output <- evolution(N, max_age, y, breeding_F, breeding_M,
    #                            G_piv, P_piv, Delta_piv, Pivotal_temps,
    #                            Gamma_piv, Epsilon_piv, evolution_piv, 
    #                            G_threshold, P_threshold, Delta_threshold, 
    #                            Threshold_temps, Gamma_threshold, 
    #                            Epsilon_threshold, evolution_threshold)
    #   
    #   Pivotal_temps[y]   <- evol_output[[1]]
    #   G_piv              <- evol_output[[2]]
    #   P_piv              <- evol_output[[3]]
    #   Threshold_temps[y] <- evol_output[[4]]
    #   G_threshold        <- evol_output[[5]]
    #   P_threshold        <- evol_output[[6]]
    #   
    # }
    
    # reproduction
    # set.seed(seed)
    rep_output <- reproduction(N, M, y, beta, max_age, 
                               F_remigration_interval, M_remigration_interval,
                               clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                               emergence_success_A, emergence_success_k, 
                               emergence_success_t0, 
                               season_temp_mus, clutch_temp_sd,
                               k_piv, T_piv, T_threshold, evolution, 
                               trait, sdSegregation, sdPhenotypic, G, P, 
                               conservation_action, conservation_years, 
                               intensity, effect_size)
    
    # add recruits to population size array
    N[1, 1, y]          <- rep_output[[1]]
    N[2, 1, y]          <- rep_output[[2]]
    OSRs[y]             <- rep_output[[3]]
    G[, , y]            <- rep_output[[4]]
    P[, , y]            <- rep_output[[5]]
    
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
