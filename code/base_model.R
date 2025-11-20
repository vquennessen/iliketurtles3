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
                                   evolution, trait, h2, varGenetic, max_N,
                                   conservation_action, frequency)
  
  N                  <- init_output[[1]]   # population size array
  season_temp_mus    <- init_output[[2]]   # mean temps at the season level
  OSRs               <- init_output[[3]]   # operational sex ratio
  varSegregation     <- init_output[[4]]   # evolution segregation variance
  varPhenotypic      <- init_output[[5]]   # evolution phenotypic variance
  G                  <- init_output[[6]]   # genotypes
  G_stats            <- init_output[[7]]   # genotype stats, to keep
  P                  <- init_output[[8]]   # phenotypes
  P_stats            <- init_output[[9]]   # phenotype stats, to keep  
  conservation_years <- init_output[[10]]  # years for conservation action
  
  ##### model ##################################################################
  # set.seed(seed)
  for (y in 2:yrs) {
    
    # population dynamics
    # survival for each age 
    # set.seed(seed)
    popdy_output <- pop_dynamics(N, max_age, y, M,
                      IF_survival, IM_survival, MF_survival, MM_survival, 
                      evolution, G, G_stats, P, P_stats)
    
    N       <- popdy_output[[1]]
    G       <- popdy_output[[2]]
    G_stats <- popdy_output[[3]]
    P       <- popdy_output[[4]]
    P_stats <- popdy_output[[5]]
    
    # reproduction
    # set.seed(seed)
    rep_output <- reproduction(N, M, y, beta, max_age, 
                               F_remigration_interval, M_remigration_interval,
                               clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                               emergence_success_A, emergence_success_k, 
                               emergence_success_t0, 
                               season_temp_mus, clutch_temp_sd,
                               k_piv, T_piv, T_threshold, evolution, 
                               trait, varSegregation, varPhenotypic, 
                               G, G_stats, P, P_stats,  
                               conservation_action, conservation_years, 
                               intensity, effect_size)
    
    # add recruits to population size array
    N[1, 1, y] <- rep_output[[1]]
    N[2, 1, y] <- rep_output[[2]]
    OSRs[y]    <- rep_output[[3]]
    G          <- rep_output[[4]]
    G_stats    <- rep_output[[5]]
    P          <- rep_output[[6]]
    P_stats    <- rep_output[[7]]
    
    # break out of loop if there are zero males or females at any age
    if (sum(N[1, , y], 
            na.rm = TRUE) < 0.5 || sum(N[2, , y], 
                                       na.rm = TRUE) < 0.5) {
      break }
  }
  
  ##### output #################################################################

  output <- list(N, 
                 OSRs,
                 G_stats, 
                 P_stats)
  
  return(output)
  
}
