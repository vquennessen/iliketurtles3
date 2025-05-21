# base model

base_model <- function(scenario, beta, years, max_age,
                       F_survival, M_survival, F_init, M_init, 
                       M, F_remigration_int, M_remigration_int,
                       clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                       hatch_success_A, hatch_success_k, hatch_success_t0, 
                       T_piv, k_piv, h2_piv, ag_var_piv, evolution_piv, 
                       T_threshold, h2_threshold, ag_var_threshold, 
                       evolution_threshold,
                       temp_mu, climate_stochasticity, 
                       season_temp_sd, clutch_temp_sd, noise, AC) {
  
  ##### source initialized arrays ##############################################
  
  init_output <- initialize_arrays(scenario, years, max_age, F_init, M_init, M, 
                                   F_remigration_int, M_remigration_int, 
                                   T_piv, k_piv, h2_piv, ag_var_piv, 
                                   evolution_piv,
                                   T_threshold, h2_threshold, ag_var_threshold, 
                                   evolution_threshold,
                                   temp_mu, climate_stochasticity, 
                                   season_temp_sd, clutch_temp_sd, noise, AC)
  
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
  OSR                <- init_output[[15]]   # operational sex ratio
  
  ##### model ##################################################################
  for (y in 2:years) {
    
    # population dynamics
    # survival for each age 
    N <- pop_dynamics(N, max_age, y, F_survival, M_survival)
    
    # evolution (if applicable)
    if (evolution_piv == TRUE || evolution_threshold == TRUE) {
      
      output <- evolution(N, M, max_age, y, 
                          G_piv, P_piv, Delta_piv, Pivotal_temps,
                          Gamma_piv, Epsilon_piv, evolution_piv, 
                          G_threshold, P_threshold, Delta_threshold, 
                          Threshold_temps, Gamma_threshold, Epsilon_threshold, 
                          evolution_threshold)
      
      Pivotal_temps[y]   <- output[[1]]
      G_piv              <- output[[2]]
      P_piv              <- output[[3]]
      Threshold_temps[y] <- output[[4]]
      G_threshold        <- output[[5]]
      P_threshold        <- output[[6]]
      
    }
    
    # # if temp is over threshold temperature, then hatching success is zero,
    # # so there's no age 1, skip reproduction step
    # if (temperatures[y] > Threshold_temps[y]) {
    #   N[1, 1, y] <- 0
    #   N[2, 1, y] <- 0 } else {
    
    # reproduction
    rep_output <- reproduction(N, M, y, beta, max_age,
                               F_remigration_int, M_remigration_int,
                               clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                               hatch_success_A, hatch_success_k, 
                               hatch_success_t0, 
                               season_temp_mus, clutch_temp_sd,
                               k_piv, Pivotal_temps, Threshold_temps)
    
    # add recruits to population size array
    N[1, 1, y]       <- rep_output[[1]]
    N[2, 1, y]       <- rep_output[[2]]
    OSR[y]           <- rep_output[[3]]
  
  # break out of loop if there are zero males or females at any age
  if (sum(N[1, , y], na.rm = TRUE) < 0.5 || sum(N[2, , y], na.rm = TRUE) < 0.5) {
    break }
}

##### output #################################################################

# create abundance array
abundance_F <- colSums(N[1, , ], dims = 1)
abundance_M <- colSums(N[2, , ], dims = 1)
abundance_total <- colSums(N, dims = 2)
abundance_mature <- colSums(round(N[, , ]*M, 2), dims = 2)

# output N and abundance arrays

if (evolution_piv == FALSE) { Pivotal_temps <- NA }

if (evolution_threshold == FALSE) { Threshold_temps <- NA }

output <- list(N, 
               abundance_F, 
               abundance_M, 
               abundance_total, 
               abundance_mature, 
               OSR,
               Pivotal_temps, 
               Threshold_temps)

return(output)

}
