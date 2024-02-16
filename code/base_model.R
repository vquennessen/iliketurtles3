# base model

base_model <- function(start_year, end_year, scenario, beta,
                       max_age, F_survival, M_survival, F_init, M_init, 
                       M, F_remigration_int, M_remigration_int,
                       nests_mu, nests_sd, eggs_mu, eggs_sd, 
                       hatch_success_A, hatch_success_k, 
                       hatch_success_t0, T_piv, k, temp_mu, temp_sd, 
                       climate_stochasticity, demographic_stochasticity) {
  
  ##### source initialized arrays ##############################################
  
  init_output <- initialize_arrays(start_year, end_year, scenario, beta,
                         max_age, F_survival, M_survival, F_init, M_init, 
                         M, F_remigration_int, M_remigration_int,
                         nests_mu, nests_sd, eggs_mu, eggs_sd, 
                         hatch_success_A, hatch_success_k, 
                         hatch_success_t0, T_piv, k, temp_mu, temp_sd, 
                         climate_stochasticity)
  
  A             <- init_output[[1]]    # number of ages
  Y             <- init_output[[2]]    # number of years
  years         <- init_output[[3]]    # years to run model
  temperatures  <- init_output[[4]]    # temperatures across climate scenarios
  N             <- init_output[[5]]    # population size array
  
  ##### model ##################################################################
  for (y in 2:Y) {
    
    # population dynamics
    # survival for each age 
    N <- pop_dynamics(N, max_age, y, F_survival, M_survival)
      
    # break out of loop if there are zero males or females at any age
    if (sum(N[1, , y], na.rm = TRUE) < 1 || sum(N[2, , y], na.rm = TRUE) < 1) { 
      break }
    
    # climate change temperature estimates
    temp <- temperatures[y]
    
    # if temp is over 35, then hatching success is zero, so there's no age 1
    if (temp > 35) { 
      N[1, 1, y] <- 0
      N[2, 1, y] <- 0 }
    
    else {
      
      # reproduction
      rep_output <- reproduction(N, y, beta, max_age, M, 
                                 F_remigration_int, M_remigration_int,
                                 nests_mu, nests_sd, eggs_mu, eggs_sd, 
                                 hatch_success_A, hatch_success_k, 
                                 hatch_success_t0, temp, temp_sd, T_piv, k, 
                                 climate_stochasticity)
      
      # add recruits to population size array
      N[1, 1, y] <- rep_output[[1]]
      N[2, 1, y] <- rep_output[[2]]
      
    }
    
  }
  
  ##### output #################################################################
  
  # create abundance array
  abundance_F <- colSums(N[1, , ], dims = 1)
  abundance_M <- colSums(N[2, , ], dims = 1)
  abundance_total <- colSums(N, dims = 2)
  mature_abundance <- colSums(N[, age_maturity:max_age, ], dims = 2)
  
  # output N and abundance arrays
  output <- list(N, abundance_F, abundance_M, abundance_total, mature_abundance)
  
  return(output)
  
}
