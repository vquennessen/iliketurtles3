# base model

base_model <- function(max_age, age_maturity_mu, age_maturity_sd, 
                       F_survival_years, F_survival_values, 
                       M_survival_years, M_survival_values, 
                       F_remigration_int, M_remigration_int,
                       nests_mu, nests_sd, eggs_mu, eggs_sd, 
                       hatch_success_mu, hatch_success_a, 
                       hatch_success_b, T_piv, k, temp_mu, temp_sd,  
                       start_year, end_year, scenario, beta, 
                       demographic_stochasticity, 
                       climate_stochasticity, 
                       evolution) {
  
  ##### source initialized arrays ##############################################
  
  init_output <- initialize_arrays(max_age, age_maturity_mu, age_maturity_sd, 
                                   F_survival_years, F_survival_values, 
                                   M_survival_years, M_survival_values, 
                                   F_remigration_int, M_remigration_int,
                                   nests_mu, nests_sd, eggs_mu, eggs_sd, 
                                   hatch_success_mu, hatch_success_a, 
                                   hatch_success_b, T_piv, k, temp_mu, temp_sd,  
                                   start_year, end_year, scenario, beta, 
                                   demographic_stochasticity, 
                                   climate_stochasticity, 
                                   evolution)
  
  A             <- init_output[[1]]    # number of ages
  Y             <- init_output[[2]]    # number of years
  years         <- init_output[[3]]    # years to run model
  hatch_success <- init_output[[4]]    # array of hatching success values
  temperatures  <- init_output[[5]]    # temperatures across climate scenarios
  N             <- init_output[[6]]    # population size array
  F_survival    <- init_output[[7]]    # vector of survival values - females
  M_survival    <- init_output[[8]]    # vector of survival values - males
  f_Leslie      <- init_output[[9]]    # female Leslie matrix
  m_Leslie      <- init_output[[10]]   # male Leslie matrix
  
  ##### model ##################################################################
  for (y in 2:Y) {
    
    # population dynamics
    # survival for each age 
    
    if (demographic_stochasticity == TRUE) {
      
      N <- pop_dynamics(N, max_age, y, F_survival, M_survival)
      
    } else {
      
      # annual survival - females
      N[1, , y] <- round(f_Leslie %*% N[1, , y - 1])
      
      # annual survival - males
      N[2, , y] <- round(m_Leslie %*% N[2, , y - 1])
      
    }
    
    # break out of loop if there are zero males or females at any age
    if (sum(N[1, , y]) < 1 || sum(N[2, , y]) < 1) { break }
    
    # climate change temperature estimates
    temp <- temperatures[y]
    
    # if temp is over 35, then hatching success is zero, so there's no repro
    if (temp > 35) { 
      N[1, 1, y] <- 0
      N[2, 1, y] <- 0 }
    
    else {
      
      # reproduction
      rep_output <- reproduction(N, y, beta, max_age, M,
                                 F_remigration_int, M_remigration_int,
                                 nests_mu, nests_sd, eggs_mu, eggs_sd, 
                                 climate_stochasticity, temp, temp_sd, 
                                 T_piv, k)
      
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
