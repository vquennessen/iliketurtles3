# base model

base_model <- function(max_age, F_survival_years, F_survival_values, 
                       M_survival_years, M_survival_values, age_maturity, 
                       betas, remigration_int, nests_mu, nests_sd, 
                       eggs_mu, eggs_sd, hatch_success_mu, hatch_success_a, 
                       hatch_success_b, hatch_success_stochasticity, 
                       logit_a, logit_b, temp_mu, temp_sd, 
                       climate_stochasticity, start_year, end_year, scenarios) {
  
  ##### source initialized arrays ##############################################
  
  init_output <- initialize_arrays(max_age, start_year, end_year, scenarios, 
                                   betas, hatch_success_stochasticity, 
                                   hatch_success_a, hatch_success_b, 
                                   hatch_success_mu, 
                                   F_survival_years, F_survival_values, 
                                   M_survival_years, M_survival_values, 
                                   temp_mu, climate_stochasticity, 
                                   logit_a, logit_b)
  
  A <- init_output[[1]]               # number of ages
  Y <- init_output[[2]]               # number of years
  S <- init_output[[3]]               # number of scenarios
  B <- init_output[[4]]               # number of beta values
  years <- init_output[[5]]           # years to run model
  hatch_success <- init_output[[6]]   # array of hatching success values
  temperatures <- init_output[[7]]    # temperatures across climate scenarios
  N <- init_output[[8]]               # population size array
  F_survival <- init_output[[9]]      # vector of survival values - females
  M_survival <- init_output[[10]]     # vector of survival values - males
  f_Leslie <- init_output[[11]]       # female Leslie matrix
  m_Leslie <- init_output[[12]]       # male Leslie matrix
  
  ##### model ##################################################################
  for (b in 1:B) {
  
  for (s in 1:length(scenarios)) {
    
   for (y in 2:Y) {
    
    # population dynamics
    # survival for each age 
    
    # annual survival - females
    N[1, , y, s, b] <- floor(f_Leslie %*% N[1, , y - 1, s, b])
    
    # annual survival - males
    N[2, , y, s, b] <- floor(m_Leslie %*% N[2, , y - 1, s, b])
    
    # break out of loop if there are zero males or females at any age
    if (sum(N[1, , y, s, b]) < 1 || sum(N[2, , y, s, b]) < 1) { break }
    
    # climate change temperature estimates
    temp <- temperatures[y, s]
    
    # reproduction
    rep_output <- reproduction(N, age_maturity, max_age, remigration_int, 
                               beta, nests_mu, nests_sd, eggs_mu, eggs_sd, 
                               hatch_success = hatch_success[y], 
                               climate_stochasticity, temp, temp_sd, 
                               logit_a, logit_b, y)
    
    # add recruits to population size array
    N[1, 1, y, s, b] <- rep_output[[1]]
    N[2, 1, y, s, b] <- rep_output[[2]]

   }
    
  }
    
  }
  
  ##### output #################################################################
  
  # create abundance array
  abundance <- colSums(N, dims = 3)
  mature_abundance <- colSums(N[, age_maturity:max_age, , ], dims = 3)
  
  # output N and abundance arrays
  output <- list(N, abundance, mature_abundance)
  
  return(output)
  
}




