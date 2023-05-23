# initialize arrays

initialize_arrays <- function(start_year, end_year, A, Y, 
                              hatch_success_stochasticity, hatch_success_a, 
                              hatch_success_b, hatch_success_mu, scenarios, 
                              F_survival_years, F_survival_values, 
                              M_survival_years, M_survival_values, 
                              temp_mu, climate_stochasticity, 
                              logit_a, logit_b) {
  
  # years
  years <- seq(from = start_year, to = end_year)
  
  # initialize population size array
  # dimensions = sexes * ages * scenarios * years
  N <- array(rep(0, times = 2 * A * length(scenarios) * Y), 
                  dim = c(2, A, length(scenarios), Y))  
  
  # initialize hatching success
  # determine hatching success
  if (hatch_success_stochasticity == TRUE) {
    hatch_success <- rbeta(n = Y, 
                           shape1 = hatch_success_a, 
                           shape2 = hatch_success_b)
  } else {
    hatch_success <- rep(hatch_success_mu, times = Y)
  }
  
  # initialize temperature scenarios
  temperatures <- array(rep(NA, times = length(scenarios)*Y), 
                        dim = c(length(scenarios), Y))
  
  # for each scenario
  for (i in 1:length(scenarios)) {
    
    # generate mean temperature values that go up linearly 
    temp_mus <- seq(from = temp_mu, to = temp_mu + scenarios[i], length = Y)
    
    # if we're including climate stochasticity in the model
    if (climate_stochasticity == TRUE) {
      
      # generate stochastic temperatures from means given temp_sd
      temperatures[i, ] <- rnorm(n = Y, mean = temp_mus, sd = temp_sd)
      
      # if not, the temperatures are just the means
    } else { temperatures[i, ] <- temp_mus }
    
  }
  
  # survival values vector - females
  F_survival <- rep(F_survival_values, times = F_survival_years)
  
  # survival values vector - males
  M_survival <- rep(M_survival_values, times = M_survival_years)
  
  # check it's long enough, and if not, add the last survival_value until it is
  # females
  if (length(F_survival) < A) {
    F_survival <- c(F_survival, rep(F_survival_values[length(F_survival_values)], 
                                    A - length(F_survival)))
  }
  
  # males
  if (length(M_survival) < A) {
    M_survival <- c(M_survival, rep(M_survival_values[length(M_survival_values)], 
                                    A - length(M_survival)))
  }
  
  # make female leslie matrix for survival
  f_matrix <- matrix(diag(F_survival[1:(A - 1)]), ncol = A - 1)
  f_Leslie <- rbind(rep(0, A), cbind(f_matrix, rep(0, A - 1)))
  
  # make male leslie matrix for survival
  m_matrix <- matrix(diag(M_survival[1:(A - 1)]), ncol = A - 1)
  m_Leslie <- rbind(rep(0, A), cbind(m_matrix, rep(0, A - 1)))
  #
  # # initialize population size array by age class and sex
  # N_output <- initialize_population(ages, no_betas, betas, no_scenarios, 
  #                                   scenarios, a, temp_mu, age_maturity, 
  #                                   burn_in = 150, max_age, remigration_int, 
  #                                   nests_mu, nests_sd, eggs_mu, eggs_sd,
  #                                   hatch_success = hatch_success_mu, 
  #                                   f_Leslie, m_Leslie)
  
  init_N <- initialize_population2(temp_mu, logit_a, logit_b, A, Y, 
                                   F_survival, M_survival)
  
  N <- 
  
  # output
  output <- list(years, hatch_success, temperatures, N, 
                 F_survival, M_survival, f_Leslie, m_Leslie)
  
  return(output)
  
}