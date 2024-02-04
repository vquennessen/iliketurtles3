# initialize arrays

initialize_arrays <- function(max_age, age_maturity, 
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
  
  # years
  years <- seq(from = start_year, to = end_year)
  
  # dimensions
  A <- max_age
  Y <- length(start_year:end_year)
  
  # initialize population size array
  # dimensions = sexes * ages  * years
  N <- array(rep(0, times = 2 * A * Y), 
             dim = c(2, A, Y))  
  
  # # initialize hatching success
  # # determine hatching success
  # if (hatch_success_stochasticity == TRUE) {
  #   hatch_success <- rbeta(n = Y, 
  #                          shape1 = hatch_success_a, 
  #                          shape2 = hatch_success_b)
  # } else {
  #   hatch_success <- rep(hatch_success_mu, times = Y)
  # }
  
  # initialize temperature scenarios
  temperatures <- rep(NA, times = Y)
  
  # generate mean temperature values that go up linearly 
  temp_mus <- seq(from = temp_mu, to = temp_mu + scenario, length = Y)
  
  # if we're including climate stochasticity in the model
  if (climate_stochasticity == TRUE) {
    
    # generate stochastic temperatures from means given temp_sd
    temperatures <- rnorm(n = Y, mean = temp_mus, sd = temp_sd)
    
    # if not, the temperatures are just the means
  } else { temperatures <- temp_mus }
  
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
    M_survival <- c(M_survival, 
                    rep(M_survival_values[length(M_survival_values)], 
                        A - length(M_survival)))
  }
  
  # make female leslie matrix for survival
  f_matrix <- matrix(diag(F_survival[1:(A - 1)]), ncol = A - 1)
  f_Leslie <- rbind(rep(0, A), cbind(f_matrix, rep(0, A - 1)))
  
  # make male leslie matrix for survival
  m_matrix <- matrix(diag(M_survival[1:(A - 1)]), ncol = A - 1)
  m_Leslie <- rbind(rep(0, A), cbind(m_matrix, rep(0, A - 1)))
  
  # # initialize population size array by age class and sex
  # N <- initialize_population2(temp_mu, logit_a, logit_b, A, Y, 
  #                             F_survival, M_survival)
  
  SAD <- initialize_population(beta, burn_in = 1000, 
                               max_age, age_maturity, 
                               F_remigration_int, M_remigration_int,
                               nests_mu, eggs_mu, hatch_success_mu, 
                               logit_a, logit_b, temp_mu,
                               f_Leslie, m_Leslie)
  
  # separate by sex
  F_SAD <- filter(SAD, Sex == 'Female')
  M_SAD <- filter(SAD, Sex == 'Male')
  
  # set first timestep to SAD times a value to get at least 30 adult males
  # and 170 adult females
  f_min <- F_initial / sum(F_SAD$N[age_maturity:max_age])
  m_min <- M_initial / sum(M_SAD$N[age_maturity:max_age])
  multiplicator <- max(m_min, f_min)
  
  N[1, , 1] <- round(F_SAD$N * multiplicator)
  N[2, , 1] <- round(M_SAD$N * multiplicator)
  
  # if evolution is turned on, turn pivotal temperature and TRT into arrays
  
  
  # output
  output <- list(A, Y, years, hatch_success, temperatures, N, 
                 F_survival, M_survival, f_Leslie, m_Leslie)
  
  return(output)
  
}