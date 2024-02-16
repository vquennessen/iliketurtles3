# initialize arrays

initialize_arrays <- function(start_year, end_year, scenario, beta,
                              max_age, F_survival, M_survival, F_init, M_init, 
                              M, F_remigration_int, M_remigration_int,
                              nests_mu, nests_sd, eggs_mu, eggs_sd, 
                              hatch_success_A, hatch_success_k, 
                              hatch_success_t0, T_piv, k, temp_mu, temp_sd, 
                              climate_stochasticity, demographic_stochasticity) {
  
  # years
  years <- seq(from = start_year, to = end_year)
  
  # dimensions
  A <- max_age
  Y <- length(start_year:end_year)
  
  # initialize population size array
  # dimensions = sexes * ages  * years
  N <- array(rep(0, times = 2 * A * Y), 
             dim = c(2, A, Y))  
  
  # initial population size
  N[1, , 1] <- F_init
  N[2, , 1] <- M_init
  
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
  
  # if evolution is turned on, turn pivotal temperature and TRT into arrays
  
  
  # output
  output <- list(A, Y, years, temperatures, N)
  
  return(output)
  
}