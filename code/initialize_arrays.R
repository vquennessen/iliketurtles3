# initialize arrays

initialize_arrays <- function(scenario, years, A, Y, F_init, M_init, 
                              M, T_piv, k, H, phen_var, evolution,
                              temp_mu, temp_sd, climate_stochasticity) {
  
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
  
  # genetics for pivotal temperature
  G <- rep(T_piv, A)
  
  # if evolution is turned on, create epsilon and delta vectors
  if (evolution == TRUE) {
    
    # distribution of G
    G <- rnorm(n = A, mean = T_piv, sd = sqrt(H*phen_var))
    
    # gamma, error term for the expected genotype
    Gamma <- rnorm(n = Y, mean = 0, sd = sqrt((H*phen_var) / 2))       
    
    # epsilon, error term for the expected pivotal temperature
    Epsilon <- rnorm(n = Y, mean = 0, sd = sqrt((phen_var*(1 - H))))
    
  } else { 
    
    epsilon <- NULL 
    gamma <- NULL 
    
    }
  
  # output
  output <- list(temperatures, N, G, Gamma, Epsilon)
  
  return(output)
  
}