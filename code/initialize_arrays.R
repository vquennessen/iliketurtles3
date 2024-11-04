# initialize arrays

initialize_arrays <- function(scenario, years, A, Y, F_init, M_init, 
                              M, T_piv, k, H, ag_var, evolution,
                              temp_mu, temp_sd, climate_stochasticity, 
                              F_remigration_int, M_remigration_int) {
  
  # initialize population size array
  # dimensions = sexes * ages  * years
  N <- array(rep(0, times = 2 * A * Y), 
             dim = c(2, A, Y))  
  
  # initial population size
  N[1, , 1] <- round(F_init)
  N[2, , 1] <- round(M_init)
  
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
  
  # delta vector for just phenotypic variation
  Delta <- rnorm(n = Y, mean = 0, sd = sqrt(ag_var / H))
  
  # if evolution is turned on, create epsilon and delta vectors
  if (evolution == TRUE) {
    
    # distribution of G - starting genotypes
    G <- rnorm(n = A, mean = T_piv, sd = sqrt(ag_var))
    
    # gamma, error term for the expected genotype
    Gamma <- rnorm(n = Y, mean = 0, sd = sqrt(ag_var / 2))       
    
    # epsilon, error term for the expected pivotal temperature
    Epsilon <- rnorm(n = Y, mean = 0, sd = sqrt((ag_var / H - ag_var)))
    
    # intializez pivotal temperatures vector
    Pivotal_temps <- c(G[1], rep(NA, times = Y - 1))
    
  } else { 
    
    Epsilon <- NULL 
    Gamma <- NULL 
    Pivotal_temps <- NULL
    
  }
  
  # OSR vector - dimensions 1 for each year
  OSR <- rep(NA, times = Y)
  
  # first OSR value
  # females only breed every F_remigration_int years
  n_breeding_F <- sum((F_init * M), na.rm = TRUE) / F_remigration_int
  
  # males only breed every M_remigration_int years
  n_breeding_M <- sum((M_init * M), na.rm = TRUE) / M_remigration_int
  
  OSR[1] <- n_breeding_M / (n_breeding_M + n_breeding_F)
  
  # output
  output <- list(temperatures, N, G, Delta, Gamma, Epsilon, Pivotal_temps, OSR)
  
  return(output)
  
}