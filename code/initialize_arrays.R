# initialize arrays

initialize_arrays <- function(scenario, years, A, Y, F_init, M_init, M, 
                              F_remigration_int, M_remigration_int, 
                              T_piv, k, H_piv, ag_var_piv, evolution_piv,
                              temp_mu, temp_sd, climate_stochasticity) {
  
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
  G_piv <- rep(T_piv, A)
  
  # delta vector for just phenotypic variation
  Delta_piv <- rnorm(n = Y, mean = 0, sd = sqrt(ag_var_piv / H_piv))
  
  # if evolution_piv is turned on, create epsilon and delta vectors
  if (evolution_piv == TRUE) {
    
    # distribution of G - starting genotypes
    G_piv <- rnorm(n = A, mean = T_piv, sd = sqrt(ag_var_piv))
    
    # gamma, error term for the expected genotype
    Gamma_piv <- rnorm(n = Y, mean = 0, sd = sqrt(ag_var_piv / 2))       
    
    # epsilon, error term for the expected pivotal temperature
    Epsilon_piv <- rnorm(n = Y, 
                         mean = 0, 
                         sd = sqrt((ag_var_piv / H_piv - ag_var_piv)))
    
    # intialize pivotal temperatures vector
    Pivotal_temps <- c(G_piv[1], rep(NA, times = Y - 1))
    
  } else { 
    
    Epsilon_piv <- NULL 
    Gamma_piv <- NULL 
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
  output <- list(temperatures, N, 
                 G_piv, Delta_piv, Gamma_piv, Epsilon_piv, Pivotal_temps, 
                 OSR)
  
  return(output)
  
}