# reproduction

reproduction <- function(N, M, y, beta, max_age,
                         F_remigration_int, M_remigration_int,
                         nests_mu, nests_sd, eggs_mu, eggs_sd, 
                         hatch_success_A, hatch_success_k, 
                         hatch_success_t0, temperatures,
                         k_piv, Pivotal_temps, Threshold_temps) {
  
  # calculate number of breeding adults
  # females only breed every F_remigration_int years
  n_breeding_F <- round(sum(round(N[1, , y - 1] * M), na.rm = TRUE) / F_remigration_int)
  
  # males only breed every M_remigration_int years
  n_breeding_M <- round(sum(round(N[2, , y - 1] * M), na.rm = TRUE) / M_remigration_int)  
  
  if (n_breeding_F > 0.5 & n_breeding_M > 0.5) {
    
    # operational sex ratio - proportion of males
    # multiply by 2 to transform to beta function with x from 0 to 0.5 instead 
    # of 0 to 1
    OSR <- n_breeding_M / (n_breeding_M + n_breeding_F)
    
    # calculate reproductive success
    # if 50% males or fewer, use beta function to calculate breeding success
    # multiply OSR by 2 to transform to beta function with x from 0 to 0.5 
    # instead of 0 to 1
    if (OSR <= 0.5) {
      breeding_success <- pbeta(2 * OSR, 
                                shape1 = 1, 
                                shape2 = beta) 
      
      # if OSR > 0.5 all the females get to mate
    } else { breeding_success <- 1 }
    
    # number of nests per female (round to nearest integer)
    nests <- round(rnorm(n = round(n_breeding_F), 
                         mean = nests_mu, 
                         sd = nests_sd))
    
    # calculate nest temperatures
    
    # if we're including climate stochasticity in the model
    if (climate_stochasticity == TRUE) {

      white_noise <- rnorm(n = sum(nests, na.rm = TRUE), mean = 0, sd = temp_sd)

      if (noise == 'White') {

        # generate stochastic temperatures from means given temp_sd
        temperatures <- temp_mus + white_noise

      }

      if (noise == 'Red') {

        # initialize deviations
        deviations <- rep(NA, times = length(nests))

        # first deviation term
        deviations[1] <- rnorm(n = 1, mean = 0, sd = temp_sd) + white_noise[1]

        # autocorrelated deviation series
        for (i in 2:years) {

          deviations[i] <- AC * deviations[i - 1] + white_noise[i]

        }

        temperatures <- temp_mus + deviations

      }

      # if no climate stochasticity, the temperatures are just the means
    } else { temperatures <- temp_mus }
    
    # replace any number < 1 with +1
    nests[which(nests < 1)] <- 1
    
    # initialize eggs vector
    eggs <- rep(NA, times = length(nests))
    
    # number of eggs per n_breeding_F (round to nearest integer)
    for (f in 1:length(nests)) {
      
      eggs[f] <- sum(round(rnorm(n = nests[f], 
                                 mean = eggs_mu, 
                                 sd = eggs_sd)), 
                     na.rm = TRUE)
      
    }
    
    # hatching success
    hatch_success <- hatch_success_A / (1 + exp(-hatch_success_k * (temperatures[y] - hatch_success_t0)))
    
    # total hatchlings = breeding success * total eggs * hatching success
    # (round to nearest integer)
    hatchlings <- round(breeding_success * sum(eggs, na.rm = TRUE) * hatch_success)
    
    # if fewer than 1 hatchling (rounded), no hatchlings for this cohort
    if (hatchlings < 1) { 
      
      female_hatchlings <- 0
      male_hatchlings <- 0  
      
      # if there are more than 0.5 hatchlings
    } else { 
      
      # determine proportion of male hatchlings based on temperature and 
      # phenotypic variation
      prop_male <- 1/(1 + exp(-k_piv * (temperatures[y] - (Pivotal_temps[y]))))
      
      # number of male and female hatchlings
      female_hatchlings <- round(hatchlings * (1 - prop_male))
      male_hatchlings <- round(hatchlings * prop_male)
      
    } 
    
    # if there are no mature females or no mature males
  } else {     
    
    OSR <- NA
    female_hatchlings <- 0
    male_hatchlings <- 0  
    
  }
  
  # output
  output <- list(female_hatchlings, male_hatchlings, OSR)
  
  return(output)
  
}
