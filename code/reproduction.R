# reproduction

reproduction <- function(N, M, y, beta, max_age,
                         F_remigration_int, M_remigration_int,
                         clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                         hatch_success_A, hatch_success_k, 
                         hatch_success_t0, 
                         season_temp_mus, clutch_temp_sd,
                         k_piv, Pivotal_temps, Threshold_temps) {
  
  # calculate number of breeding adults
  # females only breed every F_remigration_int years
  n_breeding_F <- round(sum(round(N[1, , y - 1] * M), na.rm = TRUE) / F_remigration_int)
  
  # males only breed every M_remigration_int years
  n_breeding_M <- round(sum(round(N[2, , y - 1] * M), na.rm = TRUE) / M_remigration_int)  
  
  if (n_breeding_F > 0 & n_breeding_M > 0) {
    
    # operational sex ratio - proportion of males
    # multiply by 2 to transform to beta function with x from 0 to 0.5 instead 
    # of 0 to 1
    OSR <- n_breeding_M / (n_breeding_M + n_breeding_F)
    
    # calculate reproductive success
    # if 50% males or fewer, use beta function to calculate breeding success
    # multiply OSR by 2 to transform to beta function with x from 0 to 0.5 
    # instead of 0 to 1
    if (OSR < 0.5) {
      breeding_success <- pbeta(2 * OSR, 
                                shape1 = 1, 
                                shape2 = beta) 
      
      # if OSR > 0.5 all the females get to mate
    } else { breeding_success <- 1 }
    
    # number of clutches per female (round to nearest integer)
    clutches <- round(rnorm(n = round(n_breeding_F), 
                            mean = clutches_mu, 
                            sd = clutches_sd))
    
    # replace any number < 1 with +1
    clutches[which(clutches < 1)] <- 1
    
    # potential eggs vector, one number for each clutch
    potential_eggs <- round(rnorm(n = sum(clutches), 
                                  mean = eggs_mu, 
                                  sd = eggs_sd))
    
    # clutch temperature vector, one number for each clutch 
    clutch_temps <-rnorm(n = sum(clutches), 
                         mean = season_temp_mus[y], 
                         sd = clutch_temp_sd)
    
    # hatching successes vector, one number for each clutch 
    hatch_successes <- hatch_success_A / (1 + exp(-hatch_success_k * (clutch_temps - hatch_success_t0)))   
    
    # proportions male vector, one number for each clutch 
    props_male <- 1/(1 + exp(-k_piv * (clutch_temps - (Pivotal_temps[y]))))
    
    # numbers of hatchlings vector, one number for each clutch 
    hatchlings <- round(breeding_success * potential_eggs * hatch_successes)
    
    # male and female hatchlings summed across all clutches
    female_hatchlings <- sum(round(hatchlings * (1 - props_male)))
    male_hatchlings <- sum(round(hatchlings * props_male))    
    
  } else {     
    
    OSR <- NA
    female_hatchlings <- 0
    male_hatchlings <- 0  
    
  }
  
  # output
  output <- list(female_hatchlings, male_hatchlings, OSR)
  
  return(output)
  
}
