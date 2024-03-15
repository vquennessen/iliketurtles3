# reproduction

reproduction <- function(N, y, beta, max_age, M, 
                         F_remigration_int, M_remigration_int,
                         nests_mu, nests_sd, eggs_mu, eggs_sd, 
                         hatch_success_A, hatch_success_k, 
                         hatch_success_t0, G, H, ag_var, 
                         Gamma, Epsilon, temp, temp_sd, T_piv, k, 
                         evolution, climate_stochasticity) {
  
  # calculate number of breeding adults
  # females only breed every F_remigration_int years
  n_breeding_F <- sum((N[1, 1:max_age, y - 1]*M), 
                      na.rm = TRUE) / F_remigration_int
  
  # males only breed every M_remigration_int years
  n_breeding_M <- sum((N[2, 1:max_age, y - 1]*M), 
                      na.rm = TRUE) / M_remigration_int  
  
  if (n_breeding_F > 0.5 & n_breeding_M > 0.5) {
    
    # proportion of males
    # multiply by 2 to get BSR from 0 to 1 instead of 0 to 0.5
    BSR <- 2*(n_breeding_M / (n_breeding_M + n_breeding_F))
    
    # calculate reproductive success
    breeding_success <- pbeta(BSR, shape1 = 1, shape2 = beta)
    
    # number of nests per female
    nests <- rnorm(n = round(n_breeding_F), mean = nests_mu, sd = nests_sd)
    
    # replace any zeros or -1 with +1
    nests[which(nests < 1)] <- 1
    
    # initialize eggs vector
    eggs <- rep(NA, times = length(nests))
    
    # number of eggs per nest
    for (f in 1:length(nests)) {
      
      eggs[f] <- sum(rnorm(n = nests[f], mean = eggs_mu, sd = eggs_sd), 
                     na.rm = TRUE)
      
    }
    
    # hatching success
    hatch_success <- hatch_success_A/(1 + exp(-hatch_success_k*(temp - hatch_success_t0)))
    
    # total hatchlings = total eggs * hatching success * breeding_success
    hatchlings <- sum(eggs, na.rm = TRUE) * hatch_success * breeding_success
    
    # for current temperature 
    if (climate_stochasticity == TRUE) {
      temperature <- rnorm(n = 1, mean = temp, sd = temp_sd)
    } else {
      temperature <- temp
    }
    
    # evolution
    if (evolution == TRUE) {
      
      # weighted average of genotypes for males
      GM <- weighted.mean(x = G, w = N[2, , y])
      
      # weighted average of genotypes for females
      GF <- weighted.mean(x = G, w = N[1, , y])
      
      # hatchling genotype
      GH <- (GM + GF) / 2 + Gamma[y]
      
      # determine proportion of male hatchlings based on temperature + genetics
      prop_male <- 1/(1 + exp(-k*(temperature - GH + Epsilon[y])))
      
      
    } else {
      
      # determine proportion of male hatchlings based on temperature
      prop_male <- 1/(1 + exp(-k*(temperature-T_piv)))
      
    }
    
    # number of male and female hatchlings
    female_hatchlings <- round(hatchlings * (1 - prop_male))
    male_hatchlings <- round(hatchlings * prop_male)
    
  } else { 
    
    female_hatchlings <- 0
    male_hatchlings <- 0 
    
  }
  
  # output
  output <- list(female_hatchlings, male_hatchlings)
  
  return(output)
  
}
