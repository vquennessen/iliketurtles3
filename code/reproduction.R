# reproduction

reproduction <- function(N, y, beta, max_age, M, 
                         F_remigration_int, M_remigration_int,
                         nests_mu, nests_sd, eggs_mu, eggs_sd, 
                         hatch_success_A, hatch_success_k, 
                         hatch_success_t0, G, H, ag_var, Delta, 
                         Gamma, Epsilon, temp, temp_sd, T_piv, k, 
                         evolution, climate_stochasticity, Pivotal_temps) {
  
  # calculate number of breeding adults
  # females only breed every F_remigration_int years
  n_breeding_F <- sum((N[1, , y - 1] * M), na.rm = TRUE) / F_remigration_int
  
  # males only breed every M_remigration_int years
  n_breeding_M <- sum((N[2, , y - 1] * M), na.rm = TRUE) / M_remigration_int  
  
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
      breeding_success <- pbeta(2 * OSR, shape1 = 1, shape2 = beta) 
      
      # if OSR > 0.5 all the females get to mate
    } else { breeding_success <- 1 }
    
    # number of nests per female
    nests <- rnorm(n = round(n_breeding_F), mean = nests_mu, sd = nests_sd)
    
    # replace any number < 1 with +1
    nests[which(nests < 1)] <- 1
    
    # initialize eggs vector
    eggs <- rep(NA, times = length(nests))
    
    # number of eggs per nest
    for (f in 1:length(nests)) {
      
      eggs[f] <- sum(rnorm(n = nests[f], mean = eggs_mu, sd = eggs_sd), 
                     na.rm = TRUE)
      
    }
    
    # hatching success
    hatch_success <- hatch_success_A / (1 + exp(-hatch_success_k * (temp - hatch_success_t0)))
    
    # total hatchlings = breeding success * total eggs * hatching success
    hatchlings <- breeding_success * sum(eggs, na.rm = TRUE) * hatch_success
    
    # if fewer than 1 hatchling (rounded), no hatchlings for this cohort
    if (hatchlings < 0.5) { 
      
      female_hatchlings <- 0
      male_hatchlings <- 0  
      
      # if there are more than 0.5 hatchlings
    } else { 
      
      # evolution
      if (evolution == TRUE) {
        
        # weighted average of genotypes for mature males from last year
        GM <- weighted.mean(x = G, w = N[2, , y - 1] * M)
        
        # weighted average of genotypes for mature females from last year
        GF <- weighted.mean(x = G, w = N[1, , y - 1] * M)
        
        # hatchling genotype, with genotypic variance
        Pivotal_temps[y] <- (GM + GF) / 2 + Gamma[y]
        
        # determine proportion of male hatchlings based on temperature + genetics
        prop_male <- 1/(1 + exp(-k*(temp - (Pivotal_temps[y] + Epsilon[y]))))
        
        # if evolution == FALSE
      } else {
        
        # determine proportion of male hatchlings based on temperature and 
        # phenotypic variation
        prop_male <- 1/(1 + exp(-k * (temp - (T_piv + Delta[y]))))
        
        # don't track pivotal temperatures
        Pivotal_temps <- NULL
        
      }
      
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
  output <- list(female_hatchlings, male_hatchlings, Pivotal_temps, OSR)
  
  return(output)
  
}
