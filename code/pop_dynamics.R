# population dynamics with demographic stochasticity

pop_dynamics <- function(N, max_age, y, M,
                         IF_survival, IM_survival, MF_survival, MM_survival, 
                         evolution, G, G_stats, P, P_stats) {
  
  # survival based on binomial distribution with survival rates as probabilities
  # immature females that survived
  survived_immature_F <- rbinom(n = max_age - 1, 
                                size = round(N[1, 1:(max_age - 1), y - 1]), 
                                prob = IF_survival[1:(max_age - 1)])
  
  # immature females that matured
  new_mature_F <- rbinom(n = max_age - 1, 
                         size = survived_immature_F, 
                         prob = M)
  
  # updated immature female population
  N[1, 2:max_age, y] <- as.numeric(survived_immature_F) - as.numeric(new_mature_F)
  
  # mature females that survived
  survived_mature_F <- rbinom(n = max_age - 1, 
                              size = round(N[3, 1:(max_age - 1), y - 1]), 
                              prob = MF_survival)
  
  # updated mature female population
  N[3, 2:max_age, y] <- as.numeric(survived_mature_F) + as.numeric(new_mature_F)
  
  # immature males that survived
  survived_immature_M <- rbinom(n = max_age - 1, 
                                size = round(N[2, 1:(max_age - 1), y - 1]), 
                                prob = IM_survival[1:(max_age - 1)])
  
  # immature males that matured
  new_mature_M <- rbinom(n = max_age - 1, 
                         size = survived_immature_M, 
                         prob = M)
  
  # updated immature male population
  N[2, 2:max_age, y] <- as.numeric(survived_immature_M) - as.numeric(new_mature_M)
  
  # mature males that survived
  survived_mature_M <- rbinom(n = max_age - 1, 
                              size = round(N[4, 1:(max_age - 1), y - 1]), 
                              prob = MM_survival)
  
  # updated mature male population
  N[4, 2:max_age, y] <- as.numeric(survived_mature_M) + as.numeric(new_mature_M)
  
  # initialize G and P arrays
  if (evolution == TRUE) {
    
    # initialize new G arrays
    G_new <- array(rep(NA, 4 * max_age * max_N), 
                   dim = c(4, max_age, max_N))
    
    for (a in 2:max_age) {
      
      # survived immature females
      G_new[1, a, y] <- c(sample(!is.na(G[1, a - 1, ]), 
                                 survived_immature_F[a]), 
                          rep(NA, max_N - survived_immature_F[a]))
      
      # survived immature males
      G_new[2, a, y] <- c(sample(!is.na(G[2, a - 1, ]), 
                                 survived_immature_M[a]), 
                          rep(NA, max_N - survived_immature_M[a]))
      
      # survived mature females
      G_new[3, a, y] <- c(sample(!is.na(G[3, a - 1, ]), 
                                 survived_mature_F[a]), 
                          rep(NA, max_N - survived_mature_F[a]))
      
      # survived mature males
      G_new[4, a, y] <- c(sample(!is.na(G[4, a - 1, ]), 
                                 survived_mature_M[a]), 
                          rep(NA, max_N - survived_mature_M[a]))
      
    }
    
    # new arrays
    G <- G_new
    
    # genotype stats
    G_stats[, , y, 1] <- apply(G, c(1, 2), mean, na.rm = TRUE)
    G_stats[, , y, 2] <- apply(G, c(1, 2), median, na.rm = TRUE)
    G_stats[, , y, 3] <- apply(G, c(1, 2), var, na.rm = TRUE)
    
    # phenotype array, dimensions sex * age * max N value
    P <- rnorm(n = c(4 * max_age * max_N), 
               mean = G, 
               sd = sqrt(varPhenotypic))
    
    # phenotype stats
    P_stats[, , y, 1] <- apply(P, c(1, 2), mean, na.rm = TRUE)
    P_stats[, , y, 2] <- apply(P, c(1, 2), median, na.rm = TRUE)
    P_stats[, , y, 3] <- apply(P, c(1, 2), var, na.rm = TRUE)
    
  } else {
    
    G       <- NULL
    G_stats <- NULL 
    P       <- NULL
    P_stats <- NULL
    
  }
  
  # what objects to return
  output <- list(N, G, G_stats, P, P_stats)
  
  # output
  return(output)
  
}