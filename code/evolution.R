evolution <- function(G, 
                      P,
                      n_breeding_F, 
                      n_available_M, 
                      male_probs, 
                      contributions, 
                      trait,
                      clutches, 
                      eggs,
                      clutch_temps, 
                      emergence_success_A, 
                      emergence_success_k, 
                      emergence_success_t0, 
                      T_threshold, 
                      k_piv, 
                      T_piv
) {
  
  # extract maternal genotypes
  GM <- sample(unlist(G[3, ])[!is.na(unlist(G[3, ]))], 
               size = n_breeding_F)
  
  # extract potential paternal genotypes
  potential_GP <- sample(unlist(G[4, 2:max_age])[!is.na(unlist(G[4, 2:max_age]))], 
                         size = n_available_M)        
  
  # how many males does each female mate with
  nMales <- sample(1:length(male_probs), 
                   size = n_breeding_F, 
                   prob = male_probs, 
                   replace = TRUE)
  
  # if there are more males assigned to a female than there are available, reduce
  # it with the maximum number of males available
  nMales[nMales > n_available_M] <- n_available_M
  
  # assign male genotypes to each female
  GP <- map(nMales, ~ sample(potential_GP, size = .x))
  
  # calculate offspring genotypes and phenotypes
  G_females <- list()
  G_males <- list()
  P_females <- list()
  P_males <- list()
  
  for (i in 1:n_breeding_F) {
    
    # for each clutch, assign maternal genotypes to offspring
    GM_eggs <- lapply(eggs[[i]], function(x) rep(GM[[i]], times = x))
    
    # for each clutch, assign paternal genotypes to offspring
    if (nMales[[i]] == 1) {
      
      # paternal genotypes per egg 
      GP_eggs <- lapply(eggs[[i]], function(x) rep(GP[[i]], times = x))
      
    } else {
      
      GP_eggs <- lapply(eggs[[i]], 
                        function(x) {
                          sample(GP[[i]], 
                                 size = x, 
                                 prob = contributions[[nMales[[i]]]], 
                                 replace = TRUE)
                        }
      )
    }
    
    # calculate offspring genotypes
    G_eggs <- lapply(Map('+', GM_eggs, GP_eggs), function(x) x/2)
    
    # calculate offspring phenotypes
    P_eggs <- lapply(G_eggs, 
                     function(x) rnorm(n = length(x), 
                                       mean = x, 
                                       sd = sqrt(
                                         varGenetic*(1 - h2)/h2
                                       ))
    )
    
    # # for each clutch, assign difference in clutch temperature and trait to offspring
    # temps_diff <- pmap(list(clutch_temps[[i]], 
    #                         eggs[[i]], 
    #                         P_eggs),
    #                    function(x, y, z) { rep(x, times = y) - z 
    #                    }
    # )
    
    if (trait == 'T_piv') {
      
      # list of probability of emergence, one number for each clutch 
      probs_emerged <- lapply(clutch_temps[[i]], 
                              emergence_success, 
                              A = emergence_success_A, 
                              k = emergence_success_k, 
                              t0 = emergence_success_t0, 
                              thermal_limit = T_threshold) %>%
        lapply(pmax, 0)
      
    } else {
      
      probs_emerged <- map2(clutch_temps[[i]], 
                            P_eggs, 
                            ~ emergence_success_A / (
                              1 + exp(-emergence_success_k * (.x - .y))))
      
    }
    
    # which eggs emerge as hatchlings?
    indices_hatchlings <- map2(eggs[[i]], probs_emerged, 
                               function(x, y) {
                                 rbinom(n = x, size = 1, prob = y)
                               })
    
    hatchlings <- unlist(lapply(indices_hatchlings, 
                                sum))
    
    # hatchling genotypes
    G_hatchlings <- map2(G_eggs, 
                         indices_hatchlings, 
                         ~ .x[as.logical(.y)])
    
    # hatchling phenotypes
    P_hatchlings <- map2(P_eggs, 
                         indices_hatchlings, 
                         ~ .x[as.logical(.y)])
    
    if (trait == 'T_piv') {
      
      probs_male <- map2(clutch_temps[[i]], 
                         P_hatchlings, 
                         ~ 1 / (1 + exp(-k_piv * (.x - .y))))
      
    } else {
      
      # list of probability of emergence, one number for each clutch 
      probs_male <- lapply(clutch_temps[[i]], 
                           probability_male, 
                           k = k_piv, 
                           pivotal_temp = T_piv) %>%
        lapply(pmax, 0)
      
    }
    
    # which hatchlings developed as male?
    indices_males <- map2(hatchlings, probs_male, 
                          function(x, y) {
                            rbinom(n = x, size = 1, prob = y)
                          })
    
    G_females[[i]] <- map2(G_hatchlings, indices_males, 
                           ~ .x[-as.logical(.y)])
    G_males[[i]] <- map2(G_hatchlings, indices_males, 
                         ~ .x[as.logical(.y)])
    
    P_females[[i]] <- map2(P_hatchlings, indices_males, 
                           ~ .x[-as.logical(.y)])
    P_males[[i]] <- map2(P_hatchlings, indices_males, 
                         ~ .x[as.logical(.y)])
    
  }
  
  # assign hatchling genotypes
  EGF <- list(unlist(lapply(G_females, unlist)))
  EGM <- list(unlist(lapply(G_males, unlist)))
  
  # assign hatchling phenotypes
  EPF <- list(unlist(lapply(P_females, unlist)))
  EPM <- list(unlist(lapply(P_males, unlist)))
  
  # G stats values
  G_means <- lapply(c(EGF, EGM), function(x) mean(unlist(x), na.rm = TRUE))    
  G_medians <- lapply(c(EGF, EGM), function(x) median(unlist(x), na.rm = TRUE))
  G_variances <- lapply(c(EGF, EGM), function(x) var(unlist(x), na.rm = TRUE))
  
  # P stats values
  P_means <- lapply(c(EPF, EPM), function(x) mean(unlist(x), na.rm = TRUE))    
  P_medians <- lapply(c(EPF, EPM), function(x) median(unlist(x), na.rm = TRUE))
  P_variances <- lapply(c(EPF, EPM), function(x) var(unlist(x), na.rm = TRUE))
  
  EG_stats <- unlist(c(G_means, G_medians, G_variances))
  
  EP_stats <- unlist(c(P_means, P_medians, P_variances))
  
  ##### output #################################################################
  
  output <- list(EGF, EGM, EPF, EPM, EG_stats, EP_stats)
  
  return(output)
  
}
