evolution <- function(max_age, G, P, 
                      n_breeding_F, n_available_M, 
                      trait, male_probs, contributions, 
                      h2, varGenetic, varPhenotypic,
                      clutches, eggs, clutch_temps, 
                      emergence_success_A, emergence_success_k, 
                      emergence_success_t0, 
                      T_threshold, k_piv, T_piv) {
  
  # extract maternal genotypes
  GM <- as.list(resample(unlist(G[3, ]), size = n_breeding_F))
  
  # extract potential paternal genotypes
  potential_GP <- resample(unlist(G[4, ]), size = n_available_M)       
  
  # how many males does each female mate with
  nMales <- as.list(resample(1:length(male_probs), 
                   size = n_breeding_F, 
                   prob = male_probs, 
                   replace = TRUE))
  
  # if there are more males assigned to a female than there are available, 
  # reduce it with the maximum number of males available
  nMales[nMales > n_available_M] <- n_available_M
  
  # assign male genotypes to each female
  GP <- map(nMales, ~ resample(potential_GP, size = .x))
  
  ##############################################################################
  # TO DO: make this function work without a for loop ##########################
  ##############################################################################
  
  # lapply(clutches, replicate, 
  #        function(i) {
  #          pmap(list(GP, eggs, nMales, GM), 
  #               function(w, x, y, z) {
  #                 rnorm(n = unlist(x), 
  #                       mean = (z + unlist(resample(unlist(w), 
  #                                                   size = unlist(x), 
  #                                                   prob = unlist(contributions[y]), 
  #                                                   replace = TRUE))) / 2, 
  #                       sd = sqrt(varGenetic/2))
  #               }
  #          )
  #        }
  # )
  
  # GM_eggs <- map2(GM_clutches, eggs, 
  #                 function(x, y) lapply(seq_along(x), 
  #                                       function(i) rep(x[[i]], times = y[i])))
  
  ##############################################################################
  
  # calculate offspring genotypes and phenotypes
  G_females <- list()
  G_males <- list()
  P_females <- list()
  P_males <- list()
  
  # GP_eggs <- pmap(list(eggs, GP, nMales), 
  #                 function(x, y, z) {
  #                   lapply(x, function(w) {
  #                     unlist(resample(resample(unlist(y)), 
  #                                   size = x[w], 
  #                                   prob = contributions[[z]], 
  #                                   replace = TRUE))}
  #                   )
  #                   }
  #                 )

  
  
  for (i in 1:n_breeding_F) {
    
    # for each clutch, assign maternal genotypes to offspring
    GM_eggs <- lapply(eggs[[i]], function(x) rep(GM[[i]], times = x))
    
    GP_eggs <- lapply(eggs[[i]], 
                      function(x) {
                        resample(resample(GP[[i]]), 
                                 size = x, 
                                 prob = contributions[[nMales[[i]]]], 
                                 replace = TRUE)
                      }
    )
    
    # calculate offspring genotypes
    G_eggs <- lapply(Map('+', GM_eggs, GP_eggs), function(x) x/2)
    
    # calculate offspring phenotypes
    P_eggs <- lapply(G_eggs, 
                     function(x) rnorm(n = length(x), 
                                       mean = x, 
                                       sd = sqrt(varGenetic*(1 - h2)/h2))
                     )
    
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
                               ~ as.logical(
                                 rbinom(n = .x, size = 1, prob = .y)))
    
    hatchlings <- unlist(lapply(indices_hatchlings, sum, na.rm = TRUE))
    
    # hatchling genotypes and phenotypes
    G_hatchlings <- map2(G_eggs, indices_hatchlings, ~ .x[as.logical(.y)])
    P_hatchlings <- map2(P_eggs, indices_hatchlings, ~ .x[as.logical(.y)])
    
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
                          ~ as.logical(rbinom(n = .x, size = 1, prob = .y)))
    indices_females <- map(indices_males, ~ as.logical(Map(`-`, 1, .x)))

    
    G_females[[i]] <- map2(G_hatchlings, indices_females, ~ .x[as.logical(.y)])
    G_males[[i]] <- map2(G_hatchlings, indices_males, ~ .x[as.logical(.y)])
    
    P_females[[i]] <- map2(P_hatchlings, indices_females, ~ .x[as.logical(.y)])
    P_males[[i]] <- map2(P_hatchlings, indices_males, ~ .x[as.logical(.y)])
    
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
