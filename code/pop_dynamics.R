# population dynamics with demographic stochasticity

pop_dynamics <- function(N, max_age, y, M,
                         IF_survival, IM_survival, MF_survival, MM_survival,
                         evolve, varPhenotypic, G, P, G_stats, P_stats) {
  
  # survival based on binomial distribution with survival rates as probabilities
  # immature females that survived
  survived_immature_F <- rbinom(n = max_age - 1,
                                size = round(N[1, 1:(max_age - 1), y - 1]),
                                prob = IF_survival[1:(max_age - 1)])
  
  # immature females that matured
  new_mature_F <- rbinom(n = max_age - 1, size = survived_immature_F, prob = M)
  
  # updated immature female population
  N[1, , y] <- c(NA, as.numeric(survived_immature_F) - as.numeric(new_mature_F))
  
  # mature females that survived
  survived_mature_F <- rbinom(n = max_age - 1,
                              size = round(N[3, 1:(max_age - 1), y - 1]),
                              prob = MF_survival)
  
  # updated mature female population
  N[3, , y] <- c(0, as.numeric(survived_mature_F) + as.numeric(new_mature_F))
  
  # immature males that survived
  survived_immature_M <- rbinom(n = max_age - 1,
                                size = round(N[2, 1:(max_age - 1), y - 1]),
                                prob = IM_survival[1:(max_age - 1)])
  
  # immature males that matured
  new_mature_M <- rbinom(n = max_age - 1, size = survived_immature_M, prob = M)
  
  # updated immature male population
  N[2, , y] <- c(NA, as.numeric(survived_immature_M) - as.numeric(new_mature_M))
  
  # mature males that survived
  survived_mature_M <- rbinom(n = max_age - 1,
                              size = round(N[4, 1:(max_age - 1), y - 1]),
                              prob = MM_survival)
  
  # updated mature male population
  N[4, , y] <- c(0, as.numeric(survived_mature_M) + as.numeric(new_mature_M))
  
  # initialize G and P arrays
  if (evolve == TRUE) {
    
    # initialize new G arrays
    # G_new <- array(rep(NA, 4 * max_age * max_N),
    #                dim = c(4, max_age, max_N))
    G_new <- asplit(array(rep(NA, 4 * max_age * 2),
                          dim = c(4, max_age, 2)),
                    c(1, 2))
    # G_new <- list()
    
    for (a in 2:max_age) {
      
      # females
      G_old_IF <- G[1, a - 1][[1]][which(!is.na(G[1, a - 1][[1]]))]
      G_old_MF <- G[3, a - 1][[1]][which(!is.na(G[3, a - 1][[1]]))]
      
      size_sIF <- survived_immature_F[a - 1]
      size_nMF <- new_mature_F[a - 1]
      size_sMF <- survived_mature_F[a - 1]
      
      # if(size_sIF > sum(!is.na(G[1, a - 1, ]))) {
      if(size_sIF > sum(!is.na(G[1, a - 1][[1]]))) {
        print(paste('a =', a, '- error! size_sIF > G_old_IF', sep = ' '))
        break }   
      
      # if(size_nMF > sum(!is.na(G[1, a - 1, ]))) {
      if(size_nMF > sum(!is.na(G[1, a - 1][[1]]))) {
        print(paste('a =', a, '- error! size_nMF > G_old_IF', sep = ' '))
        break } 
      
      # if(size_sMF > sum(!is.na(G[3, a - 1, ]))) {
      if(size_sMF > sum(!is.na(G[3, a - 1][[1]]))) {
        print(paste('a =', a, '- error! size_sMF > G_old_MF', sep = ' '))
        break } 
      
      ###### new female genotypes ##############################################
      
      # check to make sure there are immature genotypes to sample from 
      # AND some survived
      if (sum(!is.na(G_old_IF)) > 0 & size_sIF > 0) {
        
        # which immature genotypes survived
        G_new_sIF <- sample(G_old_IF, size = size_sIF)
        
        # if any matured
        if (size_nMF > 0) {
          
          # sample new_sIF for those that matured
          mf_indices <- sample(length(G_new_sIF), size = size_nMF)
          G_new_nMF <- G_new_sIF[mf_indices]
          
          # rest of new_sIF didn't mature
          G_new[1, a] <- list(G_new_sIF[-mf_indices])
          
          # if none matured
        } else { 
          G_new[1, a] <- list(G_new_sIF)
          G_new_nMF <- NULL 
        }
        
        # if there are no old immature genotypes to sample OR none survived, 
        # then there are no new immature OR mature genotypes
      } else {
        G_new[1, a] <- list(NA)
        G_new_nMF <- NULL 
      }
      
      # if some mature genotypes survived AND some immature genotypes matured
      if (size_sMF > 0 & size_nMF > 0) {
        G_new_sMF <- sample(G_old_MF, size = size_sMF)
        G_new[3, a] <- list(c(G_new_nMF, G_new_sMF))   
      }
      
      # if no mature genotypes survived BUT 
      # some immature genotypes survived and matured
      if (!(size_sMF > 0) & size_nMF > 0) {
        G_new[3, a] <- list(G_new_nMF)
      }
      
      # if mature genotypes survived BUT 
      # no immature genotypes survived and matured
      if (size_sMF > 0 & !(size_nMF > 0)) {
        G_new_sMF <- sample(G_old_MF, size = size_sMF)
        G_new[3, a] <- list(G_new_sMF)
      }
      
      # if no mature genotypes survived AND
      # no immature genotypes survived and matured
      if (!(size_sMF > 0) & !(size_nMF > 0)) {
        G_new[3, a] <- list(NA)
      }
      
      ##### new male genotypes #################################################
      
      G_old_IM <- G[2, a - 1][[1]][which(!is.na(G[2, a - 1][[1]]))]
      G_old_MM <- G[4, a - 1][[1]][which(!is.na(G[4, a - 1][[1]]))]
      
      size_sIM <- survived_immature_M[a - 1]
      size_nMM <- new_mature_M[a - 1]
      size_sMM <- survived_mature_M[a - 1]
      
      # if(size_sIM > sum(!is.na(G[2, a - 1, ]))) {
      if(size_sIM > sum(!is.na(G[2, a - 1][[1]]))) {
        print(paste('a =', a, '- error! size_sIM > G_old_IM', sep = ' '))
        break }   
      
      # if(size_nMM > sum(!is.na(G[2, a - 1, ]))) {
      if(size_nMM > sum(!is.na(G[2, a - 1][[1]]))) {
        print(paste('a =', a, '- error! size_nMM > G_old_IM', sep = ' '))
        break } 
      
      # if(size_sMM > sum(!is.na(G[4, a - 1, ]))) {
      if(size_sMM > sum(!is.na(G[4, a - 1][[1]]))) {
        print(paste('a =', a, '- error! size_sMM > G_old_MM', sep = ' '))
        break }      
      
      #. make sure there are immature genotypes to sample from AND some survived
      if (sum(!is.na(G_old_IM)) > 0 & size_sIM > 0) {
        
        # which immature genotypes survived
        G_new_sIM <- sample(G_old_IM, size = size_sIM)
        
        # if any matured
        if (size_nMM > 0) {
          
          # sample new_sIM for those that matured
          mm_indices <- sample(length(G_new_sIM), size = size_nMM)
          G_new_nMM <- G_new_sIF[mm_indices]
          
          # rest of new_sIM didn't mature
          G_new[2, a] <- list(G_new_sIM[-mm_indices])
          
          # if none matured
        } else { 
          G_new[2, a] <- list(G_new_sIM)
          G_new_nMM <- NULL 
        }
        
        # if there are no old immature genotypes to sample OR none survived, 
        # then there are no new immature genotypes
      } else {
        G_new[2, a] <- list(NA)
        G_new_nMM <- NULL 
      }
      
      # if some mature genotypes survived AND some immature genotypes matured
      if (size_sMM > 0 & size_nMM > 0) {
        G_new_sMM <- sample(G_old_MM, size = size_sMM)
        G_new[4, a] <- list(c(G_new_nMM, G_new_sMM)) 
      }
      
      # if no mature genotypes survived BUT 
      # some immature genotypes survived and matured
      if (!(size_sMM > 0) & size_nMM > 0) {
        G_new[4, a] <- list(G_new_nMM)
      }
      
      # if mature genotypes survived BUT 
      # no immature genotypes survived and matured
      if (size_sMM > 0 & !(size_nMM > 0)) {
        G_new_sMM <- sample(G_old_MM, size = size_sMM)
        G_new[4, a] <- list(G_new_sMM)
      }
      
      # if no mature genotypes survived AND
      # no immature genotypes survived and matured
      if (!(size_sMM > 0) & !(size_nMM > 0)) {
        G_new[4, a] <- list(NA)
      }
      
    }
    
    # genotype stats
    G_stats[, , y, 1] <- apply(G_new, c(1, 2), 
                               function(x) mean(unlist(x), na.rm = TRUE))    
    G_stats[, , y, 2] <- apply(G_new, c(1, 2), 
                               function(x) median(unlist(x), na.rm = TRUE))
    G_stats[, , y, 3] <- apply(G_new, c(1, 2), 
                               function(x) var(unlist(x), na.rm = TRUE))
    
    # phenotype array, dimensions sex * age * max N value
    P_new <- apply(G_new, c(1, 2), 
                   function(x) rnorm(n = length(unlist(x)), 
                                     mean = unlist(x), 
                                     sd = sqrt(varPhenotypic)))
    
    # phenotype stats
    P_stats[, , y, 1] <- apply(P_new, c(1, 2), 
                               function(x) mean(unlist(x), na.rm = TRUE))    
    P_stats[, , y, 2] <- apply(P_new, c(1, 2), 
                               function(x) median(unlist(x), na.rm = TRUE))
    P_stats[, , y, 3] <- apply(P_new, c(1, 2), 
                               function(x) var(unlist(x), na.rm = TRUE))
    
  } else {
    G       <- NULL
    G_stats <- NULL
    P       <- NULL
    P_stats <- NULL
    
  }
  
  # shift G and P
  G <- G_new
  P <- P_new
  
  # what objects to return
  output <- list(N, G, P, G_stats, P_stats)
  
  # output
  return(output)
  
}