# population dynamics with demographic stochasticity

pop_dynamics <- function(N,
                         max_age,
                         y,
                         M,
                         IF_survival,
                         IM_survival,
                         MF_survival,
                         MM_survival,
                         evolution,
                         G,
                         G_stats,
                         P,
                         P_stats) {
  # survival based on binomial distribution with survival rates as probabilities
  # immature females that survived
  survived_immature_F <- rbinom(n = max_age - 1,
                                size = round(N[1, 1:(max_age - 1), y - 1]),
                                prob = IF_survival[1:(max_age - 1)])
  
  # immature females that matured
  new_mature_F <- rbinom(n = max_age - 1, size = survived_immature_F, prob = M)
  
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
  new_mature_M <- rbinom(n = max_age - 1, size = survived_immature_M, prob = M)
  
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
      
      # females
      G_old_IF <- G[1, a - 1, which(!is.na(G[1, a - 1, ]))]
      G_old_MF <- G[3, a - 1, which(!is.na(G[3, a - 1, ]))]
      
      size_sIF <- survived_immature_F[a - 1]
      size_nMF <- new_mature_F[a - 1]
      size_sMF <- survived_mature_F[a - 1]
      
      if(size_sIF > sum(!is.na(G[1, a - 1, ]))) {
        print(paste('a =', a, '- error! size_sIF > G_old_IF', sep = ' '))
        break }   
      if(size_nMF > sum(!is.na(G[1, a - 1, ]))) {
        print(paste('a =', a, '- error! size_nMF > G_old_IF', sep = ' '))
        break } 
      if(size_sIF + size_nMF > sum(!is.na(G[1, a - 1, ]))) {
        print(paste('a =', a, '- error! size_sIF + size_nMF > G_old_IF', 
                    sep = ' '))
        break } 
      if(size_sMF > sum(!is.na(G[3, a - 1, ]))) {
        print(paste('a =', a, '- error! size_sMF > G_old_MF', sep = ' '))
        break } 
      
      #  males
      G_old_IM <- G[2, a - 1, which(!is.na(G[2, a - 1, ]))]
      G_old_MM <- G[4, a - 1, which(!is.na(G[4, a - 1, ]))]
      
      size_sIM <- survived_immature_M[a - 1]
      size_nMM <- new_mature_M[a - 1]
      size_sMM <- survived_mature_M[a - 1]
      
      if(size_sIM > sum(!is.na(G[2, a - 1, ]))) {
        print(paste('a =', a, '- error! size_sIM > G_old_IM', sep = ' '))
        break }   
      if(size_nMM > sum(!is.na(G[2, a - 1, ]))) {
        print(paste('a =', a, '- error! size_nMM > G_old_IM', sep = ' '))
        break } 
      if(size_sIM + size_nMM > sum(!is.na(G[2, a - 1, ]))) {
        print(paste('a =', a, '- error! size_sIM + size_nMM > G_old_IM', 
                    sep = ' '))
        break } 
      if(size_sMM > sum(!is.na(G[4, a - 1, ]))) {
        print(paste('a =', a, '- error! size_sMM > G_old_MM', sep = ' '))
        break }      
      
      ###### new female genotypes ##############################################
      
      # if there are immature genotypes to sample from AND some survived
      if (sum(!is.na(G_old_IF)) > 0 & size_sIF > 0) {
        
        # which immature genotypes survived
        new_sIF <- sample(G_old_IF, size = size_sIF)
        
        # if any matured
        if (size_nMF > 0) {
          
          # sample new_sIF for those that matured
          m_indices <- sample(1:length(new_sIF), size = size_nMF)
          new_nMF <- new_sIF[m_indices]
          
          # rest of new_sIF didn't mature
          G[1, a, ] <- c(new_sIF[-m_indices], 
                          rep(NA, max_N - length(new_sIF[-m_indices])))
          
          # if none matured
        } else { 
          G[1, a, ] <- c(new_sIF, 
                          rep(NA, max_N - length(new_sIF)))
          G_new_nMF <- NULL }
        
        # if there are no old immature genotypes to sample OR none survived
      } else {
        G_new[1, a, ] <- rep(NA, max_N)
        G_new_nMF <- NULL }
      
      # if there are mature genotypes to sample from AND some survived
      if (sum(!is.na(G_old_MF)) > 0 & size_sMF > 0) {
        
        G_new_sMF <- sample(G_old_MF, size = size_sMF)
        
        # if there are no old mature genotypes to sample OR none survived
      } else { G_new_sMF <- NULL }
      
      # add to genotypes array
      
      if (is.null(G_new_nMF) & is.null(G_new_sMF)) {
        G_new[3, a, ] <- rep(NA, max_N)
      } else{
        G_new[3, a, ] <- c(G_new_nMF, G_new_sMF, 
                            rep(NA, max_N - length(c(G_new_nMF, G_new_sMF))))} 
      
      ##### new male genotypes #################################################
      
      
      
      
      
      
      
      
      
      
    #   # if there are no new immature females, no genotypes to sample
    #   if (size_IF == 0) { G_new[1, a, ] <- rep(NA, max_N) 
    #   
    #   } else {
    #     
    #     # make sure there is more than 1 genotype to sample
    #     if (length(G_old_IF) > 1) {
    #       
    #       G_new[1, a, ] <- c(sample(G_old_IF, size = size_IF), 
    #                          rep(NA, max_N - size_IF)) }
    #     
    #     # otherwise, if there's only genotype to sample, the sample IS the 
    #     # genotype
    #     if (length(G_old_IF) == 1) { G_new[1, a, ] <- G_old_IF } }
    #   
    #   ##### immature males #####
    #   
    #   G_old_IM <- G[2, a - 1, which(!is.na(G[2, a - 1, ]))]
    #   size_IM <- survived_immature_M[a - 1] - new_mature_M[a - 1]
    #   if(size_IM > sum(!is.na(G[2, a - 1, ]))) {
    #     print(paste('a =', a, '- error! size_IM > length(G_old_IM)', sep = ' '))
    #     break }
    #   
    #   # if there are no new immature males, no genotypes to sample
    #   if (size_IM == 0) { G_new[2, a, ] <- rep(NA, max_N) 
    #   
    #   } else {
    #     
    #     # make sure there is more than 1 genotype to sample
    #     if (length(G_old_IM) > 1) {
    #       
    #       G_new[2, a, ] <- c(sample(G_old_IM, size = size_IM), 
    #                          rep(NA, max_N - size_IM)) }
    #     
    #     # otherwise, if there's only genotype to sample, the sample IS the 
    #     # genotype
    #     if (length(G_old_IM) == 1) { G_new[2, a, ] <- G_old_IM } }
    #   
    #   
    #   ##### mature females #####
    #   G_old_MF <- G[3, a - 1, which(!is.na(G[3, a - 1, ]))]
    #   size_sMF <- survived_mature_F[a - 1] 
    #   size_nMF <- new_mature_F[a - 1]
    #   if(size_sMF > sum(!is.na(G[3, a - 1, ]))) {
    #     print(paste('a =', a, '- error! size_sMF > length(G_old_MF)', sep = ' '))
    #     break }      
    #   if(size_nMF > sum(!is.na(G[1, a - 1, ]))) {
    #     print(paste('a =', a, '- error! size_nMF > length(G_old_IF)', sep = ' '))
    #     break }  
    #   
    #   # if there are no new mature females, no genotypes to sample
    #   if (size_sMF == 0 & size_nMF == 0) { G_new[3, a, ] <- rep(NA, max_N) 
    #   
    #   } else {
    #     
    #     # make sure there is more than 1 genotype to sample
    #     if (length(G_old_MF) > 1) {
    #       G_sMF <- sample(G_old_MF, size = size_sMF) } else { G_sMF <- NULL }
    #     if (length(G_old_IF) > 1) {
    #       G_nMF <- sample(G_old_IF, size = size_nMF) } else { G_nMF <- NULL }
    #     
    #     new_G_MF <- c(G_sMF, G_nMF)
    #     
    #     G_new[3, a, ] <- c(new_G_MF, rep(NA, max_N - length(new_G_MF)))}
    #   
    #   # otherwise, if there's only one genotype to sample, the sample IS the 
    #   # genotype
    #   if (length(G_old_MF) == 1) { G_new[3, a, ] <- G_old_MF } }
    # 
    # ##### mature males #####
    # 
    # G_old_MM <- G[4, a - 1, which(!is.na(G[4, a - 1, ]))]
    # size_MM <- survived_mature_M[a - 1] + new_mature_M[a - 1]
    # if(size_MM > sum(!is.na(G[4, a - 1, ]))) {
    #   print(paste('a =', a, '- error! size_MM > length(G_old_MM)', sep = ' '))
    #   break }
    # 
    # # if there are no new mature males, no genotypes to sample
    # if (size_MM == 0) { G_new[4, a, ] <- rep(NA, max_N) 
    # 
    # } else {
    #   
    #   # make sure there is more than 1 genotype to sample
    #   if (length(G_old_MM) > 1) {
    #     
    #     G_new[4, a, ] <- c(sample(G_old_MM, size = size_MM), 
    #                        rep(NA, max_N - size_MM)) }
    #   
    #   # otherwise, if there's only genotype to sample, the sample IS the 
    #   # genotype
    #   if (length(G_old_MM) == 1) { G_new[4, a, ] <- G_old_MM } }
    
    
    
    # # survived immature males
    # if (length(G[2, a - 1, ][which(!is.na(G[2, a - 1, ]))]) > 0) {
    #   G_new[2, a, ] <- c(sample(G[2, a - 1, ][!is.na(G[2, a - 1, ])], size = c(
    #     survived_immature_M[a - 1] -
    #       new_mature_M[a - 1]
    #   )),
    #   rep(NA, max_N - survived_immature_M[a - 1] +
    #         new_mature_M[a - 1]))
    # }
    # 
    # # survived mature females
    # if (length(G[3, a - 1, ][which(!is.na(G[3, a - 1, ]))]) > 0) {
    #   g_survived_mature <- sample(G[3, a - 1, ][which(!is.na(G[3, a - 1, ]))], size = survived_mature_F[a - 1])
    # } else {
    #   g_survived_mature <- NULL
    # }
    # 
    # if (length(G[1, a - 1, ][which(!is.na(G[1, a - 1, ]))]) > 0) {
    #   g_new_mature <- sample(G[1, a - 1, ][!is.na(G[1, a - 1, ])], size = new_mature_F[a - 1])
    # } else {
    #   g_new_mature <- NULL
    # }
    # 
    # G_new[3, a, ] <- c(
    #   append(g_survived_mature, g_new_mature),
    #   NA - length(g_survived_mature) - length(g_new_mature)
    # )
    # 
    # # survived mature males
    # if (length(G[4, a - 1, ][which(!is.na(G[4, a - 1, ]))]) > 0) {
    #   g_survived_mature <- sample(G[4, a - 1, ][!is.na(G[4, a - 1, ])], size = survived_mature_M[a - 1])
    # } else {
    #   g_survived_mature <- NULL
    # }
    # 
    # if (length(G[2, a - 1, ][which(!is.na(G[2, a - 1, ]))]) > 0) {
    #   if (length(G[2, a - 1, ][which(!is.na(G[2, a - 1, ]))]) == 1) {
    #     
    #   }
    #   
    #   g_new_mature <- sample(G[2, a - 1, ][which(!is.na(G[2, a - 1, ]))], size = new_mature_M[a - 1])
    # } else {
    #   g_new_mature <- NULL
    # }
    # 
    # G_new[4, a, ] <- c(
    #   append(g_survived_mature, g_new_mature),
    #   NA - length(g_survived_mature) - length(g_new_mature)
    # )
  }
  
  # new arrays
  G <- G_new
  
  # genotype stats
  G_stats[, , y, 1] <- apply(G, c(1, 2), mean, na.rm = TRUE)
  G_stats[, , y, 2] <- apply(G, c(1, 2), median, na.rm = TRUE)
  G_stats[, , y, 3] <- apply(G, c(1, 2), var, na.rm = TRUE)
  
  # phenotype array, dimensions sex * age * max N value
  P <- rnorm(
    n = c(4 * max_age * max_N),
    mean = G,
    sd = sqrt(varPhenotypic)
  )
  
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