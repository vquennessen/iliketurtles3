# evolution script

evolution <- function(N, M, max_age, y, 
                      G_piv, P_piv, Delta_piv, Pivotal_temps,
                      Gamma_piv, Epsilon_piv, evolution_piv, 
                      G_threshold, P_threshold, Delta_threshold, 
                      Threshold_temps, Gamma_threshold, Epsilon_threshold, 
                      evolution_threshold)
  
{
  
  ##### pivotal temperature ####################################################
  
  # evolution in the pivotal temperature
  if (evolution_piv == TRUE) {
    
    # weighted average of genotypes for mature females from last year
    GF_piv <- weighted.mean(x = G_piv, w = N[1, , y - 1] * M)    
    
    # weighted average of genotypes for mature males from last year
    GM_piv <- weighted.mean(x = G_piv, w = N[2, , y - 1] * M)
    
    # parent genotypes to hatchling genotype, with genotypic variance
    GH_piv <- (GF_piv + GM_piv) / 2 + Gamma_piv[y]
    
    # hatchling expected phenotype
    PH_piv <- GH_piv + Epsilon_piv[y]
    
    # hatchling actual phenotype with phenotypic variation
    Pivotal_temps[y] <- PH_piv[y] + Delta_piv[y]    
    
    # update genotypes vector
    G_piv <- c(GH_piv, G_piv[1:(max_age - 1)])
    
    # update expected phenotypes vector
    P_piv <- c(PH_piv, P_piv[1:(max_age - 1)])
    
  } 
  
  ##### threshold temperature ##################################################
  
  # evolution in thermal threshold
  if (evolution_threshold == TRUE) {
    
    # weighted average of genotypes for mature females from last year
    GF_threshold <- weighted.mean(x = G_threshold, w = N[1, , y - 1] * M)    
    
    # weighted average of genotypes for mature males from last year
    GM_threshold <- weighted.mean(x = G_threshold, w = N[2, , y - 1] * M)
    
    # parent genotypes to hatchling genotype, with genotypic variance
    GH_threshold <- (GF_threshold + GM_threshold) / 2 + Gamma_threshold[y]
    
    # hatchling genotype to expected hatchling phenotype
    PH_threshold <- GH_threshold + Epsilon_threshold[y]
    
    # hatchling actual phenotype with phenotypic variation
    Threshold_temps[y] <- PH_threshold + Delta_threshold[y]
    
    # update genotypes vector
    G_threshold <- c(GH_threshold, G_threshold[1:(max_age - 1)])
    
    # update expected phenotypes vector
    P_threshold <- c(PH_threshold, P_threshold[1:(max_age - 1)])
    
  } 
  
  output <- c(Pivotal_temps[y], G_piv, P_piv, 
              Threshold_temps[y], G_threshold, P_threshold)
  
  return(output)
  
}