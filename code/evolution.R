# evolution script

evolution <- function(N, max_age, y, breeding_F, breeding_M,
                      trait, h2, varG)
  
  
  
{
  
  ##### pivotal temperature ####################################################
  
  # evolution in the pivotal temperature
  if (evolution_piv == TRUE) {
    
    # weighted average of genotypes for mature females from last year
    GF_piv <- weighted.mean(x = G_piv, w = breeding_F)    
    
    # weighted average of genotypes for mature males from last year
    GM_piv <- weighted.mean(x = G_piv, w = breeding_M)
    
    # hatchling genotype = average of parental genotypes + genotypic variation 
    GH_piv <- (GF_piv + GM_piv) / 2 + Gamma_piv[y]
    
    # hatchling expected phenotype = hatchling genotype + epsilon
    PH_piv <- GH_piv + Epsilon_piv[y]
    
    # hatchling actual phenotype = expected phenotype + phenotypic variation
    Pivotal_temps[y] <- PH_piv + Delta_piv[y]    
    
    # update genotypes vector
    new_G_piv <- c(GH_piv, G_piv[1:(max_age - 1)])
    
    # update expected phenotypes vector
    new_P_piv <- c(PH_piv, P_piv[1:(max_age - 1)])
    
  } else {
    
    new_G_piv <- NULL
    new_P_piv <- NULL
    
  }
  
  ##### threshold temperature ##################################################
  
  # evolution in thermal threshold
  if (evolution_threshold == TRUE) {
    
    # weighted average of genotypes for mature females from last year
    GF_threshold <- weighted.mean(x = G_threshold, w = breeding_F)    
    
    # weighted average of genotypes for mature males from last year
    GM_threshold <- weighted.mean(x = G_threshold, w = breeding_M)
    
    # parent genotypes to hatchling genotype, with genotypic variance
    GH_threshold <- (GF_threshold + GM_threshold) / 2 + Gamma_threshold[y]
    
    # hatchling genotype to expected hatchling phenotype
    PH_threshold <- GH_threshold + Epsilon_threshold[y]
    
    # hatchling actual phenotype with phenotypic variation
    Threshold_temps[y] <- PH_threshold + Delta_threshold[y]
    
    # update genotypes vector
    new_G_threshold <- c(GH_threshold, G_threshold[1:(max_age - 1)])
    
    # update expected phenotypes vector
    new_P_threshold <- c(PH_threshold, P_threshold[1:(max_age - 1)])
    
  } else {
    
    new_G_threshold <- NULL
    new_P_threshold <- NULL
    
  }
  
  output <- list(Pivotal_temps[y], new_G_piv, new_P_piv, 
                 Threshold_temps[y], new_G_threshold, new_P_threshold)
  
  return(output)
  
}