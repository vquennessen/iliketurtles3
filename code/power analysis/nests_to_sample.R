# function to determine our confidence in BSR estimate given how many nests we 
# sample to robustly estimate the BSR for that year

sample_nests <- function(pop_size = 100,            # total population size
                         # probabilities for mating with 1 - max males    
                         Mprob = c(0.463, 0.318, 0.157, 0.034, 0.028),
                         # probabilities for mating with 1 - max females    
                         Fprob = c(22/30, 7/30, 1/30),
                         nests_mu = 4.59,           # average # of nests per F
                         nests_sd = 2.09,           # sd # of nests per F
                         eggs_mu = 100.58,          # average # of eggs per nest
                         eggs_sd = 22.68,           # sd # of eggs per nest
                         breeding = '',             # fertilization mode
                         sample_size = 32,          # sample size of hatchlings
                         nsims = 100000             # number of simulations
)  

{
  
  # dimensions
  maxM <- length(Mprob) # max number of males a female can mate with
  maxF <- length(Fprob) # max number of females a male can mate with
  
  # breeding sex ratios
  BSR <- seq(from = 0.05, to = 0.5, by = 0.05)
  nB <- length(BSR)
  
  # proportion of nests sampled
  propNests <- seq(from = 0.05, to = 1, by = 0.05)
  npN <- length(propNests)
  
  # pre-allocate data frame for results
  DF <- data.frame(BSR = rep(BSR, each = npN), 
                   PropNests = rep(propNests, times = nB), 
                   Proportion = NA)
  
  
  for (b in 1:nB) {
    
    # make population of males and females
    nM <- pop_size*BSR[b]
    nF <- pop_size - nM
    
    # make breeding pool of males
    BPm <- data.frame(Male = rep(1:nM, each = maxM), 
                      ID = 1:(nM*maxM))
    
    # assign males to females
    
    for (pn in 1:npN) {
      
      # initialize vector of whether or not all males were identified
      ID <- rep(NA, nsims)
      
      # initialize number of nests, make sure no numbers below 1
      nNests <- round(rnorm(n = nF, mean = nests_mu, sd = nests_sd))
      nNests[nNests < 1] <- 1
      
      for (i in 1:nsims) {
        
        # females mate with 1 - max males from breeding pool until they run out
        
        # for each female
        for (f in 1:nF) {
          
          # how many nests for this female
          nN <- nNests[f]
          
          # if there are enough males left in the breeding pool
          if (nrow(BPm) > nN) {
            
            # indices for contributing males
            indices <- sample(BPm$ID, size = nN, replace = FALSE)
            
            # contributing males themselves
            males <- unique(BPm$Male[indices])
            
            # new breeding pool for males
            BPm <- BPm[-indices, ]
            
            # how many males were identified?
            
            
          }
          
        }
        
        # nests with size and 1 - max male fathers
        
        # sample proportion of nests given fertilization mode
        
        # were all the contributing males identified?
        
        
      }
    }
    
  }
  
  
  # return output
  return(output)
  
}