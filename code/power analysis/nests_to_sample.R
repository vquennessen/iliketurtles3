# function to determine our confidence in BSR estimate given how many nests we 
# sample to robustly estimate the BSR for that year

nests_to_sample <- function(pop_size = 100,            # total population size
                            # probabilities for mating with 1 - max males    
                            Mprob = c(0.463, 0.318, 0.157, 0.034, 0.028),
                            # probabilities for mating with 1 - max females    
                            Fprob = c(22/30, 7/30, 1/30),
                            id_probs = NULL,
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
  DF2 <- data.frame(BSR = rep(BSR, each = npN), 
                    PropNests = rep(propNests, times = nB), 
                    Proportion = NA)
  
  # for each BSR population
  for (b in 1:nB) {
    
    # make population of males and females
    nM <- pop_size*BSR[b]
    nF <- pop_size - nM
    
    # # make breeding pool of males
    # BPm <- rep(1:nM, each = maxM)
    
    # for each proportion of nests sampled
    for (pn in 1:npN) {
      
      # initialize vector of whether or not all males were identified
      ID <- rep(NA, nsims)
      
      # initialize number of nests, make sure no numbers below 1
      nNests <- matrix(round(rnorm(n = nF*nsims, mean = nests_mu, sd = nests_sd)), 
                       nrow = nF, ncol = nsims)
      
      # make sure there aren't any negative or 0 nests
      nNests[nNests < 1] <- 1
      
      # initialize number of males
      nMales <- matrix(sample(1:maxM, 
                              size = nF*nsims, 
                              prob = Mprob, 
                              replace = TRUE), 
                       nrow = nF, ncol = nsims)
      
      # for each simulation
      for (i in 1:nsims) {
        
        # initialize vector to see which males in the season were identified
        identified_for_season <- NA
        
        # make breeding pool of males
        BPm <- rep(1:nM, each = maxM)
        
        # for each female
        for (f in 1:nF) {
          
          # how many males for this female
          nM_f <- nMales[f, i]
          
          # if there are not enough unique males left in the breeding pool 
          # for this female
          if (length(unique(BPm)) < nM_f) {
            
            # change the number of males with however many unique males are left
            nM_f <- length(unique(BPm))
            
          }
          
          # if there are no males left, stop the loop for the females    
          if (length(unique(BPm)) == 0) { break }
          
          # who are the contributing males themselves, sample from breeding pool 
          # without duplicates
          males_f <- sample(unique(BPm), size = nM_f, replace = FALSE)
          
          # if there is only one male that mated with this female    
          if (nM_f == 1) { 
            
            # it gets identified no matter what
            identified_for_female <- males_f
            
          } else {
            
            # new breeding pool for males
            BPm <- BPm[-match(males_f, BPm)]
            
            # probability of identification of all possible males for this female
            sub <- subset(id_probs, Males_contributing == nM_f & Probability > 0)
            
            # if there's only one possible number of males identified for any nest
            if (nrow(sub) == 1) {
              
              identified_for_female <- males_f
              
            } else {
              
              # how many nests for this female
              nN <- nNests[f, i]
              
              # how many males were identified in each nest for this female?
              nM_id <- sample(sub$Males_identified, 
                              size = nN, 
                              prob = sub$Probability, 
                              replace = TRUE)
              
              # if all the males for this female were identified in any nest
              if (sum(nM_id == nM_f) > 0) {
                
                identified_for_female <- males_f
                
                # otherwise, which of the possible males were identified across 
                # all nests
              } else {
                
                identified_for_female <- sample(males_f, 
                                                size = nM_id, 
                                                replace = FALSE)
                
              }
              
            }
            
          }
          
          # WHICH males were identified, add to identified males vector
          identified_for_season <- append(identified_for_season, 
                                          identified_for_female)
          
        }
        
        # were all the males identified across the whole season for this sim?
        ID[i] <- ifelse(length(na.omit(unique(identified_for_season))) == as.integer(nM), 1, 0)
        
      }
      
      # calculate index
      index <- (b - 1)*npN + pn
      print(index)
      
      # proportion of simulations where all males were identified
      all_males_ID <- mean(ID, na.rm = TRUE)
      
      # add ID to dataframe
      DF2$Proportion[index] <- all_males_ID
      
    }
    
  }
  
  # return output
  return(DF2)
  
}