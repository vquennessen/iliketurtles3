# function to determine our confidence in BSR estimate given how many nests we 
# sample to robustly estimate the BSR for that year

nests_to_sample <- function(nsims,            # number of simulations
                            pop_size,         # total population size
                            breeding,         # fertilization mode
                            Mprob,            # probs for mating with 1 - max M    
                            Fprob,            # probs for mating with 1 - max F   
                            nests_mu,         # average # of nests per F
                            nests_sd,         # sd # of nests per F
                            id_probs)         # probs of IDing 1-all M in a nest
  
  
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
        
        # how many females per male
        nFemales <- sample(1:maxF, size = nM, prob = Fprob, replace = TRUE)
        
        # make breeding pool of males
        BPm <- rep(1:nM, times = nFemales)
        
        # initialize nests list
        nests <- NA
        
        # for each female
        for (f in 1:nF) {
          
          # how many nests for this female
          nN_f <- nNests[f, i]
          
          # how many males for this female
          nM_f <- nMales[f, i]
          
          # if there are no males left, stop the loop for the females    
          if (length(unique(BPm)) == 0) { break }
          
          # if there are not enough unique males left in the breeding pool 
          # for this female
          if (length(unique(BPm)) < nM_f) {
            
            # change the number of males with however many unique males are left
            nM_f <- length(unique(BPm))
            
          }
          
          # who are the contributing males themselves, sample from breeding pool 
          # without duplicates
          males_f <- sample(unique(BPm), size = nM_f, replace = FALSE)
          
          # new breeding pool for males
          BPm <- BPm[-match(males_f, BPm)]
          
          # if there's only 1 male
          if (nM_f == 1) {
            
            # append identified male to nests nN_f times
            nests <- append(rep(list(males_f), times = nN_f))
            
            
          } else {
            
            # probability of identification of all possible males for this female
            sub <- subset(id_probs, Males_contributing == nM_f & Probability > 0)
            
            # if there's only one possible number of males identified for any nest
            if (nrow(sub) == 1) {
              
              nests <- append(nests, list(males_f))
              
            } else {
              
              # how many males were identified in each nest for this female?
              nM_id <- sample(sub$Males_identified,
                              size = nN_f,
                              prob = sub$Probability,
                              replace = TRUE)
              
              # if there's only 1 nest
              if (nN_f == 1) {
                
                nests <- append(nests, list(sample(males_f,
                                                   size = nM_id,
                                                   replace = TRUE)))
                
              } else {
                
                for (n in 1:nN_f) {
                  
                  nests <- append(nests, list(sample(males_f,
                                                     size = nM_id[n],
                                                     replace = TRUE)))
                  
                }
                
              }
              
            }
            
          }
          
        }
        
        # remove NA from nests
        nests <- na.omit(nests)
        
        # number of nests total
        all_nests <- length(nests)
        
        # how many nests
        num_nests <- round(all_nests*npN[pn])
        
        # sample all possible nests for proportion
        indices <- sample(1:num_nests, 
                               size = num_nests, 
                               replace = FALSE)
        
        # WHICH males were identified, add to identified males vector
        sampled_nests <- unlist(nests[nests == indices])
        
        # were all males identified?
        ID[i] <- ifelse(length(unique(sampled_nests)) == nM, 1, 0)
        
      }
      
      # calculate index
      index <- (b - 1)*npN + pn
      print(index)
      print(Sys.time())
      
      # proportion of simulations where all males were identified
      all_males_ID <- mean(ID, na.rm = TRUE)
      
      # add ID to dataframe
      DF2$Proportion[index] <- all_males_ID
      
    }
    
  }
  
  # return output
  return(DF2)
  
}