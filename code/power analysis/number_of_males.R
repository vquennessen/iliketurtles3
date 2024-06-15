# number of males

# set working directory
setwd('~/Projects/iliketurtles3/code/power analysis/')

# function parameters
hatchlings_mu <- 100.58                  # number of eggs per nest, mean
hatchlings_sd <- 22.61                   # number of eggs per nest, SD
max_males <- 5                           # max # of M F can mate with
n_sims <- 1e6                            # number of simulations to run

n_sizes <- c(32, 96)                     # sample sizes to calculate probs for  
fertilization_modes <- c('random',       # fertilization modes
                         'exponential', 
                         'dominant50', 
                         'dominant70', 
                         'dominant90', 
                         'mixed_dominant')

# dimensions
nF <- length(fertilization_modes)
nM <- max_males - 1
nS <- length(n_sizes)

# males identified


# pre-allocate data frame
DF <- data.frame(Fertilization_mode = NA,
                 Males_contributing = NA, 
                 Sample_size = NA,
                 Males_identified = NA,
                 Probability = NA)

# for each fertilization mode
for (f in 1:nF) {
  
  # fertilization mode
  fertilization_mode <- fertilization_modes[f]
  
  # for each number of males that contribute to a nest:
  for (m in 2:max_males) {
    
    # set contributions per males
    if (fertilization_mode == 'dominant90') {
      MC <- 0.90
      contributions <- c(MC, rep((1 - MC)/(m - 1), (m - 1)))
      
    } else if (fertilization_mode == 'dominant70') {
      MC <- 0.70
      contributions <- c(MC, rep((1 - MC)/(m - 1), (m - 1)))
      
    } else if (fertilization_mode == 'dominant50') {
      MC <- 0.50
      contributions <- c(MC, rep((1 - MC)/(m - 1), (m - 1)))
      
    } else if (fertilization_mode == 'exponential') {
      MC <- 0.5
      contributions <- 0.5^c(1:(m-1))
      contributions <- c(contributions, contributions[m-1])
      
    } else if (fertilization_mode == 'random') {
      contributions <- rep(1/m, m)
      
    } else if (fertilization_mode == 'mixed_dominant') {
      doms <- sample(c(0.50, 0.70, 0.90), size = n_sims, replace = TRUE)
      M1 <- matrix(doms, nrow = n_sims, ncol = 1)
      M2 <- matrix(rep((1 - doms) / (m - 1), m - 1), nrow = n_sims, ncol = m - 1)
      probs <- cbind(M1, M2)
      
    }
    
    # for each sample size
    for (s in 1:nS) {
      
      # pre-allocate vector of males identified
      males_identified <- rep(NA, n_sims)
      
      # initialize nest sizes
      # pull numbers of hatchlings from normal distribution
      n_hatchlings <- rnorm(n = n_sims, mean = hatchlings_mu, sd = hatchlings_sd)
      
      # set any nests with 0 or fewer eggs to 10 eggs
      n_hatchlings[which(n_hatchlings <= 0)] <- 10
      
      for (j in 1:n_sims) {
        
        # if mixed dominant, extract contributions
        if (fertilization_mode == 'mixed_dominant') { contributions <- probs[j, ]}
        
        # make nest with i fathers
        nest <- sample(x = 1:m, 
                       size = n_hatchlings[j], 
                       replace = TRUE,
                       prob = contributions)
        # }
        
        # take sample of size j from nest (or the whole nest if < j)
        sample_size <- min(n_sizes[s], length(nest))
        samples <- sample(x = nest, 
                          size = sample_size,
                          replace = FALSE)
        
        # how many males were identified
        males_identified[j] <- length(unique(samples))
        # estimate[k] <- length(unique(samples))
        
      }
      
      # calculate index in data frame
      # index <- (i - 2)*(max(n_sizes)) + j 
      # print(index)
      
      # probabilities for each # of males
      props <- rep(NA, m)
      
      for (k in 1:m) {
        
        props[k] <- round(length(males_identified[males_identified == k]) / 
                            length(males_identified), digits = 4)
        
      }
      
      # create dataframe
      # pre-allocate data frame
      DF2 <- data.frame(Fertilization_mode = rep(fertilization_modes[f], times = m),
                        Males_contributing = rep(m, times = m),
                        Sample_size = rep(n_sizes[s], times = m),
                        Males_identified = 1:m,
                        Probability = props)
      
      # attach to DF
      DF <- rbind(DF, DF2)
      
    }
    
  }
  
}

# remove row of NAs at the top
number_of_males <- DF[-1, ]

# save object
save(number_of_males, 
     file = 'number_of_males.Rdata')
