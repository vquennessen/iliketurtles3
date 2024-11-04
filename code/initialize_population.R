initialize_population <- function(beta, burn_in, max_age, M, 
                                  F_remigration_int, M_remigration_int,
                                  nests_mu, eggs_mu, hatch_success_A,
                                  hatch_success_k, hatch_success_t0,
                                  k, T_piv, temp_mu, f_Leslie, m_Leslie) {
  
  
  # dimensions
  A <- max_age
  
  # initialize population size array by age class and sex
  # N <- array(rep(NA, times = 2 * A * burn_in),
  #            dim = c(2, A, burn_in))
  # N <- array(rep(NA, times = 2 * A),
  #            dim = c(2, A))
  
  # intialize N as a dataframe
  # DF <- data.frame(Beta = rep(beta, times = (A*burn_in)), 
  #                  Age = rep(1:A, times = burn_in), 
  #                  Year = rep(1:burn_in, each = A),
  #                  N = rep(0, times = A*burn_in))
  
  N <- array(rep(0, times = 2 * A * burn_in), dim = c(2, A, burn_in))
  # DF <- data.frame(Beta = beta, 
  #                  Age = 1:A, 
  #                  Year = 1,
  #                  N = NA)
  
  # initial pop size
  # DF$N[1] <- 100
  # DF$N[2:A] <- 1
  N[ , 1, 1] <- 100

  N[ , 2:A, 1] <- 1
  
  # males <- DF
  # females <- DF
  
  # add initial population size estimates to N at t = 1
  
  # # females and males - 1000 per age
  # N[, 2:max_age, 1] <- 1000
  # 
  # # female and male hatchlings - 100000 each
  # N[, 1, 1] <- 1000000
  
  # move population forward in time burn_in years
  for (y in 2:burn_in) {
    
    # # current age distribution
    # current_F <- females$N
    # current_M <- males$N 
    
    ##### population dynamics
    # # annual survival - females
    # N[1, , y] <- round(f_Leslie %*% N[1, , y - 1])
    # 
    # # annual survival - males
    # N[2, , y] <- round(m_Leslie %*% N[2, , y - 1])
    # # annual survival - females
    # new_F <- round(f_Leslie %*% current_F)
    
    # step females forward
    N[1, , y] <- round(f_Leslie %*% N[1, , y - 1])

    # # annual survival - males
    # new_M <- round(m_Leslie %*% current_M)

    # step males forward
    N[2, , y] <- round(m_Leslie %*% N[2, , y - 1])
    
    # # demographic stochasticity
    # N <- pop_dynamics(N, max_age, y, F_survival, M_survival)
    
    
    
    ##### reproduction
    
    # calculate number of breeding adults
    # # females only breed every F_remigration_int years
    # n_breeding_F <- sum((N[1, 1:max_age, y - 1]*M), 
    #                     na.rm = TRUE) / F_remigration_int
    # 
    # # males only breed every M_remigration_int years
    # n_breeding_M <- sum((N[2, 1:max_age, y - 1]*M), 
    #                     na.rm = TRUE) / M_remigration_int
    
    # females only breed every F_remigration_int years
    n_breeding_F <- sum((N[1, , y] * M), na.rm = TRUE) / F_remigration_int
    
    # males only breed every M_remigration_int years
    n_breeding_M <- sum((N[2, , y] * M), na.rm = TRUE) / M_remigration_int
    
    # as long as there is at least one mature female and one mature male:
    if (n_breeding_F > 0.5 & n_breeding_M > 0.5) {
      
      # operational sex ratio - proportion of males
      OSR <- n_breeding_M / (n_breeding_M + n_breeding_F)
      
      # calculate reproductive success
      # if 50% males or fewer, use beta function to calculate breeding success
      if (OSR <= 0.5) {
        breeding_success <- pbeta(2 * OSR, shape1 = 1, shape2 = beta) 
        
      # else, if there are more than 50% males, all the females get to mate
      } else { breeding_success <- 1 }
      
      # # number of nests per female
      # nests <- round(nests_mu)
      # 
      # # # replace any zeros or -1 with +1
      # # nests[which(nests < 1)] <- 1
      # 
      # # initialize eggs vector
      # eggs <- rep(eggs_mu, times = n_breeding_F)
      
      # number of eggs
      eggs <- sum(n_breeding_F * nests_mu * eggs_mu)
      
      # hatching success
      hatch_success <- hatch_success_A / (1 + exp(-hatch_success_k * (temp_mu - hatch_success_t0)))
      
      # total hatchlings = breeding_success * total eggs * hatching success
      hatchlings <- breeding_success * eggs * hatch_success
      
      # determine proportion of male hatchlings based on temperature
      prop_male <- 1 / (1 + exp(-k * (temp_mu - T_piv)))
      
      # number of male and female hatchlings
      # female hatchlings
      N[1, 1, y] <- round(hatchlings * (1 - prop_male))
      # male hatchlings
      N[2, 1, y] <- round(hatchlings * prop_male)
      
    } else { 
      
      N[1, 1, burn_in] <- 0
      N[2, 1, burn_in] <- 0 
      
    }
    
    # # update dataframe
    # start <- A*(y - 1) + 1
    # end <- start + A - 1
    # females$N[start:end] <- N[1, , y] / sum(N[, , y])
    # males$N[start:end] <- N[2, , y] / sum(N[, , y])
    
    # # update dataframe with hatchlings
    # females$N <- new_F 
    # males$N <- new_M 
    # 
    # # update dataframe with year
    # females$Year <- y
    # males$Year <- y
    # 
  }
  
  # # put females and males DF together
  # females$Sex <- 'Female'
  # males$Sex <- 'Male'
  # DF2 <- rbind(females, males)
  
  # normalize
  # females$N <- females$N / sum(females$N)
  # males$N <- males$N / sum(males$N)
  SAD_F <- N[1, , burn_in] / sum(N[1, , burn_in])
  SAD_M <- N[2, , burn_in] / sum(N[2, , burn_in])
  
  # how many times more females than males?
  # M_multiplicators <- N[1, , burn_in] / N[2, , burn_in]
  # M_multiplicator <- M_multiplicators[1]
  
  # # put them together
  # SAD <- rbind(females, males)
  
  # # # plot
  # DF2 %>%
  #   # filter(Beta == betas[9]) %>%
  #   ggplot(aes(x = Year, y = N, colour = as.factor(Age))) +
  #   geom_line() +
  #   facet_wrap(vars(Sex))
  
  # SAD <- DF2 %>%
  #   filter(Year == burn_in)
  
  output <- list(SAD_F, SAD_M)
  
  return(output)
  
}
