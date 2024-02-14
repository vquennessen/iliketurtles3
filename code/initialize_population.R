initialize_population <- function(beta, burn_in, max_age, M, 
                                  F_remigration_int, M_remigration_int,
                                  nests_mu, eggs_mu, hatch_success_mu,
                                  k, T_piv, temp_mu, f_Leslie, m_Leslie) {
  
  
  # dimensions
  A <- max_age
  
  # initialize population size array by age class and sex
  N <- array(rep(NA, times = 2 * A * burn_in), 
             dim = c(2, A, burn_in))
  
  # intialize N as a dataframe
  DF <- data.frame(Beta = rep(beta, times = A*burn_in), 
                   Age = rep(1:A, times = burn_in), 
                   Year = rep(1:burn_in, each = A),
                   N = rep(0, times = A*burn_in))
  
  males <- DF
  females <- DF
  
  # add initial population size estimates to N at t = 1
  
  # females and males - 100 per age
  N[, 2:max_age, 1] <- 1000
  
  # female and male hatchlings - 10000 each
  N[, 1, 1] <- 1000000
  
  # move population forward in time burn_in years
  for (y in 2:burn_in) {
    
    ##### population dynamics
    # annual survival - females
    N[1, , y] <- round(f_Leslie %*% N[1, , y - 1])
    
    # annual survival - males
    N[2, , y] <- round(m_Leslie %*% N[2, , y - 1])
    
    ##### reproduction
    
    # calculate number of breeding adults
    # females only breed every F_remigration_int years
    n_breeding_F <- sum(N[1, (1:max_age)*M, y - 1], 
                        na.rm = TRUE) / F_remigration_int
    
    # males only breed every M_remigration_int years
    n_breeding_M <- sum(N[2, (1:max_age)*M, y - 1], 
                        na.rm = TRUE) / M_remigration_int
    
    if (n_breeding_F > 0 & n_breeding_M > 0) {
      
      # proportion of males
      # multiply by 2 to get BSR from 0 to 1 instead of 0 to 0.5
      BSR <- 2*(n_breeding_M / (n_breeding_M + n_breeding_F))
      
      # relate prop_males to breeding success via mating function
      breeding_success <- pbeta(BSR, shape1 = 1, shape2 = beta)
      
      # number of nests per female
      nests <- round(nests_mu)
      
      # replace any zeros or -1 with +1
      nests[which(nests < 1)] <- 1
      
      # initialize eggs vector
      eggs <- rep(round(eggs_mu), times = n_breeding_F)
      
      # total hatchlings = total eggs * hatching success * breeding_success
      hatchlings <- sum(eggs) * hatch_success_mu * breeding_success
      
      # determine proportion of male hatchlings based on temperature
      prop_male <- 1/(1 + exp(k*(temp_mu-T_piv)))
      
      # number of male and female hatchlings
      # female hatchlings
      N[1, 1, y] <- round(hatchlings * (1 - prop_male))
      # male hatchlings
      N[2, 1, y] <- round(hatchlings * prop_male)
      
    } else { 
      
      N[1, 1, y] <- 0
      N[2, 1, y] <- 0 
      
    }
    
    # update dataframe
    start <- A*(y - 1) + 1
    end <- start + A - 1
    females$N[start:end] <- N[1, , y] / sum(N[1, , y])
    males$N[start:end] <- N[2, , y] / sum(N[2, , y])
    
  }
  
  # put females and males DF together
  females$Sex <- 'Female'
  males$Sex <- 'Male'
  DF2 <- rbind(females, males)
  
  # # # plot
  # DF2 %>%
  #   # filter(Beta == betas[9]) %>%
  #   ggplot(aes(x = Year, y = N, colour = as.factor(Age))) +
  #   geom_line() +
  #   facet_wrap(vars(Sex))
  
  SAD <- DF2 %>%
    filter(Year == burn_in)
  
  return(SAD)
  
}
