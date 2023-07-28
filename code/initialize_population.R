initialize_population <- function(beta, burn_in,
                                  max_age, age_maturity, remigration_int,
                                  nests_mu, eggs_mu, hatch_success_mu,
                                  logit_a, logit_b, temp_mu,
                                  f_Leslie, m_Leslie) {
  
  # load libraries
  # library(dplyr)
  # library(ggplot2)
  
  # # model parameters
  # betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
  # burn_in <- 1000
  # 
  # # demographics
  # max_age <- 85                   # lifespan
  # age_maturity <- 23              # age at first reproduction
  # remigration_int <- 5.557        # remigration interval
  # nests_mu <- 4.945312            # mean number of nests per female per season
  # eggs_mu <- 100.6486             # mean number of eggs per nest
  # hatch_success_mu <- 0.8241024   # mean of hatching success
  # logit_a <- 41.362228            # temp -> proportion of males a
  # logit_b <- -1.415462            # temp -> proportion of males b
  # temp_mu <- 31.80387             # base incubation temp mean
  
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
    # females only breed every remigration_int years
    n_breeding_F <- round(sum(N[1, age_maturity:max_age, y - 1], 
                              na.rm = TRUE) / remigration_int)
    
    # males mate every year???
    n_breeding_M <- sum(N[2, age_maturity:max_age, y - 1], na.rm = TRUE)
    
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
      prop_male <- exp(logit_a + logit_b*temp_mu) / (1 + exp(logit_a + logit_b*temp_mu))
      
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
