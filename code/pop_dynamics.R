# population dynamics with demographic stochasticity

pop_dynamics <- function(N, max_age, y, F_survival, M_survival) {
  
  #survival based on binomial distribution with survival rates as probabilities

  # for each age 
  for (a in 2:max_age) {
    
    # annual survival - females
    N[1, a, y] <- sum(rbinom(n = N[1, a - 1, y - 1], 
                             size = 1, 
                             prob = F_survival[a - 1]))
    
    # annual survival - males
    N[2, a, y] <- sum(rbinom(n = N[2, a - 1, y - 1], 
                             size = 1, 
                             prob = M_survival[a - 1]))
    
  }
  
  # output
  return(N)
  
}