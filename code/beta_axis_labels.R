# function to get beta axis labels

beta_axis_labels <- function(betas) {
  
  # load libraries
  library(ggplot2)
  library(dplyr)
  
  # values of x to plot
  x <- seq(from = 0, to = 1, by = 0.0001)

  # initialise DF
  DF <- data.frame(Breeding_Sex_Ratio = rep(x, times = length(betas)), 
                   Beta = rep(betas, each = length(x)), 
                   Reproductive_Success = NA)
  
  # CDF function
  for (b in 1:length(betas)) {
    
    start <- (b - 1)*length(x) + 1
    stop <- start + length(x) - 1
    DF$Reproductive_Success[start:stop] <- pbeta(x, shape1 = 1, shape2 = betas[b])
    
  }
  
  # make Beta factor
  DF$Beta <- as.factor(DF$Beta)
  
  # extract values from each beta curve that corresponds to a 10% decrease in 
  # reproductive success
  acceptable_loss_of_males <- DF %>% 
    group_by(Beta) %>%
    filter(Reproductive_Success <= 0.5) %>%
    filter(Breeding_Sex_Ratio == max(Breeding_Sex_Ratio)) %>%
    mutate(Acceptable_loss = (1 - Breeding_Sex_Ratio)*100) %>%
    select(Acceptable_loss)
  
  output <- as.character(acceptable_loss_of_males$Acceptable_loss)
  
  return(output)
  
}
