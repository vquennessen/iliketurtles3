# function to get beta axis labels

OPMs_to_betas <- function(OPMs) {
  
  # load libraries
  library(ggplot2)
  library(dplyr)

  # beta values to cycle through
  betas <- seq(from = 1, to = 100, by = 0.01)

  # initialise DF
  DF <- data.frame(Operational_Proportion_Male = rep(OPMs, each = length(betas)), 
                   Beta = rep(betas, times = length(OPMs)), 
                   Reproductive_Success = NA)
  
  # CDF function
  for (a in 1:length(OPMs)) {
    
    start <- (a - 1)*length(betas) + 1
    stop <- start + length(betas) - 1
    DF$Reproductive_Success[start:stop] <- pbeta(2 * OPMs[a], 
                                                 shape1 = 1, 
                                                 shape2 = betas)
    
  }
  
  # make Beta factor
  #DF$Beta <- as.factor(DF$Beta)
  
  acceptable_loss_of_males <- DF %>% 
    group_by(Operational_Proportion_Male) %>%
    filter(Reproductive_Success <= 0.99) %>%
    filter(Beta == max(as.numeric(Beta)))
  
  output <- as.character(acceptable_loss_of_males$Beta)
  
  return(output)
  
}
