# function to get beta axis labels

OSRs_to_betas <- function(OSRs) {
  
  # load libraries
  library(ggplot2)
  library(dplyr)

  # beta values to cycle through
  betas <- seq(from = 1, to = 100, by = 0.01)

  # initialise DF
  DF <- data.frame(Operational_Sex_Ratio = rep(OSRs, each = length(betas)), 
                   Beta = rep(betas, times = length(OSRs)), 
                   Reproductive_Success = NA)
  
  # CDF function
  for (a in 1:length(OSRs)) {
    
    start <- (a - 1)*length(betas) + 1
    stop <- start + length(betas) - 1
    DF$Reproductive_Success[start:stop] <- pbeta(2 * OSRs[a], 
                                                 shape1 = 1, 
                                                 shape2 = betas)
    
  }
  
  # make Beta factor
  #DF$Beta <- as.factor(DF$Beta)
  
  acceptable_loss_of_males <- DF %>% 
    group_by(Operational_Sex_Ratio) %>%
    filter(Reproductive_Success <= 0.99) %>%
    filter(Beta == max(as.numeric(Beta)))
  
  output <- as.character(acceptable_loss_of_males$Beta)
  
  return(output)
  
}
