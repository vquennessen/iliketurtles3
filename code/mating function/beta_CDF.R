  # cumulative density function of a beta distribution
  
  # load libraries
  library(ggplot2)
  
  # values of x to plot
  x <- seq(from = 0, to = 1, by = 0.01)
  
  # beta values to cycle through
  betas <- c(1, 2, 3, 5, 10, 20, 100)
  
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
  
  # plot
  ggplot(data = DF, aes(x = Breeding_Sex_Ratio/2, y = Reproductive_Success, 
                        color = Beta)) +
    geom_hline(yintercept = 0.5, linetype = 2, alpha = 0.5, lwd = 2) +
    geom_line(lwd = 2) +
    ylab('Reproductive Success') +
    xlab('Breeding Sex Ratio (Proportion Male)') +
    ggtitle('Hypothetical mating functions') +
    theme_gray() +
    theme(axis.title.y = element_text(margin = margin(r = 15, l = 10)), 
          axis.title.x = element_text(margin = margin(t = 15, b = 10)), 
          plot.title = element_text(margin = margin(b = 10, t = 10), 
                                    size = 30), 
          axis.text = element_text(size = 20), 
          axis.title = element_text(size = 25), 
          legend.title = element_text(size = 20), 
          legend.text = element_text(size = 15))
  
