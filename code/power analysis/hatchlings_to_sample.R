### function for number of eggs to sample to determine the number of sires

hatchlings_to_sample <- function(hatchlings_mu,
                                 hatchlings_sd,      # number of eggs per nest
                                 max_males,          # max # of M F can mate with
                                 fertilization_mode, # fertilization mode
                                 n_sims,             # number of simulations to run
                                 n_sizes,            # sample sizes to run  
                                 computer)           # which computer is this running on    
{
  
  # pre-allocate data frame
  DF <- data.frame(Fertilization_mode = fertilization_mode,
                   Males = rep(2:max_males, each = max(n_sizes)), 
                   Sample_size = rep(c(1:max(n_sizes)), times = (max_males - 1)), 
                   Proportion_correct = rep(NA, dim = (max_males - 1)*(max(n_sizes) - 1))) 
  # Avg_detected = rep(NA, dim = max_males*(max_hatchlings - 1)))
  
  
  # for each number of males that contribute to a nest:
  for (i in 2:max_males) {
    
    # set contributions per males
    if (fertilization_mode == 'dominant90') {
      MC <- 0.90
      contributions <- c(MC, rep((1 - MC)/(i - 1), (i - 1)))
      title <- paste('Dominant (90%) fertilization mode', sep = '')
      
    } else if (fertilization_mode == 'dominant70') {
      MC <- 0.70
      contributions <- c(MC, rep((1 - MC)/(i - 1), (i - 1)))
      title <- paste('Dominant (70%) fertilization mode', sep = '')
      
    } else if (fertilization_mode == 'dominant50') {
      MC <- 0.50
      contributions <- c(MC, rep((1 - MC)/(i - 1), (i - 1)))
      title <- paste('Dominant (50%) fertilization mode', sep = '')
      
    } else if (fertilization_mode == 'exponential') {
      MC <- 0.5
      contributions <- 0.5^c(1:(i-1))
      contributions <- c(contributions, contributions[i-1])
      title <- 'Exponential (1/2) fertilization mode'
      
    } else if (fertilization_mode == 'random') {
      contributions <- rep(1/i, i)
      title <- 'Random fertilization mode'
      
    } else if (fertilization_mode == 'dominant_mixed') {
      doms <- sample(c(0.50, 0.70, 0.90), size = n_sims, replace = TRUE)
      M1 <- matrix(doms, nrow = n_sims, ncol = 1)
      M2 <- matrix(rep((1 - doms) / (i - 1), i - 1), nrow = n_sims, ncol = i - 1)
      probs <- cbind(M1, M2)
      title <- 'Mixed dominant fertilization mode'

    }
    
    # } else if (breeding == 'flexible_dominant') {
    #   contributions <- list(c(0.8868, 0.1132), 
    #                         c(0.4744, 0.3241, 0.2015), 
    #                         c(0.5485, 0.2508, 0.1509, 0.0499), 
    #                         c(0.4744, 0.1982, 0.1523, 0.0997, 0.0755))
    #   title <- 'D. Flexible dominant fertilization mode \n based on Alfaro-Nunez et al., 2015'
    #   
    # }
    
    # proportion_correct array
    prop_correct <- rep(NA, n_sims)
    
    # for each sample size
    for (j in 1:max(n_sizes)) {
      
      # pre-allocate correct identifications of number of males
      correct <- rep(NA, n_sims)
      # estimate <- rep(NA, n_sims)
      
      # initialize nest sizes
      # pull numbers of hatchlings from normal distribution
      n_hatchlings <- rnorm(n = n_sims, mean = hatchlings_mu, sd = hatchlings_sd)
      
      # set any nests with 0 or fewer eggs to 10 eggs
      n_hatchlings[which(n_hatchlings <= 0)] <- 10
      
      for (k in 1:n_sims) {
        
        # simulate male contributions to nest
        # if (breeding != 'flexible_dominant') {
        #   nest <- sample(x = 1:i, 
        #                  size = n_hatchlings[k], 
        #                  replace = TRUE,
        #                  prob = contributions)
        # } else {
        
        # if mixed dominant, extract contributions
        if (fertilization_mode == 'dominant_mixed') { contributions <- probs[k, ]}
        
        # make nest with i fathers
        nest <- sample(x = 1:i, 
                       size = n_hatchlings[k], 
                       replace = TRUE,
                       prob = contributions)
        # }
        
        # take sample of size j from nest (or the whole nest if < j)
        sample_size <- min(j, length(nest))
        samples <- sample(x = nest, 
                          size = sample_size,
                          replace = FALSE)
        
        # correct allocation of number of males?
        correct[k] <- length(unique(samples)) == i
        # estimate[k] <- length(unique(samples))
        
      }
      
      # calculate index in data frame
      index <- (i - 2)*(max(n_sizes)) + j 
      # print(index)
      
      # stick proportion in data frame
      DF$Proportion_correct[index] <- mean(correct)
      # DF$Avg_detected[index] <- mean(estimate)
      
    }
    
  }
  
  #### plot results
  
  # color-blind friendly color palette
  colors <- viridis(max_males)
  
  # plot results - proportion correct
  fig1 <- ggplot(DF, aes(x = Sample_size, y = Proportion_correct, 
                         col = as.factor(Males))) +
    geom_hline(yintercept = 0.8, linetype = 2) +
    geom_path(lwd = 1) +
    labs(col = 'Number \n of Males') +
    scale_color_manual(values = colors) +
    ylab('Proportion Correct') +
    xlab('Hatchlings Sampled') +
    geom_vline(xintercept = c(n_sizes), linetype = 3) +
    ggtitle(title)
  
  # 
  # # plot results - average estimate (over or underestimated?)
  # fig2 <- ggplot(DF, aes(x = Sample_size, y = Avg_detected/Males, 
  #                        col = as.factor(Males))) +
  #   geom_hline(yintercept = 1, linetype = 2) +
  #   geom_path(lwd = 1.25) +
  #   labs(col = 'Number \n of Males') +
  #   scale_color_manual(values = colors) +
  #   ylab('Number of Males Detected Relative to Correct Value') +
  #   xlab('Hatchlings Sampled') +
  #   geom_vline(xintercept = c(n_sizes), linetype = 3) +
  #   ggtitle(title)
  
  # which computer am I using???
  if (computer == 'desktop') { comp <- 'Vic' } else if (computer == 'laptop') { comp <- 'vique' }
  
  # save plot
  ggsave(plot = fig1, 
         filename = paste(fertilization_mode, '_fig1_proportion_correct.png', sep = ''),
         path = paste('C://Users/', comp, '/Documents/Projects/iliketurtles3/figures/power analyses', 
                      sep = ''),
         width = 6, height = 4)
  
  # What's our confidence if we sample 32 percent of the eggs?
  DFsamples <- DF %>% 
    filter(Sample_size %in% n_sizes) %>%
    spread(Sample_size, Proportion_correct)
  
  # save confidence table
  png(filename = paste('C://Users/', comp, '/Documents/Projects/iliketurtles3/figures/power analyses/', 
                       fertilization_mode, '_conf_table.png', sep = ''), 
      width = 200, height = 200)
  grid.table(DFsamples, rows = NULL)
  dev.off()
  
  
  output <- list(fig1, DF)
  
  return(output)
  
}