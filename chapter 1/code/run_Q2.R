# run Q2

run_Q2 <- function(arguments) {
  
  # function arguments
  sample_size                 <- arguments[[1]]
  paternal_contribution_mode  <- arguments[[2]]
  nsims                       <- 1e5
  
  # model parameters
  pop_size <- 100                               # total population size
  
  # which scenario
  # scenario <- c('base', 'uniform_Fprob_no_polygyny', 'uniform_Fprob_and_Mprob')
  scenario <- c('uniform_Fprob_and_Mprob')
  
  if (scenario == 'base') {
    
    Fprob <- c(0.463, 0.318, 0.157, 0.034, 0.028)
    Mprob <- c(1)
    
  } 
  
  if (scenario == 'uniform_Fprob_no_polygyny') {
    
    Fprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)   
    Mprob <- c(1)
    
  } 
  
  if (scenario == 'uniform_Fprob_and_Mprob') {
    
    Fprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)   
    Mprob <- c(0.2, 0.2, 0.2, 0.2, 0.2)   
    
  }
  
  clutches_mu <- 4.95                              # average # of nests per F
  clutches_sd <- 2.09                              # sd # of nests per F
  
  # pull out probabilities of IDing different numbers of fathers
  prop_correct <- proportion_correct_all %>%
    filter(Sample_Size == sample_size) %>%
    filter(Paternal_Contribution_Mode == paternal_contribution_mode) %>%
    select(Fathers, Proportion_Correct)
  
  # write to progress text file
  update <- paste(Sys.time(), 
                  ' - sample size ', 
                  sample_size, 
                  ' - ', 
                  paternal_contribution_mode, 
                  ' - ', 
                  nsims, 
                  ' sims', 
                  sep = '')
  
  write(update, file = 'progress.txt', append = TRUE)
  
  # run sample_nests
  output <- clutches_to_sample(nsims, 
                               pop_size, 
                               sample_size, 
                               paternal_contribution_mode,
                               Fprob, 
                               Mprob, 
                               clutches_mu, 
                               clutches_sd, 
                               prop_correct)
  
  # save output
  save(output, 
       file = paste('output/',
                    scenario, 
                    '_',
                    sample_size, 
                    '_clutches_to_sample_', 
                    paternal_contribution_mode, 
                    '_', 
                    nsims, 
                    '.Rdata', 
                    sep = ''))
  
}
