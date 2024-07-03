# run Q2

run_Q2 <- function(arguments) {
  
  # function arguments
  sample_size         <- arguments[[1]]
  fertilization_mode  <- arguments[[2]]
  nsims               <- 1e5
  
  # model parameters
  pop_size <- 100                               # total population size
  
  # population parameters
  Mprob <- c(0.463, 0.318, 0.157, 0.034, 0.028) # probabilities for mating with 1 - max males    
  Fprob <- c(22/30, 7/30, 1/30)                 # probabilities for mating with 1 - max females    
  nests_mu <- 4.95                              # average # of nests per F
  nests_sd <- 2.09                              # sd # of nests per F
  
  # pull out probabilities of IDing different numbers of males
  id_probs <- number_of_males %>%
    filter(Sample_size == sample_size) %>%
    filter(Fertilization_mode == fertilization_mode) %>%
    select(Males_contributing, Males_identified, Probability)
  
  # write to progress text file
  update <- paste(Sys.time(), 
                  ' - sample size ', sample_size, ' - ', 
                  fertilization_mode, ' - ', 
                  nsims, ' sims', sep = '')
  write(update, file = 'progress.txt', append = TRUE)
  
  # run sample_nests
  output <- nests_to_sample(nsims, pop_size, sample_size, fertilization_mode,
                            Mprob, Fprob, nests_mu, nests_sd, id_probs)
  
  # save output
  save(output, 
       file = paste(sample_size, '_nests_to_sample_', fertilization_mode, '_', 
                    nsims, '.Rdata', sep = ''))
  
}
