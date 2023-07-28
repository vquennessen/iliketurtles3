# run_base_model <- function(scenario) {

run_base_model <- function(scenarios, num_sims, betas) {
  
  # beta values
  # betas <- c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14)
  
  # number of simulations
  # num_sims <- 10000
  
  ###### model inputs ##########################################################
  
  # turtle demographics
  max_age <- 85                                         # lifespan
  F_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - F
  F_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - F
  M_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - M
  M_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - M
  demographic_stochasticity <- TRUE                     # demographic stochasticity
  age_maturity <- 23                                    # age at first reproduction
  remigration_int <- 5.557                              # remigration interval
  nests_mu <- 4.945312                                  # mean number of nests per female per season
  nests_sd <- 2.089752                                  # sd of number of nests per female per season
  eggs_mu <- 100.6486                                   # mean number of eggs per nest
  eggs_sd <- 23.08206                                   # sd of number of eggs per nest
  hatch_success_mu <- 0.8241024                         # mean of hatching success
  hatch_success_a <- 2.089414                           # beta CDF shape 1 parameter for hatching success
  hatch_success_b <- 0.4496393                          # beta CDF shape 2 parameter for hatching success
  hatch_success_stochasticity <- TRUE                   # whether or not there is stochasticity in hatching success
  pivotal_temp <- 29.3                                  # pivotal temperature 
  TRT <- c(27.6, 31.4)                                  # Transitional range of temperatures
  logit_a <- 41.362228                                  # temp -> proportion of males a
  logit_b <- -1.415462                                  # temp -> proportion of males b
  
  # climate data
  temp_mu <- 31.80387                                # base incubation temp mean
  temp_sd <- 0.841012                                # base incubation temp sd
  climate_stochasticity <- FALSE                     # whether or not to add in
  
  # model parameters
  start_year <- 2023                              # first year to simulate
  end_year <- start_year + 3*max_age              # last year to simulate
  
  # dimensions
  A <- max_age
  Y <- length(start_year:end_year)
  
  ##############################################################################
  
  for (s in 1:length(scenarios)) {
    
    scenario = scenarios[s] 
    
    for (b in 1:length(betas)) {
      
      beta = betas[b]
      
      # write to progress text file
      update <- paste(Sys.time(), ' - ', scenario, 'C - beta ', beta, ' - ', num_sims, ' sims', sep = '')
      write(update, file = 'progress.txt', append = TRUE)
      
      # initialize yield and biomass arrays
      
      # initialize population size array by age class and sex
      sims_N <- array(rep(NA, times = 2 * A * Y * num_sims), 
                      dim = c(2, A, Y, num_sims))
      
      sims_abundance_F <- array(rep(NA, times = Y * num_sims), 
                              dim = c(Y, num_sims))  
      
      sims_abundance_M <- array(rep(NA, times = Y * num_sims), 
                                dim = c(Y, num_sims)) 
      
      sims_abundance_total <- array(rep(NA, times = Y * num_sims), 
                                dim = c(Y, num_sims)) 
      
      sims_mature_abundance <- array(rep(NA, times = Y * num_sims), 
                                     dim = c(Y, num_sims))  
      
      ##############################################################################
      
      # run the model for each simulation
      for (i in 1:num_sims) {
        
        output <- base_model(max_age, demographic_stochasticity, 
                             F_survival_years, F_survival_values, 
                             M_survival_years, M_survival_values, 
                             age_maturity, beta, remigration_int, 
                             nests_mu, nests_sd, eggs_mu, eggs_sd, 
                             hatch_success_mu, hatch_success_a, 
                             hatch_success_b, hatch_success_stochasticity, 
                             logit_a, logit_b, temp_mu, temp_sd, 
                             climate_stochasticity, start_year, end_year, scenario)
        
        # save the N and abundance arrays 
        sims_N[, , , i]             <- output[[1]]
        sims_abundance_F[, i]       <- output[[2]]
        sims_abundance_M[, i]       <- output[[3]]
        sims_abundance_total[, i]   <- output[[4]]
        sims_mature_abundance[, i]  <- output[[5]]
        
        # write to progress text file
        if (i %% (num_sims/10) == 0) {
          update <- paste(Sys.time(), ' - ', scenario, 'C - beta ', beta, ' - ', i/num_sims*100, '% done!', sep = '')
          write(update, file = 'progress.txt', append = TRUE)
          
        }
        
      }
      
      # get filepaths to save objects to
      filepath1 = paste('../output/', scenario, 'C/beta', beta, '/',  num_sims, 
                        '_N.Rda', sep = '')
      filepath2 = paste('../output/', scenario, 'C/beta', beta, '/',  num_sims, 
                        '_abundance_F.Rda', sep = '')
      filepath3 = paste('../output/', scenario, 'C/beta', beta, '/',  num_sims, 
                        '_abundance_M.Rda', sep = '')
      filepath4 = paste('../output/', scenario, 'C/beta', beta, '/',  num_sims, 
                        '_abundance_total.Rda', sep = '')
      filepath5 = paste('../output/', scenario, 'C/beta', beta, '/',  num_sims, 
                        '_mature_abundance.Rda', sep = '')
      # save objects
      save(sims_N, file = filepath1)
      save(sims_abundance_F, file = filepath2)
      save(sims_abundance_M, file = filepath3)
      save(sims_abundance_total, file = filepath4)
      save(sims_mature_abundance, file = filepath5)
      
    }
  
  }
  
}