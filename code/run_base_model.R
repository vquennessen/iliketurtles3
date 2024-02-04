# run_base_model <- function(scenario) {

# run_base_model <- function(scenarios, num_sims, betas) {
run_base_model <- function(arguments) {
  
  
  ###### model inputs ##########################################################
  
  # turtle demographics
  max_age <- 85                                         # lifespan
  F_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - F
  F_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - F
  M_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - M
  M_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - M
  demographic_stochasticity <- TRUE         # demographic stochasticity
  age_maturity <- 23                        # age at first reproduction
  F_remigration_int <- 3.87                 # remigration interval - females
  M_remigration_int <- 1.47                 # remigration interval - males
  nests_mu <- 4.95                          # mean # of nests/F/season
  nests_sd <- 2.09                          # sd of # of nests/F/season
  eggs_mu <- 100.65                         # mean number of eggs/nest
  eggs_sd <- 23.08                          # sd of number of eggs/nest
  hatch_success_mu <- 0.8241024             # mean of hatching success
  hatch_success_a <- 2.089414               # stochastic beta CDF shape 1 par
  hatch_success_b <- 0.4496393              # stochastic beta CDF shape 2 par
  hatch_success_A <- 0.86                   # logistic by temp - A
  hatch_success_beta <- -1.7                # logistic by temp - beta
  hatch_success_t0 <- 32.7                  # logistic by temp - t0
  T_piv <- 29.2                             # thermal reaction norm midpoint
  k <- -1.4                                 # thermal reaction norm slope
  F_initial <- 170                          # initial adult F
  M_initial <- 30                           # initial adult M
  
  # climate data
  temp_mu <- 31.80                          # base incubation temp mean
  temp_sd <- 0.84                           # base incubation temp sd
  climate_stochasticity <- FALSE            # whether or not to add in
  
  # model parameters
  start_year <- 2023                        # first year to simulate
  end_year <- start_year + 3*max_age        # last year to simulate
  evolution <- TRUE                         # whether evolution is turned on
  
  # dimensions
  A <- max_age
  Y <- length(start_year:end_year)
  
  ##############################################################################
  
  scenario <- arguments[[1]]
  beta     <- arguments[[2]]
  nsims    <- arguments[[3]]
  
  # write to progress text file
  update <- paste(Sys.time(), ' - ', scenario, 'C - beta ', beta, ' - ', 
                  nsims, ' sims', sep = '')
  write(update, file = 'progress.txt', append = TRUE)
  
  # initialize yield and biomass arrays
  
  # initialize population size array by age class and sex
  sims_N <- array(rep(NA, times = 2 * A * Y * nsims), 
                  dim = c(2, A, Y, nsims))
  
  sims_abundance_F <- array(rep(NA, times = Y * nsims), 
                            dim = c(Y, nsims))  
  
  sims_abundance_M <- array(rep(NA, times = Y * nsims), 
                            dim = c(Y, nsims)) 
  
  sims_abundance_total <- array(rep(NA, times = Y * nsims), 
                                dim = c(Y, nsims)) 
  
  sims_mature_abundance <- array(rep(NA, times = Y * nsims), 
                                 dim = c(Y, nsims))  
  
  ########################################################################
  
  # run the model for each simulation
  for (i in 1:nsims) {
    
    output <- base_model(max_age, demographic_stochasticity, 
                         F_survival_years, F_survival_values, 
                         M_survival_years, M_survival_values, 
                         age_maturity, F_remigration_int, M_remigration_int,
                         nests_mu, nests_sd, eggs_mu, eggs_sd, 
                         hatch_success_mu, hatch_success_a, 
                         hatch_success_b,
                         T_piv, k, temp_mu, temp_sd, climate_stochasticity, 
                         start_year, end_year, scenario, beta)
    
    # save the N and abundance arrays 
    sims_N[, , , i]             <- output[[1]]
    sims_abundance_F[, i]       <- output[[2]]
    sims_abundance_M[, i]       <- output[[3]]
    sims_abundance_total[, i]   <- output[[4]]
    sims_mature_abundance[, i]  <- output[[5]]
    
    # write to progress text file
    if (round(i/nsims*100) %% 5 == 0) {
      update <- paste(Sys.time(), ' - ', scenario, 'C - beta ', beta, 
                      ' - ', nsims, ' sims - ', i/nsims*100, '% done!', 
                      sep = '')
      write(update, file = 'progress.txt', append = TRUE)
      
    }
    
  }
  
  # get filepaths to save objects to
  filepath1 = paste('../output/', scenario, 'C/beta', beta, '/',  nsims, 
                    '_N.Rda', sep = '')
  filepath2 = paste('../output/', scenario, 'C/beta', beta, '/',  nsims, 
                    '_abundance_F.Rda', sep = '')
  filepath3 = paste('../output/', scenario, 'C/beta', beta, '/',  nsims, 
                    '_abundance_M.Rda', sep = '')
  filepath4 = paste('../output/', scenario, 'C/beta', beta, '/',  nsims, 
                    '_abundance_total.Rda', sep = '')
  filepath5 = paste('../output/', scenario, 'C/beta', beta, '/',  nsims, 
                    '_mature_abundance.Rda', sep = '')
  # save objects
  save(sims_N, file = filepath1)
  save(sims_abundance_F, file = filepath2)
  save(sims_abundance_M, file = filepath3)
  save(sims_abundance_total, file = filepath4)
  save(sims_mature_abundance, file = filepath5)
  
}

#     }
#     
#   }
#   
# }