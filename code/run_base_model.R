# run_base_model <- function(scenario) {

# run_base_model <- function(scenarios, num_sims, betas) {
run_base_model <- function(arguments) {
  
  
  ###### model inputs ##########################################################
  
  # function arguments
  scenario <- arguments[[1]]
  beta     <- arguments[[2]]
  nsims    <- arguments[[3]]
  
  # scenario <- 0.5
  # beta <- 1
  # nsims <- 100
  
  # turtle demographics
  max_age <- 85                                         # lifespan
  F_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - F
  F_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - F
  M_survival_years <- c(1, 2, 7, 12, 1)                 # years per stage - M
  M_survival_values <- c(0.35, 0.8, 0.85, 0.85, 0.799)  # survival per stage - M
  age_maturity_mu <- 25                     # age at first reproduction, mean
  age_maturity_sd <- 2.5                    # age at first reproduction, SD
  F_remigration_int <- 3.87                 # remigration interval - females
  M_remigration_int <- 1.47                 # remigration interval - males
  nests_mu <- 4.95                          # mean # of nests/F/season
  nests_sd <- 2.09                          # sd of # of nests/F/season
  eggs_mu <- 100.65                         # mean number of eggs/nest
  eggs_sd <- 23.08                          # sd of number of eggs/nest
  hatch_success_A <- 0.86                   # logistic by temp - A
  hatch_success_k <- -1.7                   # logistic by temp - beta
  hatch_success_t0 <- 32.7                  # logistic by temp - t0
  T_piv <- 29.2                             # thermal reaction norm midpoint
  k <- -1.4                                 # thermal reaction norm slope
  F_initial <- 170                          # initial adult F
  M_initial <- 30                           # initial adult M
  
  # climate data
  temp_mu <- 31.80                          # base incubation temp mean
  temp_sd <- 0.84                           # base incubation temp sd
  
  # evolution data
  H <- 0.13                                 # heritability
  ag_var <- 0.01                          # phenotypic variance
  
  # model parameters and dimensions
  years <- 100                              # number of years to simulate
  evolution <- TRUE                         # whether evolution is turned on
  climate_stochasticity <- FALSE            # whether or not to add in

  A <- max_age
  Y <- years
  
  
  ##### derived arrays #########################################################
  
  ##### maturity ogive
  M <- pnorm(q = 1:max_age, mean = age_maturity_mu, sd = age_maturity_sd)
  
  ##### initial population size
  
  # survival values vector - females
  F_survival <- rep(F_survival_values, times = F_survival_years)
  
  # survival values vector - males
  M_survival <- rep(M_survival_values, times = M_survival_years)
  
  # check it's long enough, and if not, add the last survival_value until it is
  # females
  if (length(F_survival) < A) {
    F_survival <- c(F_survival, rep(F_survival_values[length(F_survival_values)], 
                                    A - length(F_survival)))
  }
  
  # males
  if (length(M_survival) < A) {
    M_survival <- c(M_survival, 
                    rep(M_survival_values[length(M_survival_values)], 
                        A - length(M_survival)))
  }
  
  # make female leslie matrix for survival
  f_matrix <- matrix(diag(F_survival[1:(A - 1)]), ncol = A - 1)
  f_Leslie <- rbind(rep(0, A), cbind(f_matrix, rep(0, A - 1)))
  
  # make male leslie matrix for survival
  m_matrix <- matrix(diag(M_survival[1:(A - 1)]), ncol = A - 1)
  m_Leslie <- rbind(rep(0, A), cbind(m_matrix, rep(0, A - 1)))

  # stable age distribution
  SAD <- initialize_population(beta, burn_in = 1000, max_age, M, 
                               F_remigration_int, M_remigration_int,
                               nests_mu, eggs_mu, hatch_success_A, 
                               hatch_success_k, hatch_success_t0, 
                               k, T_piv, temp_mu, f_Leslie, m_Leslie)
  
  # separate by sex
  F_SAD <- filter(SAD, Sex == 'Female')
  M_SAD <- filter(SAD, Sex == 'Male')
  
  # set first timestep to SAD times a value to get at least 30 adult males
  # and 170 adult females
  f_min <- F_initial / sum((F_SAD$N[1:max_age])*M)
  m_min <- M_initial / sum((M_SAD$N[1:max_age])*M)
  multiplicator <- max(m_min, f_min)
  
  F_init <- F_SAD$N * multiplicator
  M_init <- M_SAD$N * multiplicator
  
  ##############################################################################
  
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
    
    output <- base_model(scenario, beta, years, A, Y,
                         max_age, F_survival, M_survival, F_init, M_init, 
                         M, F_remigration_int, M_remigration_int,
                         nests_mu, nests_sd, eggs_mu, eggs_sd, 
                         hatch_success_A, hatch_success_k, 
                         hatch_success_t0, T_piv, k, H, ag_var, evolution,
                         temp_mu, temp_sd, climate_stochasticity)
    
    # save the N and abundance arrays 
    sims_N[, , , i]             <- output[[1]]
    sims_abundance_F[, i]       <- output[[2]]
    sims_abundance_M[, i]       <- output[[3]]
    sims_abundance_total[, i]   <- output[[4]]
    sims_mature_abundance[, i]  <- output[[5]]
    
    # write to progress text file
    if ((i/nsims*100) %% 10 == 0) {
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
