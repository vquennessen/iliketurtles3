# run_base_model <- function(scenario) {

# run_base_model <- function(scenarios, num_sims, betas) {
run_base_model <- function(arguments) {
  
  ###### model inputs ##########################################################
  
  # function arguments
  model     <- arguments$Var1
  scenario  <- arguments$Var2
  beta      <- arguments$Var3
  years     <- arguments$Var4
  nsims     <- arguments$Var5
  intensity <- arguments$Var6
  frequency <- arguments$Var7
  
  # write to progress text file
  update <- paste(lubridate::now(), ' - ', model, ' - ', scenario, 'C - beta ', 
                  beta, ' - ', nsims, ' sims - ', years, ' years', sep = '')
  write(update, file = 'progress.txt', append = TRUE)
  
  # # troubleshooting
  # model <- 'P_base'
  # scenario <- 0.5
  # beta <- 43.7
  # nsims <- 10
  # 
  # model parameters to modulate
  temp_mu <- 31.8                         # base incubation temp mean
  climate_stochasticity <- TRUE           # whether or not to add in
  season_temp_sd <- 0.364                 # variance in temp at season level
  clutch_temp_sd <- 0.790                 # variance in temp at clutch level
  noise <- 'White'                        # noise: White or Red
  AC <- 0.5                               # autocorrelation coefficient
  
  # turtle demographics
  max_age <- 85                             # lifespan
  # max_age - (1 + 2 + 7 + 12)              # years for last ageclass
  F_survival_initial <- c(0.35, 0.8, 0.8, 0.85)        # F survival - immature
  f <- length(F_survival_initial)
  F_survival_immature <- c(F_survival_initial, 
                           rep(F_survival_initial[f], max_age - f))
  M_survival_mature <- 0.799                           # F survival - mature
  M_survival_initial <- c(0.35, 0.8, 0.8, 0.85)        # M survival - immature
  m <- length(M_survival_initial)
  M_survival_immature <- c(M_survival_initial, 
                           rep(M_survival_initial[m], max_age - m))
  F_survival_mature <- 0.799                           # M survival - mature
  age_maturity_mu <- 25                     # age at first reproduction, mean
  age_maturity_sd <- 2.5                    # age at first reproduction, SD
  F_remigration_int <- 3.87                 # remigration interval - females
  M_remigration_int <- 1.47                 # remigration interval - males
  clutches_mu <- 4.95                       # mean # of clutches/F/season
  clutches_sd <- 2.09                       # sd of # of clutches/F/season
  eggs_mu <- 100.58                         # mean number of eggs/clutch - 100.58
  eggs_sd <- 22.61                          # sd of number of eggs/clutch - 22.61
  emergence_success_A <- 0.86                   # logistic by temp - A
  emergence_success_k <- -1.7                   # logistic by temp - beta
  emergence_success_t0 <- 32.7                  # logistic by temp - t0
  T_piv <- 29.2                             # thermal reaction norm midpoint
  T_threshold <- 35                         # lethal temperature threshold
  
  # pivotal temperature and evolution stats
  k_piv <- ifelse(model %in% c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
                               'P_evol_threshold', 'P_evol_threshold_high_H', 
                               'P_conservation'), 
                  -1.34, 
                  -0.561)
  
  evolution_piv <- ifelse(model %in% c('P_evol_piv', 'P_evol_piv_high_H', 
                                       'GM_evol_piv', 'GM_evol_piv_high_H'), 
                          TRUE, 
                          FALSE)
  
  h2_piv <- ifelse(model %in% c('P_evol_piv', 'GM_evol_piv'), 
                   0.135, 
                   ifelse(model %in% c('P_evol_piv_high_H', 
                                       'GM_evol_piv_high_H'), 
                          0.351, 
                          NA))
  
  ag_var_piv <- ifelse(evolution_piv == TRUE, 
                       0.017, 
                       NA)
  
  # threshold evolution stats
  evolution_threshold <- ifelse(model %in% c('P_evol_threshold', 
                                             'P_evol_threshold_high_H', 
                                             'GM_evol_threshold', 
                                             'GM_evol_threshold_high_H'), 
                                TRUE, 
                                FALSE)
  
  h2_threshold <- ifelse(model %in% c('P_evol_threshold', 'GM_evol_threshold'), 
                         0.2, ifelse(model %in% c('P_evol_threshold_high_H', 
                                                  'GM_evol_threshold_high_H'), 
                                     0.38, 
                                     NA))
  
  ag_var_threshold <- ifelse(evolution_threshold == TRUE, 
                             0.017, 
                             NA)
  
  # conservation values
  conservation <- ifelse(model %in% c('P_conservation', 'GM_conservation'), 
                         TRUE, 
                         FALSE)
  
  effect_size <- ifelse(conservation == TRUE, 
                        1.3, 
                        NA)
  
  ##### initialize output ######################################################
  
  # initialize yield and biomass arrays
  
  # initialize population size array by sex/maturity, age, years, sims
  sims_N <- array(rep(NA, times = 4 * max_age * years * nsims), 
                  dim = c(4, max_age, years, nsims))
  
  sims_OSR <- array(rep(NA, times = years * nsims), 
                    dim = c(years, nsims)) 
  
  sims_piv <- array(rep(NA, times = years * nsims), 
                    dim = c(years, nsims))
  
  sims_threshold <- array(rep(NA, times = years * nsims), 
                          dim = c(years, nsims))
  
  ##### initialize population ##################################################
  
  # initial numbers of breeding adults by sex to find starting population size
  # based on the stable age distribution
  F_initial <- 170                          # initial adult F
  M_initial <- 30                           # initial adult M
  
  ##### maturity ogive
  M <- round(pnorm(q = 1:max_age, 
                   mean = age_maturity_mu, 
                   sd = age_maturity_sd), 
             3)
  
  ##### initial population size
  
  # stable age distribution
  SAD_output <- initialize_population(beta, burn_in = 1000, max_age, 
                                      F_survival_immature, F_survival_mature, 
                                      M_survival_immature, M_survival_mature, 
                                      M, F_remigration_int, M_remigration_int,
                                      clutches_mu, eggs_mu, emergence_success_A, 
                                      emergence_success_k, emergence_success_t0, 
                                      k_piv, T_piv, temp_mu, 
                                      F_initial, M_initial)
  
  # check to see if SAD exists or returns NaN - save everything as NA
  # and move on to the next combo
  
  if (is.na(sum(SAD_output[[1]] > 0)) | 
      is.na(sum(SAD_output[[2]] > 0)) |
      is.na(sum(SAD_output[[3]] > 0)) |
      is.na(sum(SAD_output[[4]]))) {
    
    # get filepaths to save objects to
    filepath1 = paste('../output/', model, '/', scenario, 'C/beta', beta, 
                      '/', nsims, '_N.Rda', sep = '')
    filepath2 = paste('../output/', model, '/', scenario, 'C/beta', beta, 
                      '/', nsims, '_OSR.Rda', sep = '')
    
    # save objects
    sims_N <- NULL
    save(sims_N, file = filepath1)
    
    sims_OSR <- NULL
    save(sims_OSR, file = filepath2)
    
    if (evolution_piv == TRUE) {
      
      filepath3 = paste('../output/', model, '/', scenario, 'C/beta', beta, 
                        '/',  nsims, '_piv.Rda', sep = '')
      
      sims_piv <- NULL
      save(sims_piv, file = filepath3)
      
    }
    
    if (evolution_threshold == TRUE) {
      
      filepath4 = paste('../output/', model, '/', scenario, 'C/beta', beta, 
                        '/',  nsims, '_threshold.Rda', sep = '')
      
      sims_threshold <- NULL
      save(sims_threshold, file = filepath4)
      
    }
    
  } else {
    
    # separate by sex
    F_Immature_init <- SAD_output[[1]]
    M_Immature_init <- SAD_output[[2]]
    F_Mature_init <- SAD_output[[3]]
    M_Mature_init <- SAD_output[[4]]
    
    ##### run sims and save output #############################################
    
    # run the model for each simulation
    for (i in 1:nsims) {
      
      output <- base_model(scenario, beta, years, max_age,
                           F_survival_immature, F_survival_mature, 
                           M_survival_immature, M_survival_mature,
                           F_Immature_init, M_Immature_init, 
                           F_Mature_init, M_Mature_init,
                           M, F_remigration_int, M_remigration_int,
                           clutches_mu, clutches_sd, eggs_mu, eggs_sd, 
                           emergence_success_A, emergence_success_k, 
                           emergence_success_t0, 
                           T_piv, k_piv, h2_piv, ag_var_piv, evolution_piv,
                           T_threshold, h2_threshold, ag_var_threshold, 
                           evolution_threshold,
                           temp_mu, climate_stochasticity, 
                           season_temp_sd, clutch_temp_sd, noise, AC, 
                           conservation, frequency, intensity, effect_size)
      
      # save the N and abundance arrays 
      sims_N[, , , i]             <- output[[1]]
      sims_OSR[, i]               <- output[[2]]
      sims_piv[, i]               <- output[[3]]
      sims_threshold[, i]         <- output[[4]]
      
      # write to progress text file
      if ((i/nsims*100) %% 10 == 0) {
        update <- paste(lubridate::now(), ' - ', model, ' - ', scenario, 
                        'C - beta ', beta, ' - ', nsims, ' sims - ', years, 
                        ' years - ',  i/nsims*100, '% done!', sep = '')
        write(update, file = 'progress.txt', append = TRUE)
        
      }
      
    }
    
    if (conservation == TRUE) {
      
      folder <- paste('/freq_', frequency, '_intensity_', intensity, sep = '')
      
    } else { folder <- ''}
    
    # get filepaths to save objects to
    filepath1 = paste('../output/', model, '/', scenario, 'C/beta', beta, 
                      folder, '/', nsims, '_N.Rda', sep = '')
    filepath2 = paste('../output/', model, '/', scenario, 'C/beta', beta, 
                      folder, '/', nsims, '_OSR.Rda', sep = '')
    
    # save objects
    save(sims_N, file = filepath1)
    save(sims_OSR, file = filepath2)
    
    if (evolution_piv == TRUE) {
      
      filepath3 = paste('../output/', model, '/', scenario, 'C/beta', beta, 
                        '/',  nsims, '_piv.Rda', sep = '')
      
      save(sims_piv, file = filepath3)
      
    }
    
    if (evolution_threshold == TRUE) {
      
      filepath4 = paste('../output/', model, '/', scenario, 'C/beta', beta, 
                        '/',  nsims, '_threshold.Rda', sep = '')
      save(sims_threshold, file = filepath4)
      
    }
    
  } 
  
}
