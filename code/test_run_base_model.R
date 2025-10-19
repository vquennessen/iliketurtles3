# run_base_model - test
  
  # load libraries
  library(parallel)
  library(dplyr)
  library(tidyr)
  library(magrittr)
  library(readr)
  library(lubridate)
  library(lgcp)
  
  source('base_model.R')
  source('initialize_arrays.R')
  source('initialize_population.R')
  source('reproduction.R')
  source('pop_dynamics.R')
  source('mating function/OSRs_to_betas.R')
  source('evolution.R')
  source('emergence_success.R')
  source('probability_male.R')
  source('conservation.R')
  
  # troubleshooting
  model     <- 'P_base'
  scenario  <- 1.5
  beta      <- 2
  yrs       <- 100
  nsims     <- 100
  intensity <- 1
  frequency <- 1
  
  # write to progress text file
  update <- paste(lubridate::now(), ' - ', model, ' - ', scenario, 'C - beta ', 
                  beta, ' - ', nsims, ' sims - ', yrs, ' years', sep = '')
  write(update, file = 'progress.txt', append = TRUE)
  
  # model parameters to modulate
  temp_mu <- 31.8                         # base incubation temp mean
  climate_stochasticity <- TRUE           # whether or not to add in
  season_temp_sd <- 0.364                 # variance in temp at season level
  clutch_temp_sd <- 0.790                 # variance in temp at clutch level
  noise <- 'Red'                          # noise: White or Red
  AC <- 0.5                               # autocorrelation coefficient
  
  # turtle demographics
  max_age <- 85
  F_survival_values <- c(0.35, 0.8, 0.8, 0.85, 0.799)
  M_survival_values <- c(0.35, 0.8, 0.8, 0.85, 0.799)
  
  F_years_in_stage <- c(1, 2, 7, 12, 63)# lifespan
  M_years_in_stage <- c(1, 2, 7, 12, 63)# lifespan
  
  # max_age - (1 + 2 + 7 + 12)              # years for last ageclass
  IF_survival <- c(rep(F_survival_values[1], F_years_in_stage[1]),
                   rep(F_survival_values[2], F_years_in_stage[2]),
                   rep(F_survival_values[3], F_years_in_stage[3]),
                   rep(F_survival_values[4],
                       (F_years_in_stage[4] + F_years_in_stage[5])))
  IM_survival <- c(rep(M_survival_values[1], M_years_in_stage[1]),
                   rep(M_survival_values[2], M_years_in_stage[2]),
                   rep(M_survival_values[3], M_years_in_stage[3]),
                   rep(M_survival_values[4],
                       (M_years_in_stage[4] + M_years_in_stage[5])))
  MF_survival <- 0.799
  MM_survival <- 0.799
  
  age_maturity_mu <- 25                     # age at first reproduction, mean
  age_maturity_sd <- 2.5                    # age at first reproduction, SD
  F_remigration_int <- 3.87                 # remigration interval - females
  M_remigration_int <- 1.47                 # remigration interval - males
  clutches_mu <- 4.95                       # mean # of clutches/F/season
  clutches_sd <- 2.09                       # sd of # of clutches/F/season
  eggs_mu <- 100.58                         # mean number of eggs/clutch - 100.58
  eggs_sd <- 22.61                          # sd of number of eggs/clutch - 22.61
  emergence_success_A <- 0.86               # logistic by temp - A
  emergence_success_k <- -1.7               # logistic by temp - beta
  emergence_success_t0 <- 32.7              # logistic by temp - t0
  T_piv <- 29.4                             # thermal reaction norm midpoint
  T_threshold <- 35                         # lethal temperature threshold
  
  # pivotal temperature and evolution stats
  k_piv <- ifelse(model %in% c('P_base', 'P_evol_piv', 'P_evol_piv_high_H',
                               'P_evol_threshold', 'P_evol_threshold_high_H',
                               'P_conservation'),
                  # -1.34,
                  -1.54,
                  # -0.561
                  -0.77)
  
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
  conservation_action <- ifelse(model %in% c('P_conservation',
                                             'GM_conservation'),
                                TRUE,
                                FALSE)
  
  effect_size <- ifelse(conservation_action == TRUE,
                        1.3,
                        NA)
  
  ##### initialize output ######################################################
  
  # initialize yield and biomass arrays
  
  # initialize population size array by sex/maturity, age, years, sims
  sims_N <- array(rep(NA, times = 4 * max_age * yrs * nsims),
                  dim = c(4, max_age, yrs, nsims))
  
  sims_OSR <- array(rep(NA, times = yrs * nsims),
                    dim = c(yrs, nsims))
  
  sims_piv <- array(rep(NA, times = yrs * nsims),
                    dim = c(yrs, nsims))
  
  sims_threshold <- array(rep(NA, times = yrs * nsims),
                          dim = c(yrs, nsims))
  
  ##### initialize population ##################################################
  
  # # initial numbers of breeding adults by sex to find starting population size
  # # based on the stable age distribution
  # F_initial <- 170                          # initial adult F
  # M_initial <- 30                           # initial adult M
  
  ##### maturity ogive
  M <- round(pnorm(q = 1:max_age,
                   mean = age_maturity_mu,
                   sd = age_maturity_sd),
             3)
  
  ##### initial population size
  
  # # stable age distribution
  # SAD_output <- initialize_population(beta, burn_in = 1000, max_age,
  #                                     IF_survival, IM_survival,
  #                                     MF_survival, MM_survival,
  #                                     M, F_remigration_int, M_remigration_int,
  #                                     clutches_mu, eggs_mu, emergence_success_A,
  #                                     emergence_success_k, emergence_success_t0,
  #                                     k_piv, T_piv, temp_mu,
  #                                     F_initial, M_initial)
  
  load("~/Projects/iliketurtles3/output/SAD_n1000_b200.Rdata")
  # load("/home/quennessenv/iliketurtles3/output/SAD_n1000_b200.Rdata")
  
  init_age_distribution <- SADdf %>%
    filter(!is.na(Proportion)) %>%
    filter(!is.na(Abundance)) %>%
    group_by(Model, Beta, Sex, Age) %>%
    summarise(prop_median = median(Proportion, na.rm = TRUE),
              prop_Q25 = quantile(Proportion, probs = 0.25, na.rm = TRUE),
              prop_Q75 = quantile(Proportion, probs = 0.75, na.rm = TRUE),
              abundance_median = median(Abundance, na.rm = TRUE),
              abundance_Q25 = quantile(Abundance, probs = 0.25, na.rm = TRUE),
              abundance_Q75 = quantile(Abundance, probs = 0.75, na.rm = TRUE),
              adj_abun_median = median(Adjusted_Abundance, na.rm = TRUE),
              adj_abun_Q25 = quantile(Adjusted_Abundance, probs = 0.25,
                                      na.rm = TRUE),
              adj_abun_Q75 = quantile(Adjusted_Abundance, probs = 0.75,
                                      na.rm = TRUE),
              .groups = 'drop')
  
  IAD <- init_age_distribution %>%
    filter(Beta == beta) %>%
    filter(Model == model) 
  
  IF_init <- IAD %>%
    filter(Sex == 'IF') %>%
    pull(adj_abun_median)
  
  IM_init <- IAD %>%
    filter(Sex == 'IM') %>%
    pull(adj_abun_median)
  
  MF_init <- IAD %>%
    filter(Sex == 'MF') %>%
    pull(adj_abun_median)
  
  MM_init <- IAD %>%
    filter(Sex == 'MM') %>%
    pull(adj_abun_median)
  
  # # check to see if SAD exists or returns NaN - save everything as NA
  # # and move on to the next combo
  #
  # if (sum(is.na(SAD_output[[1]] > 0)) |
  #     sum(is.na(SAD_output[[2]] > 0)) |
  #     sum(is.na(SAD_output[[3]] > 0)) |
  #     sum(is.na(SAD_output[[4]] > 0))) {
  #
  #   # get filepaths to save objects to
  #   filepath1 = paste('../output/', model, '/', scenario, 'C/beta', beta,
  #                     '/', nsims, '_N.Rda', sep = '')
  #   filepath2 = paste('../output/', model, '/', scenario, 'C/beta', beta,
  #                     '/', nsims, '_OSR.Rda', sep = '')
  #
  #   # save objects
  #   sims_N <- NULL
  #   save(sims_N, file = filepath1)
  #
  #   sims_OSR <- NULL
  #   save(sims_OSR, file = filepath2)
  #
  #   if (evolution_piv == TRUE) {
  #
  #     filepath3 = paste('../output/', model, '/', scenario, 'C/beta', beta,
  #                       '/',  nsims, '_piv.Rda', sep = '')
  #
  #     sims_piv <- NULL
  #     save(sims_piv, file = filepath3)
  #
  #   }
  #
  #   if (evolution_threshold == TRUE) {
  #
  #     filepath4 = paste('../output/', model, '/', scenario, 'C/beta', beta,
  #                       '/',  nsims, '_threshold.Rda', sep = '')
  #
  #     sims_threshold <- NULL
  #     save(sims_threshold, file = filepath4)
  #
  #   }
  #
  #   update <- paste(lubridate::now(), ' - ', model, ' - ', scenario,
  #                   'C - beta ', beta, ' - ', nsims, ' sims - ', years,
  #                   ' years - no SAD, all done!', sep = '')
  #   write(update, file = 'progress.txt', append = TRUE)
  #
  # } else {
  #
  #   # separate by sex
  #   IF_init <- SAD_output[[1]]
  #   IM_init <- SAD_output[[2]]
  #   MF_init <- SAD_output[[3]]
  #   MM_init <- SAD_output[[4]]
  
  ##### run sims and save output #############################################
  
  # run the model for each simulation
  for (i in 1:nsims) {
    
    output <- base_model(scenario, beta, yrs, max_age,
                         IF_survival, IM_survival, MF_survival, MM_survival,
                         IF_init, IM_init, MF_init, MM_init,
                         M, F_remigration_int, M_remigration_int,
                         clutches_mu, clutches_sd, eggs_mu, eggs_sd,
                         emergence_success_A, emergence_success_k,
                         emergence_success_t0,
                         T_piv, k_piv, h2_piv, ag_var_piv, evolution_piv,
                         T_threshold, h2_threshold, ag_var_threshold,
                         evolution_threshold,
                         temp_mu, climate_stochasticity,
                         season_temp_sd, clutch_temp_sd, noise, AC,
                         conservation_action, frequency, intensity,
                         effect_size)
    
    # save the N and abundance arrays
    sims_N[, , , i]             <- output[[1]]
    sims_OSR[, i]               <- output[[2]]
    sims_piv[, i]               <- output[[3]]
    sims_threshold[, i]         <- output[[4]]
    
    # write to progress text file
    if ((i/nsims*100) %% 10 == 0) {
      update <- paste(lubridate::now(), ' - ', model, ' - ', scenario,
                      'C - beta ', beta, ' - ', nsims, ' sims - ', yrs,
                      ' years - ',  i/nsims*100, '% done!', sep = '')
      write(update, file = 'progress.txt', append = TRUE)
      
    }
    
  }
  
  if (conservation_action == TRUE) {
    
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
  