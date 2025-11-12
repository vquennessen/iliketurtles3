# run_base_model <- function(scenario) {

# run_base_model <- function(scenarios, num_sims, betas) {
run_base_model <- function(arguments) {
  
  ###### model inputs ##########################################################
  
  # function arguments
  folder       <- arguments$Var1
  noise        <- arguments$Var2
  TRT          <- arguments$Var3
  scenario     <- arguments$Var4
  beta         <- arguments$Var5
  yrs          <- arguments$Var6
  nsims        <- arguments$Var7
  max_N        <- arguments$Var8
  init_age_distribution <- arguments$Var9
  evolution    <- arguments$Var10
  trait        <- arguments$Var11 
  rate         <- arguments$Var12
  conservation_action <- arguments$Var13
  intensity    <- arguments$Var14
  frequency    <- arguments$Var15
  
  # write to progress text file
  # write to progress text file
  start <- lubridate::now()
  time1 <- format(start)
  update1 <- paste(time1, ' - ', TRT, ' - ', scenario, 'C - beta ', beta, 
                   ' - ', nsims, ' sims - ', yrs, ' years',sep = '')
  write(update1, file = '../output/progress.txt', append = TRUE)
  
  # model parameters to modulate
  temp_mu <- 31.8                         # base incubation temp mean
  climate_stochasticity <- TRUE           # whether or not to add in
  season_temp_sd <- 0.364                 # variance in temp at season level
  clutch_temp_sd <- 0.790                 # variance in temp at clutch level
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
  k_piv <- ifelse(TRT == 'narrow',
                  -1.54,
                  -0.77)
  
  if (evolution == TRUE) {
    
    if (trait == 'T_piv') {
      
      if (rate == '') {
        
        h2 <- 0.221
        varGenetic <- 0.926
        
      } else {
        
        h2 <- 0.576
        varGenetic <- 2.41
        
      }
    
    # or, if the evolvable trait is the emergence success midpoint (t0)
    } else {
      
      if (rate == '') {
        
        h2 <- 0.75
        # varGenetic <- 
        
      } else {
        
        h2 <- 0.88
        # varGenetic <- 
        
      }
      
    }
    
  } else {
    
    h2 <- NULL
    varGenetic <- NULL
    
  }
      
  # if conservation is TRUE    
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
  
  # stable age distributions to start with
  IAD <- init_age_distribution %>%
    filter(Beta == beta) %>%
    filter(TRangeT == TRT)
  
  IF_init <- IAD %>%
    filter(Sex == 'IF') %>%
    pull(Abundance)
  
  IM_init <- IAD %>%
    filter(Sex == 'IM') %>%
    pull(Abundance)
  
  MF_init <- IAD %>%
    filter(Sex == 'MF') %>%
    pull(Abundance)
  
  MM_init <- IAD %>%
    filter(Sex == 'MM') %>%
    pull(Abundance)
  
  # check to see if SAD exists or returns NaN - save everything as NA
  # and move on to the next combo
  
  if (sum(IM_init, MM_init) < 1 |
      sum(!is.finite(IAD$Abundance)) > 0) {
    
    # get filepaths to save objects to
    filepath1 = paste('../output/', folder, model, '/', scenario, 'C/beta', beta,
                      '/', nsims, '_N.Rda', sep = '')
    filepath2 = paste('../output/', folder, model, '/', scenario, 'C/beta', beta,
                      '/', nsims, '_OSR.Rda', sep = '')
    
    # save objects
    sims_N <- NULL
    save(sims_N, file = filepath1)
    
    sims_OSR <- NULL
    save(sims_OSR, file = filepath2)
    
    if (evolution_piv == TRUE) {
      
      filepath3 = paste('../output/', folder, model, '/', scenario, 'C/beta', beta,
                        '/',  nsims, '_piv.Rda', sep = '')
      
      sims_piv <- NULL
      save(sims_piv, file = filepath3)
      
    }
    
    if (evolution_threshold == TRUE) {
      
      filepath4 = paste('../output/', folder, model, '/', scenario, 'C/beta', beta,
                        '/',  nsims, '_threshold.Rda', sep = '')
      
      sims_threshold <- NULL
      save(sims_threshold, file = filepath4)
      
    }
    
    update <- paste(lubridate::now(), ' - ', model, ' - ', scenario,
                    'C - beta ', beta, ' - ', nsims, ' sims - ', years,
                    ' years - no SAD, all done!', sep = '')
    write(update, file = '../output/progress.txt', append = TRUE)
    
  } else {
    
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
        time2 <- format(lubridate::now())
        update2 <- paste(time2, ' - ', model, ' - ', scenario,
                        'C - beta ', beta, ' - ', nsims, ' sims - ', yrs,
                        ' years - ',  i/nsims*100, '% done!', sep = '')
        write(update2, file = '../output/progress.txt', append = TRUE)
        
      }
      
    }
    
    if (conservation_action == TRUE) {
      
      folder2 <- paste('/freq_', frequency, '_intensity_', intensity, sep = '')
      
    } else { folder2 <- ''}
    
    # get filepaths to save objects to
    filepath1 = paste('../output/', folder, model, '/', scenario, 'C/beta', beta,
                      folder2, '/', nsims, '_N.Rda', sep = '')
    filepath2 = paste('../output/', folder, model, '/', scenario, 'C/beta', beta,
                      folder2, '/', nsims, '_OSR.Rda', sep = '')
    
    # save objects
    save(sims_N, file = filepath1)
    save(sims_OSR, file = filepath2)
    
    if (evolution_piv == TRUE) {
      
      filepath3 = paste('../output/', folder, model, '/', scenario, 'C/beta', 
                        beta, '/',  nsims, '_piv.Rda', sep = '')
      
      save(sims_piv, file = filepath3)
      
    }
    
    if (evolution_threshold == TRUE) {
      
      filepath4 = paste('../output/', folder, model, '/', scenario, 'C/beta', 
                        beta, '/',  nsims, '_threshold.Rda', sep = '')
      save(sims_threshold, file = filepath4)
      
    }
    
    # update progress text file with total time it took to run the thing
    end <- lubridate::now()
    total_time <- format(round(end - start, 3))
    update3 <- paste(model, ' - ', scenario,
                     'C - beta ', beta, ' - ', nsims, ' sims - ', yrs,
                     ' years - total time: ', total_time, '\n', sep = '')
    write(update3, 
          file = '../output/progress.txt', append = TRUE)
    
  } 
  
}
