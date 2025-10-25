### machine runs

initialize_population_test2 <- function(arguments) {
  
  # pull model parameters from arguments list (varies)
  model              <- arguments$Var1
  beta               <- arguments$Var2
  burn_in            <- arguments$Var3
  temp_stochasticity <- arguments$Var4
  nsims              <- arguments$Var5
  
  # model parameters (always the same)
  sexes   <- c('IF', 'IM', 'MF', 'MM')
  S <- length(sexes)
  
  # temp stochasticity for file names
  TS1 <- ifelse(temp_stochasticity == TRUE, ' - TS - ', ' - ')
  TS2 <- ifelse(temp_stochasticity == TRUE, 'TS_', '')
  
  # model parameters to modulate
  temp_mu <- 31.8                         # base incubation temp mean
  clutch_temp_sd <- 0.790                 # variance in temp at clutch level
  
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
  eggs_mu <- 100.58                         # mean # of eggs/clutch - 100.58
  emergence_success_A <- 0.86               # logistic by temp - A
  emergence_success_k <- -1.7               # logistic by temp - beta
  emergence_success_t0 <- 32.7              # logistic by temp - t0
  T_piv <- 29.4                             # thermal reaction norm midpoint
  T_threshold <- 35                         # lethal temperature threshold
  
  # thermal reaction norm values
  k_piv <- ifelse(model %in% c('P_base', 'P_evol_piv', 'P_evol_piv_high_H', 
                               'P_evol_threshold', 'P_evol_threshold_high_H', 
                               'P_conservation'), 
                  -1.54, 
                  -0.77)
  
  ##### maturity ogive
  M <- round(pnorm(q = 1:max_age, 
                   mean = age_maturity_mu, 
                   sd = age_maturity_sd), 
             3)  
  
  ##### initialize population ##################################################
  
  # initialize N dataframe
  N <- array(rep(0, times = 4 * max_age * burn_in), 
             dim = c(4, max_age, burn_in))
  
  # immature ogive
  Mi <- 1 - M
  
  # write to progress text file
  start <- lubridate::now()
  time1 <- format(start)
  update1 <- paste(time1, ' - ', model, TS1, 'beta ', beta, ' - ', burn_in, 
                   ' burn-in years - ', nsims, ' sims', sep = '')
  write(update1, file = '../output/SAD_progress.txt', append = TRUE)
  
  # initial pop size
  N[1, 1, 1] <- 1000
  N[2, 1, 1] <- 100
  N[1, 2:max_age, 1] <- round(100 * Mi[2:max_age])
  N[2, 2:max_age, 1] <- round(5 * Mi[2:max_age])
  N[3, 2:max_age, 1] <- round(10 * M[2:max_age])
  N[4, 2:max_age, 1] <- round(1 * M[2:max_age])
  
  # initialize results dataframe
  SAD <- data.frame(Model = rep(model, each = S * max_age), 
                    Beta = rep(beta, each = S * max_age),
                    Year = rep(1, times = S * max_age),
                    Sex = rep(sexes, each = max_age),
                    Age = rep(1:max_age, times = S),
                    Abundance = NA,
                    Proportion = NA,
                    PSR = NA, 
                    OSR = NA
  )
  
  # abundance by age
  SAD$Abundance <- c(N[1, , 1], N[2, , 1], N[3, , 1], N[4, , 1])
  
  # total population size in year 1
  total <- sum(N[, , 1], na.rm = TRUE)
  
  # add proportions to SAD
  SAD$Proportion <- c(N[1, , 1]/total, 
                      N[2, , 1]/total, 
                      N[3, , 1]/total, 
                      N[4, , 1]/total)
  
  SAD$PSR <- rep(sum(N[2, 1, 1], na.rm = TRUE) / 
                   sum(N[1:2, 1, 1], na.rm = TRUE), 
                 times = S * max_age)
  
  for (i in 1:nsims) {
    
    if (temp_stochasticity == FALSE) {
      
      # calculate expected emergence success (no temp stochasticity)
      emergence_success <- emergence_success_A / 
        (1 + exp(-emergence_success_k * (temp_mu - emergence_success_t0)))
      
      # probability of each hatchling developing as male (no temp stochasticity)
      probs_male <- 1 / (1 + exp(-k_piv * (temp_mu - (T_piv))))
      
      # move population forward in time burn_in years
      for (y in 2:burn_in) {
        
        # initialize results dataframe
        sub_SAD <- data.frame(Model = rep(model, S * max_age), 
                              Beta = rep(beta, S * max_age),
                              Year = rep(y, S * max_age),
                              Sex = rep(sexes, each = max_age),
                              Age = rep(1:max_age, times = S),
                              Abundance = NA, 
                              Proportion = NA,,
                              Prop_10yr_median = NA, 
                              PSR = NA,
                              OSR = NA
        )
        
        # population dynamics
        
        # immature females that survived
        immature_survived_F <- round(
          (N[1, 1:(max_age - 1), y - 1]) * IF_survival[1:(max_age - 1)])
        
        # immature females that matured
        new_mature_F <- round(immature_survived_F * M[1:(max_age - 1)])
        
        # updated immature female population
        N[1, 2:max_age, y] <- as.numeric(immature_survived_F) -
          as.numeric(new_mature_F)
        
        # mature females that survived
        mature_survived_F <- round(N[3, 1:(max_age - 1), y - 1] * MF_survival)
        
        # updated mature female population
        N[3, 2:max_age, y] <- mature_survived_F + new_mature_F
        
        # immature males that survived
        immature_survived_M <- round(
          (N[2, 1:(max_age - 1), y - 1]) * IM_survival[1:(max_age - 1)])
        
        # immature males that matured
        new_mature_M <- round(immature_survived_M * M[1:(max_age - 1)])
        
        # updated immature male population
        N[2, 2:max_age, y] <- immature_survived_M - new_mature_M
        
        # mature males that survived
        mature_survived_M <- round(N[4, 1:(max_age - 1), y - 1] * MM_survival)
        
        # updated mature male population
        N[4, 2:max_age, y] <- mature_survived_M + new_mature_M
        
        # breeding females this year
        n_available_F <- round(
          sum(mature_survived_F, na.rm = TRUE) / F_remigration_int)
        
        # breeding males this year
        n_available_M <- round(
          sum(mature_survived_M, na.rm = TRUE) / M_remigration_int)
        
        # check that there are at least 1 available male and female for breeding
        if (n_available_F < 1 | n_available_M < 1) {
          
          female_hatchlings <- 0
          male_hatchlings <- 0 
          hatchlings <- 0
          PSR <- 0
          
        } else {
          
          # operational sex ratio - proportion of males
          OSR <- n_available_M / (n_available_M + n_available_F)
          
          # calculate reproductive success
          breeding_success <- pbeta(2 * OSR, shape1 = 1, shape2 = beta) 
          
          # how many females actually find a male to mate with and then nest
          n_breeding_F <- round(n_available_F * breeding_success)
          
          # check that there is at least one female that will successfully breed
          if (n_breeding_F < 1) {
            
            female_hatchlings <- 0
            male_hatchlings <- 0 
            hatchlings <- 0
            PSR <- 0
            
          } else {
            
            # number of clutches total
            clutches <- sum(round(rep(clutches_mu, times = n_breeding_F)), 
                            na.rm = TRUE)
            
            # number of male hatchlings
            male_hatchlings <- clutches * round(
              round(round(eggs_mu) * emergence_success) * probs_male)
            
            # number of female hatchlings
            # female_hatchlings <- sum(hatchlings) - male_hatchlings
            female_hatchlings <- clutches * round(
              round(round(eggs_mu) * emergence_success) * (1 - probs_male))
            
          }
          
        }
        
        # add hatchlings to N
        N[1, 1, y] <- female_hatchlings
        N[2, 1, y] <- male_hatchlings
        
        # add population abundances
        sub_SAD$Abundance <- c(N[1, , y], N[2, , y], N[3, , y], N[4, , y])
        
        # total population size
        total <- sum(N[, , y], na.rm = TRUE)
        
        # add proportions to SAD
        sub_SAD$Proportion <- c(N[1, , y]/total, 
                                N[2, , y]/total, 
                                N[3, , y]/total, 
                                N[4, , y]/total)
        
        sub_SAD$PSR <- rep(male_hatchlings / (male_hatchlings + female_hatchlings), 
                           times = S * max_age)
        
        sub_SAD$OSR <- rep(OSR, times = S * max_age)
        
        # add subset to big df
        SAD <- rbind(SAD, sub_SAD)
        
        # print(y)
        # 10 percent updates
        # write to progress text file, every 10% done
        if ((i/nsims*100) %% 1 == 0) {
          time2 <- format(lubridate::now())
          update2 <- paste(time2, ' - ', model, ' - beta ', 
                           beta, ' - ', burn_in, ' burn-in years', ' - ', nsims, 
                           ' sims - ', i/nsims*100, '% sims done!', sep = '')
          write(update2, file = '../output/SAD_progress.txt', append = TRUE)      
          
          # break out of loop if there are zero males at any age
          if (sum(N[2, , y], na.rm = TRUE) < 1 & 
              sum(N[4, , y], na.rm = TRUE) < 1) { break }
          
        }
        
      }
      
      # otherwise, if there is temp stochasticity:
    } else {
      
      # move population forward in time burn_in years
      for (y in 2:burn_in) {  
        
        # initialize results dataframe
        sub_SAD <- data.frame(Model = rep(model, S * max_age), 
                              Beta = rep(beta, S * max_age),
                              Year = rep(y, S * max_age),
                              Sex = rep(sexes, each = max_age),
                              Age = rep(1:max_age, times = S),
                              Abundance = NA,
                              Proportion = NA, 
                              PSR = NA,
                              OSR = NA 
        )
        
        # population dynamics
        
        # immature females that survived
        immature_survived_F <- round(
          (N[1, 1:(max_age - 1), y - 1]) * IF_survival[1:(max_age - 1)])
        
        # immature females that matured
        new_mature_F <- round(immature_survived_F * M[1:(max_age - 1)])
        
        # updated immature female population
        N[1, 2:max_age, y] <- immature_survived_F - new_mature_F
        
        # mature females that survived
        mature_survived_F <- round(N[3, 1:(max_age - 1), y - 1] * MF_survival)
        
        # updated mature female population
        N[3, 2:max_age, y] <- mature_survived_F + new_mature_F
        
        # immature males that survived
        immature_survived_M <- round(
          (N[2, 1:(max_age - 1), y - 1]) * IM_survival[1:(max_age - 1)])
        
        # immature males that matured
        new_mature_M <- round(immature_survived_M * M[1:(max_age - 1)])
        
        # updated immature male population
        N[2, 2:max_age, y] <- immature_survived_M - new_mature_M
        
        # mature males that survived
        mature_survived_M <- round(N[4, 1:(max_age - 1), y - 1] * MM_survival)
        
        # updated mature male population
        N[4, 2:max_age, y] <- mature_survived_M + new_mature_M
        
        # breeding females this year
        n_available_F <- round(
          sum(mature_survived_F, na.rm = TRUE) / F_remigration_int)
        
        # breeding males this year
        n_available_M <- round(
          sum(mature_survived_M, na.rm = TRUE) / M_remigration_int)
        
        # check that there are at least 1 available male and female for breeding
        if (n_available_F < 1 | n_available_M < 1) {
          
          female_hatchlings <- 0
          male_hatchlings <- 0  
          hatchlings <- 0
          PSR <- NA
          
        } else {
          
          # operational sex ratio - proportion of males
          OSR <- n_available_M / (n_available_M + n_available_F)
          
          # calculate reproductive success
          breeding_success <- pbeta(2 * OSR, shape1 = 1, shape2 = beta) 
          
          # how many females actually find a male to mate with and then nest
          n_breeding_F <- round(n_available_F * breeding_success)
          
          # check that there is at least one female that will successfully breed
          if (n_breeding_F < 1) {
            
            female_hatchlings <- 0
            male_hatchlings <- 0  
            hatchlings <- 0
            PSR <- NA
            
          } else {
            
            # number of clutches total
            clutches <- round(n_breeding_F * clutches_mu)
            
            # vector of clutch temperatures, one number for each clutch
            clutch_temps <- rnorm(n = clutches, 
                                  mean = temp_mu, 
                                  sd = clutch_temp_sd)
            
            # calculate expected emergence success
            emergence_success <- emergence_success_A / 
              (1 + exp(-emergence_success_k * (
                clutch_temps - emergence_success_t0)))
            
            # vector of probabilities of developing as male, one for each clutch
            probs_male <- 1 / (1 + exp(-k_piv * (clutch_temps - (T_piv))))          
            
            # eggs vector, one number for each clutch
            eggs <- round(rep(eggs_mu, clutches))
            
            # hatchlings vector, one for each clutch
            hatchlings <- round(eggs * emergence_success)
            
            # number of males
            male_hatchlings <- sum(round(hatchlings * probs_male), na.rm = TRUE)
            
            # number of females
            female_hatchlings <- sum(hatchlings, na.rm = TRUE) - male_hatchlings
            
          }
          
        }
        
        # add hatchlings to N
        N[1, 1, y] <- female_hatchlings
        N[2, 1, y] <- male_hatchlings
        
        # add population abundances
        sub_SAD$Abundance <- c(N[1, , y], N[2, , y], N[3, , y], N[4, , y])
        
        # total population size 
        total <- sum(N[, , y], na.rm = TRUE)      
        PSR <- male_hatchlings / sum(hatchlings, na.rm = TRUE)
        
        # add proportions to SAD
        sub_SAD$Proportion <- c(N[1, , y]/total, 
                                N[2, , y]/total, 
                                N[3, , y]/total, 
                                N[4, , y]/total)
        sub_SAD$PSR <- rep(PSR, times = S * max_age) 
        sub_SAD$OSR <- rep(OSR, times = S * max_age) 
        
        
        # add subset to big df
        SAD <- rbind(SAD, sub_SAD)
        
        # print(y)
        # 10 percent updates
        # # write to progress text file, every 10% done
        # if ((y/burn_in*100) %% 10 == 0) {
        #   time2 <- format(lubridate::now())
        #   update2 <- paste(time2, ' - ', model, TS1, 'beta ', beta, ' - ', 
        #                    burn_in, ' burn-in years - ', y/burn_in*100, '% done!', 
        #                    sep = '')
        #   write(update2, file = '../output/SAD_progress.txt', append = TRUE)
        #   
        # }      
        
        # break out of loop if there are zero males at any age
        if (sum(N[2, , y], na.rm = TRUE) < 1 & 
            sum(N[4, , y], na.rm = TRUE) < 1) { break }
        
      }
      
    }
    
    # write to progress text file, every 10% done
    if ((i/nsims*100) %% 1 == 0) {
      time2 <- format(lubridate::now())
      update2 <- paste(time2, ' - ', model, TS1, 'beta ', beta, ' - ', 
                       burn_in, ' burn-in years - ', nsims, ' sims - ', 
                       i/nsims*100, '% sims done!', sep = '')
      write(update2, file = '../output/SAD_progress.txt', append = TRUE)
      
    } 
    
  }
  
  # add rolling 10 and 100 year median proportion values
  SADdf <- SAD %>%
    group_by(Model, Beta, Sex, Age) %>%
    mutate(Prop_10yr_median = slide_dbl(Proportion, 
                                        median, 
                                        .before = 9, 
                                        .complete = TRUE))%>%
    mutate(Prop_100yr_median = slide_dbl(Proportion, 
                                         median, 
                                         .before = 99, 
                                         .complete = TRUE))
  
  # save object
  save(SADdf, 
       file = paste('../output/SAD_deterministic_', TS2, 'b', 
                    burn_in, '_', model, '_beta', beta, '_n', nsims, 
                    '.Rdata', sep = ''))
  
  # update progress text file with total time it took to run the thing
  end <- lubridate::now()
  total_time <- format(round(end - start, 3))
  update3 <- paste(model, TS1, 'beta ', beta, ' - ', burn_in, 
                   ' burn-in years - ', nsims, ' sims - total time: ', 
                   total_time, '\n', sep = '')
  write(update3, file = '../output/SAD_progress.txt', append = TRUE)
  
}
