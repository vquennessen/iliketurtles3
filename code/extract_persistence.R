# make figures representing output

# make this script just for extracting all the probabilities of population
# persistence, then use separate scripts to make the figures

# # set working directory - desktop
# setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

# which computer am I using?
desktop <- TRUE

# plotting model parameters
nsims <- 10000

# combos - no vs. temp stochasticity
stochasticity <- c('no_temp_stochasticity', 'temp_stochasticity')
stochasticity_names <- c('no temperature stochasticity', 'temperature stochasticity')
stochasticity_names_short <- c('nTS', 'TS')
models <- c('P_base', 'GM_base')
pops <- c('West Africa', 'Suriname')
model_names <- c('base model', 'base model')
individual_figs_filename <- 'base_persistence'
combined_fig_filename <- 'base_persistence_combined'

# # combos - evolution
# stochasticity <- c('temp_stochasticity')
# stochasticity_names <- c('temperature stochasticity')
# stochasticity_names_short <- c('TS')
# models <- c('P_evol_piv', 'P_evol_piv_high_H',
#             'GM_evol_piv', 'GM_evol_piv_high_H', 
#             'P_evol_threshold', 'P_evol_threshold_high_H',
#             'GM_evol_threshold', 'GM_evol_threshold_high_H')
# pops <- c('West Africa', 'Suriname', 'West Africa', 'Suriname')
# model_names <- c('evolution', 'evolution with high H')
# individual_figs_filename <- c('piv_evol_persistence', 
#                               'threshold_evol_persistence')
# combined_fig_filename <- c('piv_evol_persistence_combined', 
#                            'threshold_evol_persistence_combined')

# which year to track
years_to_plot <- c(100)

# temperature increase scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')
# scenarios <- paste(c(0.5, 5), 'C', sep = '')

# operational sex ratios to get 100% reproductive success
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
betas <- OSRs_to_betas(osrs)

# generate automatically
paths <- as.vector(outer(stochasticity, models, paste, sep="/"))
populations <- rep(pops, each = length(stochasticity))
models_short <- rep(models, each = length(stochasticity))
model_types <- rep(model_names, each = length(stochasticity), times = length(pops))

# dimensions
P <- length(paths)
S <- length(scenarios)
B <- length(osrs)
Y <- length(years_to_plot)

# maturity ogive
max_age <- 85
age_maturity_mu <- 25
age_maturity_sd <- 2.5
M <- pnorm(q = 1:max_age, mean = age_maturity_mu, sd = age_maturity_sd)


# initialize super data frame
SDF <- data.frame(Stochasticity = rep(NA, P*S*B),
                  Stochasticity_short = rep(NA, P*S*B), 
                  Population = rep(NA, P*S*B), 
                  Model = rep(NA, P*S*B),
                  model = rep(NA, P*S*B),
                  Scenario = rep(NA, P*S*B), 
                  OSR = rep(NA, P*S*B), 
                  Survive_to = rep(NA, P*S*B), 
                  Probability_total = rep(NA, P*S*B),
                  Probability_total_F = rep(NA, P*S*B), 
                  Probability_total_M = rep(NA, P*S*B), 
                  Probability_mature = rep(NA, P*S*B),
                  Probability_mature_F = rep(NA, P*S*B),
                  Probability_mature_M = rep(NA, P*S*B))

# for each year to plot
for (y in 1:Y) {
  
  year_to_plot <- years_to_plot[y]
  
  # for each model
  for (p in 1:P) {
    
    # for each scenario
    for (s in 1:S) {
      
      # for each mating function
      for (b in 1:B) {
        
        # load in appropriate output file
        
        if (desktop == TRUE) { user <- 'Vic' } else { user <- 'vique' }
        
        # if the file exists - desktop / laptop
        # if (file.exists(paste('E:/phd model output/',
        if (file.exists(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                              paths[p], '/', scenarios[s], '/beta', betas[b],
                              '/', nsims, '_abundance_total.Rda', sep = '')) &
            
            file.exists(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                              
                              paths[p], '/', scenarios[s], '/beta', betas[b],
                              '/', nsims, '_abundance_mature.Rda', sep = '')) &
            
            file.exists(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                              
                              paths[p], '/', scenarios[s], '/beta', betas[b],
                              '/', nsims, '_abundance_F.Rda', sep = '')) &
            
            file.exists(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                              
                              paths[p], '/', scenarios[s], '/beta', betas[b],
                              '/', nsims, '_abundance_M.Rda', sep = '')) &
            
            file.exists(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                              
                              paths[p], '/', scenarios[s], '/beta', betas[b],
                              '/', nsims, '_N.Rda', sep = ''))
            
            ) {
          
          # load in total abundance object
          load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                     
                     paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                     nsims, '_abundance_total.Rda', sep = ''))
          
          # load in abundance mature object
          load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                     
                     paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                     nsims, '_abundance_mature.Rda', sep = ''))
          
          # load in abundance F object
          load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                     
                     paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                     nsims, '_abundance_F.Rda', sep = ''))
          
          # load in abundance M object
          load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                     
                     paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                     nsims, '_abundance_M.Rda', sep = ''))
          
          # load in N object
          load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',                     
                     paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                     nsims, '_N.Rda', sep = ''))
          
        }
        
        # # if the file exists - cluster - evolution
        # if (file.exists(paste('/home/quennessenv/iliketurtles3/output/', paths[p], 
        #                       '/', scenarios[s], '/beta', betas[b], '/', nsims, 
        #                       '_abundance_total.Rda', sep = '')) &
        #     
        #     file.exists(paste('/home/quennessenv/iliketurtles3/output/', paths[p], 
        #                       '/', scenarios[s], '/beta', betas[b], '/', nsims, 
        #                       '_abundance_mature.Rda', sep = ''))) {
        #   
        #   # load in total abundance object
        #   load(paste('/home/quennessenv/iliketurtles3/output/', paths[p], '/', 
        #              scenarios[s], '/beta', betas[b], '/', nsims, 
        #              '_abundance_total.Rda', sep = ''))
        #   
        #   # load in abundance mature object
        #   load(paste('/home/quennessenv/iliketurtles3/output/', paths[p], '/', 
        #              scenarios[s], '/beta', betas[b], '/', nsims, 
        #              '_abundance_mature.Rda', sep = ''))
        #   
        # }
        
        index <- (p - 1)*S*B + (s - 1)*B + b
        print(index)
        
        # initialize dataframe - temp vs. no temp stochasticity
        SDF$Stochasticity[index] <- stochasticity_names[(p + 1) %% 2 + 1]
        SDF$Stochasticity_short[index] <- stochasticity_names_short[(p + 1) %% 2 + 1]
        SDF$Population[index] <- populations[p]
        SDF$Model[index] <- models_short[p]
        SDF$model[index] <- model_types[p]
        SDF$Scenario[index] <- scenarios[s]
        SDF$OSR[index] <- osrs[b]
        SDF$Survive_to[index] <- year_to_plot
        
        SDF$Probability_total_mean[index] <- mean(
          sims_abundance_total[year_to_plot, ] > 0.1 * sims_abundance_total[1, ])
        SDF$Probability_total_F_mean[index] <- mean(
          sims_abundance_F[year_to_plot, ] > 0.1 * sims_abundance_F[1, ])
        SDF$Probability_total_M_mean[index] <- mean(
          sims_abundance_M[year_to_plot, ] > 0.1 * sims_abundance_M[1, ])
        SDF$Probability_mature[index] <- mean(
          sims_abundance_mature[year_to_plot, ] > 0.1 * sims_abundance_mature[1, ])
        SDF$Probability_mature_F[index] <- mean(
          sims_N[1, , year_to_plot, ] * M > 0.1 * sims_N[1, , 1, ] * M)
        SDF$Probability_mature_M[index] <- mean(
          sims_N[2, , year_to_plot, ] * M > 0.1 * sims_N[2, , 1, ] * M)
        
        # # initialize dataframe - evolution
        # SDF$Stochasticity[index] <- stochasticity_names[1]
        # SDF$Stochasticity_short[index] <- stochasticity_names_short[1]
        # SDF$Population[index] <- populations[ceiling(p/2)]
        # SDF$Model[index] <- models_short[p]
        # SDF$model[index] <- model_types[p]
        # SDF$Scenario[index] <- scenarios[s]
        # SDF$OSR[index] <- osrs[b]
        # SDF$Survive_to[index] <- year_to_plot
        # SDF$Probability_total[index] <- mean(
        #   sims_abundance_total[year_to_plot, ] > 0.1*sims_abundance_total[1, ])
        # SDF$Probability_mature[index] <- mean(
        #   sims_abundance_mature[year_to_plot, ] > 0.1*sims_abundance_mature[1, ])
        
      }
      
    }
    
  }
  
  # save dataframe as R object
  base_persistence <- SDF
  
  # make scenario and osr a factor variable
  base_persistence$Scenario <- factor(base_persistence$Scenario,
                                      levels = scenarios)
  save(base_persistence,
       file = paste('~/Projects/iliketurtles3/output/base_persistence_', 
                    year_to_plot, '.Rdata', sep = ''))
  
  # # save dataframe as R object
  # evolution_persistence <- SDF
  # 
  # # make scenario and osr a factor variable
  # evolution_persistence$Scenario <- factor(evolution_persistence$Scenario, 
  #                                          levels = scenarios)
  # save(evolution_persistence, 
  #      file = paste('../output/evolution_persistence_', year_to_plot, '.Rdata', 
  #                   sep = ''))
  
}
