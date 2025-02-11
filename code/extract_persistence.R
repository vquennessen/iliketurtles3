# make figures representing output

# TODO 
# make this script just for extracting all the probabilities of population
# persistence, then use separate scripts to make the figures

# set working directory
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)

# source functions
source('code/mating function/OSRs_to_betas.R')

# which computer am I using?
desktop <- TRUE

# plotting model parameters
nsims <- 10000

# combos - no vs. temp stochasticity
stochasticity <- c('no_temp_stochasticity', 'temp_stochasticity')
stochasticity_names <- c('temperature stochasticity')
models <- c('P_evol_piv', 'P_evol_piv_high_H', 
            'GM_evol_piv', 'GM_evol_piv_high_H')
pops <- c('West Africa', 'Suriname')
model_names <- rep(c('evolution', 'evolution with high H'), times = length(pops))
individual_figs_filename <- 'evol_piv_persistence'
combined_fig_filename <- 'evol_piv_persistence_combined'

# # combos
# stochasticity <- c('temp_stochasticity')
# stochasticity_names <- c('temperature stochasticity')
# models <- c('P_evol_piv', 'P_evol_piv_high_H', 
#             'GM_evol_piv', 'GM_evol_piv_high_H')
# pops <- c('West Africa', 'Suriname')
# model_names <- rep(c('evolution', 'evolution with high H'), times = length(pops))
# individual_figs_filename <- 'evol_piv_persistence'
# combined_fig_filename <- 'evol_piv_persistence_combined'

# which year to visualize
year_to_plot <- 100

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
model_types <- rep(model_names, each = length(stochasticity))

# dimensions
P <- length(paths)
S <- length(scenarios)
B <- length(osrs)

# initialize plot list
plot_list <- list()

# initialize super data frame
SDF <- data.frame(Stochasticity = rep(NA, P*S*B), 
                  Population = rep(NA, P*S*B), 
                  Model = rep(NA, P*S*B),
                  model = rep(NA, P*S*B),
                  Scenario = rep(NA, P*S*B), 
                  OSR = rep(NA, P*S*B), 
                  Survive_to = rep(NA, P*S*B), 
                  Probability_total = rep(NA, P*S*B), 
                  Probability_mature = rep(NA, P*S*B))

# for each model
for (p in 1:P) {
  
  # for each scenario
  for (s in 1:S) {
    
    # for each mating function
    for (b in 1:B) {
      
      # load in appropriate output file
      
      if (desktop == TRUE) { user <- 'Vic' } else { user <- 'vique' }
      
      # if the file exists
      if (file.exists(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                            paths[p], '/', scenarios[s], '/beta', betas[b],
                            '/', nsims, '_abundance_total.Rda', sep = '')) &
          
          file.exists(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                            paths[p], '/', scenarios[s], '/beta', betas[b],
                            '/', nsims, '_abundance_mature.Rda', sep = ''))) {
        
        # load in total abundance object
        load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                   paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                   nsims, '_abundance_total.Rda', sep = ''))
        
        # load in abundance mature object
        load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                   paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                   nsims, '_abundance_mature.Rda', sep = ''))
        
      }
      
      # index
      index <- (p - 1)*S*B + (s - 1)*B + b
      print(index)
      
      # # initialize dataframe - temp vs. no temp stochasticity
      # SDF$Stochasticity[index] <- stochasticity_names[(p + 1) %% 2 + 1]
      # SDF$Population[index] <- populations[p]
      # SDF$Model[index] <- models_short[p]
      # SDF$model[index] <- model_types[p]
      # SDF$Scenario[index] <- scenarios[s]
      # SDF$OSR[index] <- osrs[b]
      # SDF$Survive_to[index] <- year_to_plot 
      # SDF$Probability_total[index] <- mean(
      #   sims_abundance_total[year_to_plot, ] > 0.1*sims_abundance_total[1, ])
      # SDF$Probability_mature[index] <- mean(
      #   sims_mature_abundance[year_to_plot, ] > 0.1*sims_mature_abundance[1, ])
      
      # initialize dataframe - evolution
      SDF$Stochasticity[index] <- stochasticity_names[1]
      SDF$Population[index] <- populations[ceiling(p/2)]
      SDF$Model[index] <- models_short[p]
      SDF$model[index] <- model_types[p]
      SDF$Scenario[index] <- scenarios[s]
      SDF$OSR[index] <- osrs[b]
      SDF$Survive_to[index] <- year_to_plot 
      SDF$Probability_total[index] <- mean(
        sims_abundance_total[year_to_plot, ] > 0.1*sims_abundance_total[1, ])
      SDF$Probability_mature[index] <- mean(
        sims_abundance_mature[year_to_plot, ] > 0.1*sims_abundance_mature[1, ])
      
    }
    
  }

}

# save dataframe as R object
base_persistence <- SDF

# make scenario and osr a factor variable
base_persistence$Scenario <- factor(base_persistence$Scenario, 
                                    levels = scenarios)
save(base_persistence, 
     file = paste('~/Projects/iliketurtles3/output/base_persistence.Rdata', 
                  sep = ''))
