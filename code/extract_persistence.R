# make figures representing output

# make this script just for extracting all the probabilities of population
# persistence, then use separate scripts to make the figures

# # set working directory - desktop
setwd('~/Projects/iliketurtles3')

# load libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')
# source('/home/quennessenv/iliketurtles3/code/mating function/OSRs_to_betas.R')

################################################################################
# which computer am I using?
desktop <- TRUE

# folder names for building paths
folders <- c('2025_06_26_new_N_runs')

# model names for building paths
models <- c('P_base', 'GM_base')
model_names <- c('base model', 'base model')

# which year(s) to track
years_to_plot <- c(25, 50, 75, 100)

################################################################################

# plotting model parameters
nsims <- 10000

# populations simulated
pops <- c('West Africa', 'Suriname')

# temperature increase scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')
# scenarios <- paste(c(1), 'C', sep = '')

# operational sex ratios to get 100% reproductive success
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.49)
# osrs <- c(0.25)
betas <- OSRs_to_betas(osrs)

# generate automatically
paths <- as.vector(outer(folders, models, paste, sep = "/"))
populations <- rep(pops, each = length(folders))
models_short <- rep(models, each = length(folders))
model_types <- rep(model_names, each = length(folders), times = length(pops))

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
SDF <- data.frame(
  Folder = rep(NA, P*S*B),
  Population = rep(NA, P*S*B), 
  Model = rep(NA, P*S*B),
  model = rep(NA, P*S*B),
  Scenario = rep(NA, P*S*B), 
  OSR = rep(NA, P*S*B), 
  Survive_to = rep(NA, P*S*B), 
  Probability_total_mean = rep(NA, P*S*B),
  Probability_total_F_mean = rep(NA, P*S*B), 
  Probability_total_M_mean = rep(NA, P*S*B), 
  Probability_mature_mean = rep(NA, P*S*B),
  Probability_mature_F_mean = rep(NA, P*S*B),
  Probability_mature_M_mean = rep(NA, P*S*B)
)

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
        if (
          
          file.exists(paste('E:/PhD Thesis/',
                            # file.exists(paste('~/Projects/iliketurtles3/output/',
                            # file.exists(paste('E:/',
                            # file.exists(paste('/home/quennessenv/iliketurtles3/output/',
                            paths[p], '/', scenarios[s], '/beta', betas[b],
                            '/', nsims, '_N.Rda', sep = ''))
          
        ) {
          
          # load in N object
          load(paste('C:/Users/', user, '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                     # load(paste('~/Projects/iliketurtles3/output/',
                     # load(paste('E:/',
                     # load(paste('/home/quennessenv/iliketurtles3/output/',
                     paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                     nsims, '_N.Rda', sep = ''))
        
        # index number
        index <- (p - 1)*S*B + (s - 1)*B + b
        
        
        
        # print update
        print_index <- index*(y - 1) + index
        print(paste(print_index / (P*S*B*Y) * 100, '% done', sep = ''))
        
      } else { 
        
        print('this particular combination of values does not have a SAD')
        
        SDF$Folder[index] <- folders[p]
        SDF$Population[index] <- populations[p]
        SDF$Model[index] <- models_short[p]
        SDF$model[index] <- model_types[p]
        SDF$Scenario[index] <- scenarios[s]
        SDF$OSR[index] <- osrs[b]
        SDF$Survive_to[index] <- year_to_plot
        
        SDF$Probability_total_mean[index] <- NA
        SDF$Probability_total_F_mean[index] <- NA
        SDF$Probability_total_M_mean[index] <- NA
        SDF$Probability_mature_mean[index] <- NA
        SDF$Probability_mature_F_mean[index] <- NA
        SDF$Probability_mature_M_mean[index] <- NA
      
    }
    
  }
  
  # make scenario and osr a factor variable
  SDF$Scenario <- factor(SDF$Scenario, levels = scenarios)  
  
  # save dataframe as R object - base model
  base_persistence <- SDF
  save(base_persistence,
       file = '~/Projects/iliketurtles3/output/base_persistence.Rdata')
  
  # # save dataframe as R object - evolution
  # evolution_persistence <- SDF
  # save(evolution_persistence,
  #      file = '~/Projects/iliketurtles3/output/evolution_persistence.Rdata')
  # 
  # # save dataframe as R object - conservation
  # conservation_persistence <- SDF
  # save(conservation_persistence,
  #      file = '~/Projects/iliketurtles3/output/conservation_persistence.Rdata')

  
}
