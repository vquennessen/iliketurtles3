# make figures representing output

# make this script just for extracting all the probabilities of population
# persistence, then use separate scripts to make the figures

# # set working directory - desktop
# setwd('~/Projects/iliketurtles3')

# source functions
# source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')
source('/home/quennessenv/iliketurtles3/code/mating function/OSRs_to_betas.R')

################################################################################
# which computer am I using? - desktop / laptop / cluster
computer <- 'cluster'

# folder names for building paths
folder <- c('2025_09_19_red_noise')

# model names for building paths
models <- c('P_base', 'GM_base')
model_names <- c('base model', 'base model')

# populations simulated
populations <- c('West Africa', 'Suriname')

# which year(s) to track
years_to_plot <- c(1:100)

# plotting model parameters
nsims <- 10000

################################################################################

# temperature increase scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')
# scenarios <- paste(c(1), 'C', sep = '')

# operational sex ratios to get 100% reproductive success
osrs <- c(0.49, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)
# osrs <- c(0.25)
betas <- OSRs_to_betas(osrs)

# generate automatically
folders <- rep(folder, length(models))
paths <- paste(folders, models, sep = "/")
abundances <- c('Immature Females', 'Immature Males', 'Mature Females', 
                'Mature Males', 'Total', 'Mature')

# dimensions
P <- length(paths)
S <- length(scenarios)
B <- length(osrs)
Y <- length(years_to_plot)
A <- length(abundances)
nrows <- Y*P*S*B*A

# maturity ogive
max_age <- 85
age_maturity_mu <- 25
age_maturity_sd <- 2.5
M <- pnorm(q = 1:max_age, mean = age_maturity_mu, sd = age_maturity_sd)

# initialize super data frame
SDF <- data.frame(
  Folder = rep(NA, nrows),
  Population = rep(NA, nrows), 
  Model = rep(NA, nrows),
  model = rep(NA, nrows),
  Scenario = rep(NA, nrows), 
  OSR = rep(NA, nrows), 
  Survive_to = rep(NA, nrows), 
  Abundance = rep(NA, nrows),
  Probability_mean = rep(NA, nrows)
)



# for each model
for (p in 1:P) {
  
  # for each scenario
  for (s in 1:S) {
    
    # for each mating function
    for (b in 1:B) {
      
      # load in appropriate output file
      
      user <- ifelse(computer == 'desktop', 'Vic', 'vique')
      
      # if the file exists - desktop / laptop
      if (
        
        # file.exists(paste('C:/Users/', user,
        #                   '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/i like turtles/',
                          # file.exists(paste('E:/PhD Thesis/model_output',
                          # file.exists(paste('~/Projects/iliketurtles3/output/',
                          file.exists(paste('/home/quennessenv/iliketurtles3/output/',
                          paths[p], '/', scenarios[s], '/beta', betas[b],
                          '/', nsims, '_N.Rda', sep = ''))
        
      ) {
        
        # load in N object
        # load(paste('C:/Users/', user,
        #            '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/i like turtles/',
                   # load(paste('~/Projects/iliketurtles3/output/',
                   # load(paste('E:/',
                   load(paste('/home/quennessenv/iliketurtles3/output/',
                   paths[p], '/', scenarios[s], '/beta', betas[b], '/',
                   nsims, '_N.Rda', sep = ''))
        
        # for each year to plot
        for (y in 1:Y) {
          
          year_to_plot <- years_to_plot[y] 
          
          # index number
          index <- (p - 1)*S*B*Y*A + (s - 1)*B*Y*A + (b - 1)*Y*A + (y - 1)*A + 1
          # print(index)
          
          SDF$Folder[index:(index + A - 1)] <- folders[p]
          SDF$Population[index:(index + A - 1)] <- populations[p]
          SDF$Model[index:(index + A - 1)] <- models[p]
          SDF$model[index:(index + A - 1)] <- model_names[p]
          SDF$Scenario[index:(index + A - 1)] <- scenarios[s]
          SDF$OSR[index:(index + A - 1)] <- osrs[b]
          SDF$Survive_to[index:(index + A - 1)] <- year_to_plot
          SDF$Abundance[index:(index + A - 1)] <- abundances
          
          if (is.null(sims_N)) {
            
            SDF$Probability_mean[index:(index + A - 1)] <- NA
            
          } else {
            
            # immature females
            IF_init <- colSums(sims_N[1, , 1, ])
            IF_final <- colSums(sims_N[1, , year_to_plot, ])
            
            # immature males
            IM_init <- colSums(sims_N[2, , 1, ])
            IM_final <- colSums(sims_N[2, , year_to_plot, ])
            
            # mature females
            MF_init <- colSums(sims_N[3, , 1, ])
            MF_final <- colSums(sims_N[3, , year_to_plot, ])
            
            # mature males
            MM_init <- colSums(sims_N[4, , 1, ])
            MM_final <- colSums(sims_N[4, , year_to_plot, ])
            
            # total population size
            total_init <- IF_init + IM_init + MF_init + MM_init
            total_final <- IF_final + IM_final + MF_final + MM_final
            
            # mature population size
            mature_init <- MF_init + MM_init
            mature_final <- MF_final + MM_final
            
            # probability of population persistence past 10% of initial
            SDF$Probability_mean[index:(index + A - 1)] <- c(
              mean((IF_final >= (0.10 * IF_init)), na.rm = TRUE),
              mean((IM_final >= (0.10 * IM_init)), na.rm = TRUE),
              mean((MF_final >= (0.10 * MF_init)), na.rm = TRUE),
              mean((MM_final >= (0.10 * MM_init)), na.rm = TRUE),
              mean((total_final >= (0.10 * total_init)), na.rm = TRUE),
              mean((mature_final >= (0.10 * mature_init)), na.rm = TRUE))
          }
          
        }
                  
        # print progress update
          prop <- round((index + A - 1) / (nrows) * 100, 2)
          print(paste(Sys.time(), ' - ', models[p], ' - ', scenarios[s], 
                      ' - beta ', betas[b], ' - persistence done - ', prop, 
                      '% of total done!', sep = ''))
          
      }
      
    }
    
  }
  
}

# make scenario and osr a factor variable
SDF$Scenario <- factor(SDF$Scenario, levels = scenarios)  

# save dataframe as R object - base model
base_persistence <- SDF
save(base_persistence,
     # file = '~/Projects/iliketurtles3/output/base_persistence.Rdata')
file = '/home/quennessenv/iliketurtles3/output/red_noise_base_persistence.Rdata')

# # save dataframe as R object - evolution
# evolution_persistence <- SDF
# save(evolution_persistence,
#      file = '~/Projects/iliketurtles3/output/evolution_persistence.Rdata')
# 
# # save dataframe as R object - conservation
# conservation_persistence <- SDF
# save(conservation_persistence,
#      file = '~/Projects/iliketurtles3/output/conservation_persistence.Rdata')
