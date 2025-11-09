# max number of individuals at any sex, age, year across models, beta, temp

# load libraries

# parameters
folder <- '2025_10_23_SAD_deterministic_TS_b800_10y'
models <- c('P_base', 'GM_base')
betas <- c(1.17, 2, 2.86, 3.82, 5.02, 6.64, 9.01, 12.91, 20.63, 43.7)
scenarios <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)

# initialize empty dataframe
maximums <- data.frame(Model = NULL, 
                       Beta = NULL, 
                       Scenario = NULL, 
                       Max = NULL)

# for each model
for (m in 1:length(models)) {
  
  model <- models[m]
  
  for (b in 1:length(betas)) {
    
    beta <- paste('beta', betas[b], sep = '')
    
    for (s in 1:length(scenarios)) {
      
      scenario <- paste(scenarios[s], 'C', sep = '')
      
      # load object
      load(paste('C://Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/iliketurtles3', 
                 folder, model, scenario, beta, '10000_N.Rda', sep = '/'))
      
      # extract max value
      max_value <- max(sims_N, na.rm = TRUE)
      
      # make df
      maximums <- rbind(maximums, 
                        data.frame(Model = model, 
                                   Beta = betas[b], 
                                   Scenario = scenarios[s], 
                                   Max = max_value)
      )
      
    }

  }
  
}

absolute_max <- max(maximums$Max, na.rm = TRUE)
# 112003

2*absolute_max
# 224006