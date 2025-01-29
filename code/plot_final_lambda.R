# plot final lambda at year 100

# set working directory
setwd('~/Projects/iliketurtles3')

# source functions
source('code/mating function/OSRs_to_betas.R')

# load libraries
library(ggplot2)
library(ggpattern)

##### to modify ################################################################

# which computer am I using?
desktop <- TRUE

# folder(s)
folders <- c('no_temp_stochasticity', 'temp_stochasticity')

# model(s)
models <- c('P_base', 'GM_base')

# filepaths
paths <- c(paste(folders[1], '/', models, sep = ''), 
           paste(folders[2], '/', models, sep = ''))

# years to average over
years <- 2:100
average_over <- 10
years_avg <- (average_over + 1):100

################################################################################

# plotting model parameters
nsims <- 10000

# column names for combined heatmap
populations <- c(rep('West Africa', length(folders)),
                 rep('Suriname', length(folders)))

# row names for combined heatmap
stochasticity <- rep(c('no temperature stochaticity', 
                       'temperature stochasticity'),
                     times = length(models))

# temperature increase scenarios
scenarios <- paste(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 'C', sep = '')

# operational sex ratios / betas
osrs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
betas <- OSRs_to_betas(osrs)

# abundances to plot
abundances <- c('abundance_total', 'mature_abundance')
abundance_names <- c('total abundance', 'mature abundance')

# dimensions
P <- length(paths)
M <- length(models)
S <- length(scenarios)
OSR <- length(osrs)
Y <- length(years)
Ya <- length(years_avg)
A <- length(abundances)

# clear DF and SDF objects
rm(DF)
rm(SDF)

# initialize plot list
plot_list <- list()

# initialize super data frame
SDF <- data.frame(Stochasticity = NULL, 
                  Population = NULL, 
                  Model = NULL,
                  Scenario = NULL, 
                  OSR = NULL, 
                  Year = NULL,
                  Abundance = NULL,
                  Lambda = NULL,
                  Lambda_avg = NULL)

for (p in 1:P) {
  
  for (s in 1:S) {
    
    for (osr in 1:OSR) {
      
      # initialize empty dataframe, one for each filepath
      DF <- data.frame(Stochasticity = stochasticity[p], 
                       Population = populations[p], 
                       Model = models[ceiling(p / 2)],
                       Scenario = scenarios[s], 
                       OSR = osrs[osr], 
                       Year = years, 
                       Abundance = rep(abundance_names, each = Y), 
                       Lambda = NA, 
                       Lambda_avg = NA,
                       Lambda_Q25 = NA, 
                       Lambda_Q75 = NA)
      
      # load in appropriate output file
      
      if (desktop == TRUE) { user <- 'Vic' } else { user <- vique }
      
      # if the file exists
      if (file.exists(paste('C:/Users/', user, 
                            '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                            paths[p], '/', scenarios[s], '/beta', 
                            betas[osr], '/', nsims, '_', abundances[1], '.Rda', 
                            sep = '')) 
          
          &
          
          file.exists(paste('C:/Users/', user, 
                            '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                            paths[p], '/', scenarios[s], '/beta', 
                            betas[osr], '/', nsims, '_', abundances[2], '.Rda', 
                            sep = '')))
      {
        
        # load in total abundance object
        load(paste('C:/Users/', user, 
                   '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                   paths[p], '/', scenarios[s], '/beta', betas[osr], '/', nsims, 
                   '_', abundances[1], '.Rda', sep = ''))
        
        # load in mature abundance object
        load(paste('C:/Users/', user, 
                   '/Box Sync/Quennessen_Thesis/PhD Thesis/model output/',
                   paths[p], '/', scenarios[s], '/beta', betas[osr], '/', nsims, 
                   '_', abundances[2], '.Rda', sep = ''))
        
      }
      
      # calculate lambdas for abundance total
      
      # average abundances total and mature
      avg_abundance_total <- rowMeans(sims_abundance_total)
      avg_abundance_mature <- rowMeans(sims_mature_abundance)
      
      # lambdas per year
      lambdas_total <- sims_abundance_total[2:100, ] / sims_abundance_total[1:99, ]
      lambdas_mature <- sims_mature_abundance[2:100, ] / sims_mature_abundance[1:99, ]
      
      # add lambdas to DF
      DF$Lambda[1:Y] <- rowMeans(lambdas_total)
      DF$Lambda[(Y + 1):(2 * Y)] <- rowMeans(lambdas_mature)
      
      # initialize average, Q5, and Q95 lambdas
      avg_lambdas_total <- rep(NA, Y)
      avg_lambdas_mature <- rep(NA, Y)
      
      Q25_lambdas_total <- rep(NA, Y)
      Q25_lambdas_mature <- rep(NA, Y)
      
      Q75_lambdas_total <- rep(NA, Y)
      Q75_lambdas_mature <- rep(NA, Y)
      
      # average lambdas per year over over_average years
      for (y in average_over:Y) {
        
        avg_lambdas_total[y] <- mean(lambdas_total[(y - average_over):y, ], 
                                     na.omit = TRUE)
        avg_lambdas_mature[y] <- mean(lambdas_mature[(y - average_over):y, ], 
                                      na.omit = TRUE)
        
        Q25_lambdas_total[y] <- quantile(lambdas_total[(y - average_over):y, ],
                                         na.rm = TRUE,
                                         probs = c(0.25))
        Q25_lambdas_mature[y] <- quantile(lambdas_mature[(y - average_over):y, ],
                                         na.rm = TRUE,
                                         probs = c(0.25))
        
        Q75_lambdas_total[y] <- quantile(lambdas_total[(y - average_over):y, ],
                                         na.rm = TRUE,
                                         probs = c(0.75))
        Q75_lambdas_mature[y] <- quantile(lambdas_mature[(y - average_over):y, ],
                                          na.rm = TRUE,
                                          probs = c(0.75))

        
      }
      
      # add average lambdas to DF
      DF$Lambda_avg[1:Y] <- avg_lambdas_total
      DF$Lambda_avg[(Y + 1):(2 * Y)] <- avg_lambdas_mature
      
      # add DF to SDF
      SDF <- rbind(SDF, DF)
      
      # print progress update
      print(paste(Sys.time(), ' - ', stochasticity[p], ' - ', 
                  models[ceiling(p / 2)], ' - ', scenarios[s], ' - beta ', 
                  betas[osr], ' all done!', sep = ''))
      
    }
    
  }
  
}

# save dataframe as R object
save(SDF, file = paste('~/Projects/iliketurtles3/output/lambdas.Rdata', 
                       sep = ''))

##### plot final lambdas #######################################################

years_to_plot <- 100
SDF_subset <- subset(SDF, Year == years_to_plot & 
                       Stochasticity == 'temperature stochasticity')
SDF_subset$bin <- cut(SDF_subset$Lambda,
                      breaks = c(0.5, 0.9, 0.99, 1, 1.01, 1.02, 1.03),
                      right = FALSE)

fig5a <- ggplot(data = SDF_subset, aes(x = OSR, 
                                       y = Scenario, 
                                       fill = bin)) +
  geom_tile(color = "white",
            lwd = 1.25,
            linetype = 1) +
  scale_fill_brewer(palette = "RdBu", na.value = 'gray') +
  guides(fill = guide_legend(title = "Lambda")) +
  xlab('Operational sex ratio required to fertilize all females') +
  ylab('Increase in sand temperature (\u00B0C) by year 100') +
  ggtitle('temperature stochasticity; final lambda (year 100)') +
  facet_grid(rows = vars(Abundance), 
             cols = vars(Population)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 10)) +
  theme(title = element_text(size = 13))

# save to file
ggsave(plot = fig5a, 
       filename = paste('TS_final_lambda.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)

##### plot lambdas over time ###################################################

# subset to only look at some scenarios and OSRs
SDF_subset2 <- subset(SDF,
                      Stochasticity == 'temperature stochasticity' &
                        Scenario %in% c('0.5C', '5C') &
                        OSR %in% c(0.05, 0.5))
SDF_subset2$OSR <- as.factor(SDF_subset2$OSR)

fig5b <- ggplot(data = SDF_subset2, aes(x = Year, 
                                        y = Lambda_avg, 
                                        color = OSR, 
                                        linetype = Scenario)) +
  geom_hline(yintercept = 1, lty = 1) +
  geom_path() +
  xlab('Year') +
  ylab('Lambda') +
  ggtitle('temperature stochasticity; lambdas over time') +
  facet_grid(rows = vars(Abundance), 
             cols = vars(Population)) +
  # theme_bw() +
  # theme(panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13)) +
  theme(legend.key.width = unit(2.65, "line"))

# save to file
ggsave(plot = fig5b, 
       filename = paste('TS_avg_lambdas.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 8, height = 17/3)
