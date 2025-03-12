# get vlines for plotting 10 yr median lambdas over time + IQR

# empty environment
rm(list = ls())

# load libraries
library(ggplot2)

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

# load in lambdas object
load("~/Projects/iliketurtles3/output/lambdas.Rdata")

##### plotting parameters ######################################################

# populations
pops <- c('West Africa', 'Suriname')

# abundances
abundances <- c('total abundance', 'mature abundance')
filenames <- c('10000_abundance_total.Rda', '10000_abundance_mature.Rda')

# scenarios
# scenarios <- c('1C', '4C')
scenarios <- c('0.5C', '3.5C')

# osrs
# osrs <- c(0.5, 0.05)
osrs <- c(0.1, 0.45)

betas <- OSRs_to_betas(osrs)
beta_names <- paste('beta', betas, sep = '')

# models
models <- c('P_base', 'GM_base')

##### create objects ###########################################################

# intialize super data frame (SDF)
SDF <- data.frame(Population = NULL, 
                  Scenario = NULL, 
                  OSR = NULL, 
                  Year = NULL,
                  Lambda_10yr_median = NULL, 
                  Lambda_10yr_Q25 = NULL, 
                  Lambda_10yr_Q75 = NULL)

# for each population
for (p in 1:length(pops)) {
  
  # for each scenario
  for (s in 1:length(scenarios)) {
    
    # for each osr
    for (b in 1:length(betas)) {
      
      # for each abundance
      for (a in 1:length(abundances)) {
        
        # load in appropriate object
        load(paste('~/Projects/iliketurtles3/output/lambda plots', 
                   pops[p], scenarios[s], beta_names[b], filenames[a], 
                   sep = '/'))
        
        # for each year for each sim, is the total abundance > 10% starting?
        alive <- sims_abundance_total > 0.1*sims_abundance_total[1, ]
        
        # for each year, what proportion of sims has total abundance > 10%
        # starting abundance
        prop_alive <- rowMeans(alive)
        
        # what's the year where that proportion first goes under 1%?
        vline <- max(which(prop_alive > 0.01))
        
        # subset lambdas and truncate years after vline
        subset <- lambdas %>%
          filter(Population == pops[p]) %>%
          filter(Model == models[p]) %>%
          filter(Scenario == scenarios[s]) %>%
          filter(OSR == osrs[b]) %>%
          filter(Abundance == abundances[a]) %>%
          filter(Year <= vline) %>%
          select(Population, Scenario, OSR, Abundance, Year, 
                 Lambda_10yr_median, Lambda_10yr_Q25, Lambda_10yr_Q75) %>%
          mutate(vline = vline)
        
        # tack subset onto SDF
        SDF <- rbind(SDF, subset)
        
        # update tracker
        print(paste(pops[p], scenarios[s], beta_names[b], filenames[a], 
                    ' all done!', sep = ' - '))
        print(paste('length SDF = ', nrow(SDF), sep = ''))
        
      }
      
    }
    
  }
  
}

median_lambdas_to_plot_over_time <- SDF

save(median_lambdas_to_plot_over_time, 
     file = '~/Projects/iliketurtles3/output/median_lambdas_to_plot_over_time.Rdata')

# load object
load("~/Projects/iliketurtles3/output/median_lambdas_to_plot_over_time.Rdata")

# make scenario a factor
median_lambdas_to_plot_over_time$Scenario <- 
  factor(median_lambdas_to_plot_over_time$Scenario)
median_lambdas_to_plot_over_time$OSR <- 
  factor(median_lambdas_to_plot_over_time$OSR)

# plot figure - median
fig5b <- ggplot(data = median_lambdas_to_plot_over_time, 
                aes(x = Year, 
                    y = Lambda_10yr_median, 
                    color = OSR, 
                    linetype = Scenario)) + 
  facet_grid(cols = vars(Population), rows = vars(Abundance)) +
  geom_hline(yintercept = 1) +
  geom_ribbon(aes(ymin = Lambda_10yr_Q25,
                  ymax = Lambda_10yr_Q75, 
                  col = NULL, 
                  fill = factor(OSR)),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#F8766D', '#00BFC4')) + 
  xlab('Year') +
  ylab('Median Lambda') +
  ggtitle('temperature stochasticity; (10yr) median lambdas over time + IQR') +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13)) +
  theme(legend.key.width = unit(2.65, "line"))

# save to file
ggsave(plot = fig5b, 
       filename = paste('10yr_median_lambdas.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 9, height = 8)


