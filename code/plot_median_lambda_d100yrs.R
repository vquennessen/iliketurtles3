# get vlines for plotting 10 yr median lambdas over time + IQR

# empty environment
rm(list = ls())

# load libraries
library(ggplot2)

# source functions
source('~/Projects/iliketurtles3/code/mating function/OSRs_to_betas.R')

# load in lambdas and persistence object
load("~/Projects/iliketurtles3/output/lambdas_and_persistence.Rdata")

##### plotting parameters ######################################################

# abundances to plot
abundances <- c(
  # 'Immature Females', 
  # 'Immature Males', 
  # 'Mature Females', 
  # 'Mature Males', 
  'Total', 
  'Mature'
  )

# scenarios
scenarios <- c('0.5C', '4.5C')


# osrs
osrs <- c(0.1, 0.35)
betas <- OSRs_to_betas(osrs)

# filter out stuff we don't want to plot
median_lambdas_to_plot_over_time <- lambdas_and_persistence %>%
  filter(OSR %in% osrs) %>%
  filter(Scenario %in% scenarios) %>%
  # filter(Abundance %in% abundances) %>%
  filter(Abundance == 'Mature') %>%
  mutate(Lambda_10yr_median = replace(Lambda_10yr_median, Persistence < 0.1, 
                                      NA)) %>%
  mutate(Lambda_10yr_Q25 = replace(Lambda_10yr_Q25, Persistence < 0.1, NA)) %>%
  mutate(Lambda_10yr_Q75 = replace(Lambda_10yr_Q75, Persistence < 0.1, NA)) %>%
  mutate(Mating_Function = if_else(as.numeric(as.character(OSR)) < 0.26, 
                                   'Steep', 'Shallow')) %>%
  mutate(TRT = ifelse(Population == 'West Africa', 'Narrow TRT', 'Wide TRT')) %>%
  mutate(facet_labels = ifelse(Abundance == 'Mature', 
                               'Mature abundance', 
                               'Total abundance'))


# make scenario a factor
median_lambdas_to_plot_over_time$Scenario <- 
  factor(median_lambdas_to_plot_over_time$Scenario)
median_lambdas_to_plot_over_time$OSR <- 
  factor(median_lambdas_to_plot_over_time$OSR)
median_lambdas_to_plot_over_time$Abundance <- 
  factor(median_lambdas_to_plot_over_time$Abundance, 
         levels = abundances)

# plot figure - median
fig5b <- ggplot(data = median_lambdas_to_plot_over_time, 
                aes(x = Year, 
                    y = Lambda_10yr_median, 
                    color = Scenario, 
                    linetype = Mating_Function)) + 
  # facet_grid(cols = vars(TRT), rows = vars(facet_labels)) +
  facet_grid(cols = vars(TRT)) +
  geom_hline(yintercept = 1) +
  geom_ribbon(aes(ymin = Lambda_10yr_Q25,
                  ymax = Lambda_10yr_Q75, 
                  col = NULL, 
                  fill = Scenario),
              alpha = 0.25,
              show.legend = FALSE) +
  geom_path(lwd = 1) +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
  xlab('Year') +
  ylab('Median growth rate \n (averaged over 10 years)') +
  ggtitle('(10yr) median growth rates over time + IQR') +
  theme(plot.margin = unit(c(0.5, 0.25, 1, 1), units = 'cm')) +
  theme(axis.title.x = element_text(size = 13, vjust = -3)) +
  theme(axis.title.y = element_text(size = 13, vjust = 4)) +
  theme(axis.text = element_text(size = 10)) +
  theme(strip.text = element_text(size = 12)) +
  theme(title = element_text(size = 13)) +
  theme(legend.key.width = unit(2.65, "line")) +
  labs(lty = 'Mating \n Function')

# save to file
ggsave(plot = fig5b, 
       filename = paste('10yr_median_lambdas.png', sep = ''),
       path = '~/Projects/iliketurtles3/figures/',
       width = 9, height = 5)


