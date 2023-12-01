# calculate number of sims needed based on variance of runs

# clear environment
rm(list = ls())

# start timing

# load libraries
library(ggplot2)
library(tictoc)

# start timer
tic()

# set working directory
setwd("C:/Users/vique/Documents/Projects/iliketurtles3/code/")

# set number of sims
num_sims <- 10000
num_variances <- 1000
scenarios <- c(1.5, 3)
betas <- c(11.19, 6.57)

# load datafiles - total abundance for 3C scenario with beta = 6.57 and 
# 1.5C scenario with beta = 11.19
base <- 'C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/'
# v3_6_57 <- sims_abundance_total
# load(paste(base, '2023_07_21_newdata_3gen/1.5C/beta11.19/10000_abundance_total.Rda', sep = ''))
# v1_5_11_19 <- sims_abundance_total

# set different sample sizes
sample_size <- seq(from = 1000, to = num_sims, by = 100)

# initialize variance dataframe 
variance_df <- data.frame(Scenario = rep(c('1.5C', '3C'), each = length(sample_size)),
                          Beta = rep(c(11.19, 6.57), each = length(sample_size)),
                          Sample.Size = rep(sample_size, times = length(scenarios)),
                          Variance = rep(NA, length(sample_size)))


for (s in 1:length(scenarios)) {
  
  # load total abundance for scenario
  load(paste(base, '2023_07_21_newdata_3gen/', scenarios[s], 'C/beta', betas[s], 
             '/10000_abundance_total.Rda', sep = ''))

  for (i in 1:length(sample_size)) {
    
    # reset variances vector
    variances <- rep(0, num_variances)

    for (j in 1:num_variances) {
      
      indices <- sample(1:num_sims, sample_size[i])
      variances[j] <- var(sims_abundance_total[256, indices])

    }
    
    # calculate median
    index <- (s - 1)*length(sample_size) + i
    variance_df$Variance[index] <- median(variances)

  }
  
}

A <- ggplot(data = subset(variance_df, Scenario == '1.5C'), aes(x = Sample.Size, y = Variance)) +
  geom_point() +
  ggtitle('Total Abundance Variance 1.5C, beta = 11.19')

B <- ggplot(data = subset(variance_df, Scenario == '3C'), aes(x = Sample.Size, y = Variance)) +
  geom_point() + 
  ggtitle('Total Abundance Variance 3C, beta = 6.57')

ggsave(filename = paste('Abundance_total_variance_1.5_11.19.png', sep = ''), 
       plot = A, 
       path = 'C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/variance runs')

ggsave(filename = paste('Abundance_total_variance_3_6.57.png', sep = ''),
       plot = B,
       path = 'C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/variance runs')

toc()
# num_variances = 100
# 16.63 seconds
# num_variances = 1000
# 140.1 seconds
