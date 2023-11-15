# calculate number of sims needed based on variance of runs

# load libraries
library(ggplot2)

# set working directory
setwd("C:/Users/Vic/Documents/Projects/MS-thesis/code/variance")

# set number of sims
num_sims <- 10000
num_medians <- 100
num_variances <- 100

# load datafiles - total abundance, and abundance mature
load(paste('../../data/Variance/CAB_OR_2019/', num_sims, '_biomass.Rda', sep = ''))
load(paste('../../data/Variance/CAB_OR_2019/', num_sims, '_biomass.Rda', sep = ''))

# set different sample sizes
sample_size <- seq(from = 1000, to = num_sims, by = 100)

# initialize variance dataframe 
variance_df <- data.frame(Sample.Size = sample_size,
                          A.Variance = rep(NA, length(sample_size)), 
                          M.Variance = rep(NA, length(sample_size)))

A_variance <- rep(0, num_variances)
M_variance <- rep(0, num_variances)

A_medians1 <- rep(0, num_medians)
M_medians1 <- rep(0, num_medians)

for (i in 1:length(sample_size)) {
  
  for (j in 1:num_variances) {
    
    for (k in 1:num_medians) {
      
      indices <- sample(1:num_sims, sample_size[i])
      sampled_Atotal <- colSums(sims_abundance_total[, 21, 1, 1, indices])
      sampled_Amature <- sims_mature_abundance[21, 1, 1, indices]
      
      # calculate medians 
      A_medians1[k] <- median(sampled_biomass)
      M_medians1[k] <- median(sampled_yield)
      
    }
    
    A_variance[j] <- var(B_medians1)
    M_variance[j] <- var(Y_medians1)
    
  }
  
  variance_df$A.Variance[i] <- median(A_variance)
  variance_df$M.Variance[i] <- median(M_variance)
  
}

A <- ggplot(data = variance_df, aes(x = Sample.Size, y = A.Variance)) +
  geom_point() +
  ggtitle('Biomass Variance')

M <- ggplot(data = variance_df, aes(x = Sample.Size, y = M.Variance)) +
  geom_line() +
  ggtitle('Yield Variance')

ggsave(filename = paste('Abundance_total_variance.png', sep = ''), 
       plot = A, 
       path = 'C:/Users/Vic/Box/Quennessen_Thesis/PhD Thesis/model output/variance runs')

ggsave(filename = paste('Abundance_mature_variance.png', sep = ''), 
       plot = M, 
       path = 'C:/Users/Vic/Box/Quennessen_Thesis/PhD Thesis/model output/variance runs')
