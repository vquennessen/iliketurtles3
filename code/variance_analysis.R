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
num_medians <- 100
num_variances <- 100

# load datafiles - total abundance for 3C scenario with beta = 6.57 and 
# 1.5C scenario with beta = 11.19
base <- 'C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/'
load(paste(base, '2023_07_21_newdata_3gen/3C/beta6.57/10000_abundance_total.Rda', sep = ''))
v3_6_57 <- sims_abundance_total
load(paste(base, '2023_07_21_newdata_3gen/1.5C/beta11.19/10000_abundance_total.Rda', sep = ''))
v1_5_11_19 <- sims_abundance_total

# set different sample sizes
sample_size <- seq(from = 1000, to = num_sims, by = 100)

# initialize variance dataframe 
variance_df <- data.frame(Sample.Size = sample_size,
                          A.Variance1 = rep(NA, length(sample_size)), 
                          A.Variance2 = rep(NA, length(sample_size)))

A_variance1 <- rep(0, num_variances)
A_variance2 <- rep(0, num_variances)

A_medians1 <- rep(0, num_medians)
A_medians2 <- rep(0, num_medians)

for (i in 1:length(sample_size)) {
  
  for (j in 1:num_variances) {
    
    for (k in 1:num_medians) {
      
      indices <- sample(1:num_sims, sample_size[i])
      sampled_Atotal_1 <- v3_6_57[256, indices]
      sampled_Atotal_2 <- v1_5_11_19[256, indices]
      
      # calculate medians 
      A_medians1[k] <- median(sampled_Atotal_1)
      A_medians2[k] <- median(sampled_Atotal_2)
      
    }
    
    A_variance1[j] <- var(A_medians1)
    A_variance2[j] <- var(A_medians2)
    
  }
  
  variance_df$A.Variance1[i] <- median(A_variance1)
  variance_df$A.Variance2[i] <- median(A_variance2)
  
}

A <- ggplot(data = variance_df, aes(x = Sample.Size, y = A.Variance1)) +
  geom_point() +
  ggtitle('Total Abundance Variance 1')

M <- ggplot(data = variance_df, aes(x = Sample.Size, y = A.Variance2)) +
  geom_line() +
  ggtitle('Total Abundance Variance 1')

ggsave(filename = paste('Abundance_total_variance1.png', sep = ''), 
       plot = A, 
       path = 'C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/variance runs')

ggsave(filename = paste('Abundance_total_variance2.png', sep = ''), 
       plot = M, 
       path = 'C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/model output/variance runs')

toc()
