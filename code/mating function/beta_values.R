# function to get beta axis labels that are nice round numbers

# load libraries
library(ggplot2)
library(dplyr)

# values of operational sex ratios to plot
x <- seq(from = 0, to = 0.5, by = 0.0001)

# beta values to try
betas <- seq(from = 1, to = 100, by = 0.01)

# initialise DF
DF <- data.frame(Operational_Sex_Ratio = rep(x, times = length(betas)), 
                 Beta = as.numeric(rep(betas, each = length(x))), 
                 Reproductive_Success = NA)

# print(paste(lubridate::now(), ' - beta loop starting', sep = ''))

# CDF function
for (b in 1:length(betas)) {
  
  start <- (b - 1)*length(x) + 1
  stop <- start + length(x) - 1
  DF$Reproductive_Success[start:stop] <- pbeta(2 * x, 
                                               shape1 = 1, 
                                               shape2 = betas[b])
  
  if ((b/(length(betas) - 1)*100) %% 1 == 0) {
    
    print(paste(lubridate::now(), ' - ', round(b/length(betas)*100), '% done!', sep = '')) }
  
}

# make Beta factor
DF$Beta <- as.factor(DF$Beta)

# save object
save(DF, file = 'code/mating function/potential_beta_values.Rda')

# extract values from each beta curve that corresponds to a 10% decrease in
# reproductive success
acceptable_loss_of_males <- DF %>%
  group_by(Beta) %>%
  filter(Reproductive_Success <= 0.99) %>%
  filter(Operational_Sex_Ratio == max(Operational_Sex_Ratio)) %>%
  filter(((Operational_Sex_Ratio*100) %% 1 <= 0.05))

