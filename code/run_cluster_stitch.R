# run cluster_stitch

source('cluster_stitch.R')

cluster_stitch(scenarios = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), 
               betas = c(1, 1.35, 1.94, 3.1, 6.57, 8.31, 11.19, 16.94, 34.14), 
               x = 240, 
               y = 9760)