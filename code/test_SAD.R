# SAD test

rm(list = ls())

# source functions
source('mating function/OSRs_to_betas.R')
source('test_initialize_population.R')

# stricter sample function
resample <- function(x, ...) x[sample.int(length(x), ...)]

# load libraries
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(lgcp)
library(purrr)
library(slider)
library(abind)

# function arguments
arguments <- data.frame(Var1 = 'narrow', 
                        Var2 = 6.64,
                        Var3 = TRUE,
                        Var4 = 'emergence_success_t0',
                        Var5 = 'effective', 
                        Var6 = 1, 
                        Var7 = 500, 
                        Var8 = gsub('-', '_', Sys.Date()))

test_initialize_population(arguments)
