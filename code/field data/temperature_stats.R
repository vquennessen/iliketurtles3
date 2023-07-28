# temperature stats for nesting

# clear environment
rm(list = ls())

# load libraries
library(dplyr)
library(lubridate)
library(readxl)
library(readr)
library(ggplot2)

##### create incubation data frame #############################################

# load nest object
load("~/Projects/iliketurtles3/data/nests.Rdata")
nests2 <- nests %>%
  select(Season, N_NINHO, TEMP_LOGGER_DEPLOYED, TEMP_LOGGER_REMOVED)

# define mode function
# Create mode() function to calculate mode
stat_mode <- function(x, na.rm = TRUE) {
  
  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}

# load in and clean up temp data

# initialize dataframe for each nest, with season, type of loger, CTE, 
# and month of incubation
incubation <- data.frame(nest_number = NA, 
                         season = NA, 
                         logger = NA, 
                         logger_number = NA, 
                         month = NA, 
                         CTE = NA)

### season 1
seasons <- c('season 1', 'season 2', 'season 3', 'season 4')
years <- c(2020, 2021, 2022, 2023)

for (s in 1:length(seasons)) {
  
  setwd(paste('C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/temperature files/', 
              seasons[s], sep = ''))
  
  # season 4 are .csv files, all others are .xlsx files
  if (s < 4) {
      filenames = dir(pattern="*.xlsx")
  } else { filenames = dir(pattern="*.csv") }
  
  
  tempDF <- data.frame(nest_number = rep(NA, length(filenames)), 
                           season = rep(NA, length(filenames)), 
                           logger = rep(NA, length(filenames)), 
                           logger_number = rep(NA, length(filenames)), 
                           month = rep(NA, length(filenames)), 
                           CTE = rep(NA, length(filenames)))
  
  # for each file
  for (f in 1:length(filenames)) {
    
    # load nest value
    if (s < 4) {
      temp <- read_xlsx(filenames[f])
    } else { temp <- read_csv(filenames[f], show_col_types = FALSE) }
    
    
    # add nest number, logger type, and logger number to dataframe
    tempDF$nest_number[f] <- temp$nest[1]
    tempDF$season[f] <- parse_number(seasons[s])
    tempDF$logger[f] <- temp$logger[1]
    tempDF$logger_number[f] <- temp$number[1]
    
    # extract correct row from nests2
    nest <- subset(nests2, Season == years[s] & N_NINHO == parse_number(as.character(temp$nest[1])))
    
    # make datetime variable actually datetime
    if (s == 4) { #} & (f %in% c(1, 2, 29))) {
      temp$datetime <- strptime(temp$datetime, format = '%m/%d/%Y %H:%M')
    } else { temp$datetime <- as_datetime(temp$datetime) }
    
    start <- mdy_hm(nest$TEMP_LOGGER_DEPLOYED)
    stop <- mdy_hm(nest$TEMP_LOGGER_REMOVED)
    
    # trim data based on middle third of incubation time
    incubation_dates <- temp %>%
      select(datetime, temperature) %>%
      filter(datetime > start & datetime < stop) 
    
    # extract month that most of the incubation happens in
    tempDF$month[f] <- stat_mode(as.numeric(month(incubation_dates$datetime)))
    
    # add CTE just as mean temperature for now
    tempDF$CTE[f] <- mean(parse_number(as.character((incubation_dates$temperature), na.rm = TRUE)))
    
    #### TO DO calculate CTE
    # M <- mean(incubation_dates$temperature)
    # R <- (max(incubation_dates$temperature) - min(incubation_dates$temperature)) / 2
    # T0 <- 16.4
    # T0 <- 14 # painted turtles, Telemeco et al.
    
  }
  
  incubation <- rbind(incubation, tempDF)
  
}

save(incubation, file = '../../data/incubation.Rdata')

##### stats ####################################################################

data <- incubation %>%
  filter(season != 1)  %>%
  summarise(sd(CTE))

# sd = 0.841012
  
ggplot(data, aes(x = as.factor(month), y = CTE, col = as.factor(season))) +
  geom_boxplot()

stats <- data %>%
  group_by(month) %>%
  summarise(mean = mean(CTE), 
            median = median(CTE), 
            count = n()) %>%
  mutate(frequency = count/sum(count))

# calculate average temperature given proportion and CTE in each month
sum(stats$mean*stats$frequency)
# 31.80387
