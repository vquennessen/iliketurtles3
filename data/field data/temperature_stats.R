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
load("~/Projects/iliketurtles3/output/nests.Rdata")
nests2 <- nests %>%
  select(Season, N_NINHO, TEMP_LOGGER_DEPLOYED, TEMP_LOGGER_REMOVED, 
         Hatching_success)

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

# initialize dataframe for each nest, with season, type of logger, CTE, 
# month of incubation, and hatching success
incubation <- data.frame(nest_number = NA, 
                         season = NA, 
                         logger = NA, 
                         logger_number = NA, 
                         month = NA, 
                         CTE = NA, 
                         CTE_all = NA, 
                         hatch_success = NA)

### season 1
seasons <- c('season 1', 'season 2', 'season 3', 'season 4')
years <- c(2020, 2021, 2022, 2023)

for (s in 1:length(seasons)) {
  
  directory <- paste('C:/Users/Vic/Box Sync/Quennessen_Thesis/PhD Thesis/temperature files/', 
                     seasons[s], sep = '')
  
  setwd(directory)
  
  # season 4 are .csv files, all others are .xlsx files
  if (s < 4) {
      filenames = dir(pattern="*.xlsx")
  } else { filenames = dir(pattern="*.csv") }
  
  
  tempDF <- data.frame(nest_number = rep(NA, length(filenames)), 
                       season = rep(NA, length(filenames)), 
                       logger = rep(NA, length(filenames)), 
                       logger_number = rep(NA, length(filenames)), 
                       month = rep(NA, length(filenames)), 
                       CTE = rep(NA, length(filenames)), 
                       CTE_all = rep(NA, length(filenames)),
                       hatch_success = rep(NA, length(filenames)))
  
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
    tempDF$CTE[f] <- mean(parse_number(as.character((incubation_dates$temperature), 
                                                    na.rm = TRUE)))
    
    # add CTE just as mean temperature for now
    tempDF$CTE_all[f] <- mean(parse_number(as.character((temp$temperature), 
                                                        na.rm = TRUE)))
    
    # add hatching success
    tempDF$hatch_success[f] <- nest$Hatching_success
      
    
    #### TO DO calculate CTE
    # M <- mean(incubation_dates$temperature)
    # R <- (max(incubation_dates$temperature) - min(incubation_dates$temperature)) / 2
    # T0 <- 16.4
    # T0 <- 14 # painted turtles, Telemeco et al.
    
  }
  
  incubation <- rbind(incubation, tempDF)
  
}

save(incubation, file = 'incubation_temps.Rda')

##### stats ####################################################################

data <- incubation %>%
  filter(season != 1)  %>%
  na.omit(hatch_success) 

sd_CTE <- data %>%
  summarise(sd(CTE, na.rm = TRUE))
# sd = 0.8333391
  
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
# 31.80488
# with median = 31.8335

# calculate relationship between CTE and hatching success
data2 <- incubation %>%
  select(CTE, CTE_all, hatch_success) %>%
  na.omit()

mod1 <- lm(data = data2, hatch_success ~ CTE)
summary(mod1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.35619    0.55186  -2.457 0.014830 *  
#   CTE          0.06754    0.01730   3.903 0.000129 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1994 on 203 degrees of freedom
# Multiple R-squared:  0.06981,	Adjusted R-squared:  0.06522 
# F-statistic: 15.23 on 1 and 203 DF,  p-value: 0.0001292

# howard et al. 2015 - 4th order polynomial
mod2 <- lm(data = data2, hatch_success ~ poly(CTE, 4))
summary(mod2)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.79709    0.01274  62.586  < 2e-16 ***
#   poly(CTE, 4)1  0.77817    0.18235   4.267 3.05e-05 ***
#   poly(CTE, 4)2 -1.11916    0.18235  -6.137 4.43e-09 ***
#   poly(CTE, 4)3  0.25160    0.18235   1.380   0.1692    
# poly(CTE, 4)4 -0.32112    0.18235  -1.761   0.0798 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1823 on 200 degrees of freedom
# Multiple R-squared:  0.2334,	Adjusted R-squared:  0.218 
# F-statistic: 15.22 on 4 and 200 DF,  p-value: 6.988e-11

x1 <- seq(from = min(data2$CTE), to = max(data2$CTE), by = 0.01)
y1 <- (0.79709 + 0.77817*x1 - 1.11916*x1^2 + 0.25160*x1^3 - 0.32112*x1^4) / 100 

prd <- data.frame(CTE = seq(from = range(data2$CTE)[1], to = range(data2$CTE)[2], 
                            length.out = 100))
err <- predict(mod2, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

ggplot(prd, aes(x = CTE, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = data2, aes(x = CTE, y = hatch_success)) +
  ylab('Hatching Success') +
  xlab('Middle third average incubation temperature (C)') +
  ggtitle("Howard et al. 2015")

# laloe et al. 2017 - whole incubation period
A <- 0.86
beta <- -1.7
t0 <- 32.7
x2 <- seq(from = min(data2$CTE_all), to = max(data2$CTE_all), by = 0.01)
y2 <- A / (1 + exp(-beta * (x2 - t0)))
laloe <- data.frame(CTE = x2, hatch_success = y2)

ggplot(data2, aes(x = CTE_all, y = hatch_success)) +
  theme_bw() +
  geom_point() +
  geom_line(data = laloe, aes(x = CTE, y = hatch_success), 
            lwd = 2) +
  ylab('Hatching Success') +
  xlab('Average incubation temperature (C)') +
  ggtitle("Laloe et al. 2017")

# laloe et al. 2017 - middle third of incubation 
A <- 0.86
beta <- -1.7
t0 <- 32.7
x3 <- seq(from = min(data2$CTE), to = max(data2$CTE), by = 0.01)
y3 <- A / (1 + exp(-beta * (x3 - t0)))
laloe2 <- data.frame(CTE = x3, hatch_success = y3)

ggplot(data2, aes(x = CTE, y = hatch_success)) +
  theme_bw() +
  geom_point() +
  geom_line(data = laloe2, aes(x = CTE, y = hatch_success), 
            lwd = 2) +
  ylab('Hatching Success') +
  xlab('Middle third average incubation temperature (C)') +
  ggtitle("Laloe et al. 2017")
