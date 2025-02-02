# calculate CTE

CTE <- function(x) {
  
  temperatures <- x %>%
    mutate(day = day(datetime)) %>%
    group_by(day) %>%
    summarise(mean_daily_temp = mean(temperature), 
              daily_temp_range = range(temperature))
  
  
}