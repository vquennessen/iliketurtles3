proportion_male <- function(clutch_temps, k, pivotal_temp) {
  
  props_male <- 1 / (1 + exp(-k * (clutch_temps - (pivotal_temp))))
  
  return(props_male)
  
}