emergence_success <- function(clutch_temps, A, k, t0, thermal_limit) {
  
  # calculate expected emergence success
  # prob_emergence_success <- A / (1 + exp(-k * (clutch_temps - t0)))
  emergence_success <- A / (1 + exp(-k * (clutch_temps - t0)))

  # emergence success of 0 for any clutches with an average incubation 
  # temperature at or above the thermal limit
  emergence_success[which(clutch_temps >= thermal_limit)] <- 0
  
  return(emergence_success)
  
}
