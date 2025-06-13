emergence_success <- function(clutch_temps, A, k, t0) {
  
  # calculate emergence success
  emergence_success <- A / (1 + exp(-k * (clutch_temps - t0)))
  
  return(emergence_success)
  
}
