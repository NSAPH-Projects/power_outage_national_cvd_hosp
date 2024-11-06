# helper functions for effect modification 

find_quartiles <- function(x) {
  # calculate the quartile boundaries
  quartiles <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  # classify each number into quartiles
  quartile_labels <- findInterval(x, vec = c(-Inf, quartiles, Inf))
  
  return(quartile_labels)
}
