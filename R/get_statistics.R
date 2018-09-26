#' Measure statistics of NSS
#'
#' This function is applied to measure quantity and quality of identified near-stationary states in the congested regime.
#' @param datasets The datasets of identified near-stationary states in the list format.
#' @param critical Pre-determined critical density by prior knowledge, e.g. 0.18
#' @param tolerance Minimum acceptable sample size; R-squared would be regarded zero if the sample size is smaller than the tolerance.
#' @export
get_statistics <- function(datasets, critical, tolerance) {
  # function to get statistics from one near-stationary state
  get_stats <- function(dataset) {
    count <- dataset$can_count[1]
    occ <- dataset$can_occ[1]
    points <- nrow(dataset)
    c(count, occ, points)
  }
  # get statistics from all near-stationary states
  stats0 <- lapply(datasets, get_stats)
  stats <- data.frame(matrix(unlist(stats0), ncol = 3, byrow = T), stringsAsFactors = F)
  colnames(stats) <- c("flow", "occ", "points")
  # calculate the number of near-stationary states
  N_states <- sum(stats$occ >= critical)
  # calculate the number of near-stationary points
  N_points <- sum(stats$points[stats$occ >= critical])
  # calculate R2
  R2 <- if (N_states < tolerance){
    0
  }else{
    summary(lm(stats$flow[stats$occ >= critical] ~ stats$occ[stats$occ >= critical]))$r.squared
  }
  # report outputs
  output <- data.frame(N_states, N_points, R2, stringsAsFactors = F)
  output
}

