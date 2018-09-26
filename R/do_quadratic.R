#' Quadratic duration-occupancy relation
#'
#' This function is applied to calculate the minimum acceptable duration given the occupancy based on a polynomial duration-occupancy relation
#' @param occ Candidate occupancy, which is the input variable
#' @param not Normal-occupancy threshold(*)
#' @param hot High-occupancy threshold(*)
#' @param xot Extremely high-occupancy threshold(*)
#' @param ndr The minimum acceptable duration in normal-occupancy periods(*)
#' @param hdr The minimum acceptable duration in high-occupancy periods(*)
#' @param xdr The minimum acceptable duration in extremely high-occupancy periods(*)
#' @return Minimum acceptable duration
#' @export
do_quadratic <- function (occ, not, hot, xot, ndr, hdr, xdr) {
  # parameter solver
  x <- c(not, hot, xot)
  y <- c(ndr, hdr, xdr)
  coefs <- as.numeric(solve(cbind(x^2, x, 1), y))
  # quadratic function
  output <- ndr*(occ < not) + (coefs[1]*occ^2 + coefs[2]*occ + coefs[3])*(occ >= not & occ < xot) + xdr*(occ >= xot)
  output
}