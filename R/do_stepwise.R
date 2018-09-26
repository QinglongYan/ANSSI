#' Stepwise duration-occupancy relation
#'
#' This function is applied to calculate the minimum acceptable duration given the occupancy based on a stepwise duration-occupancy relation
#' @param occ Candidate occupancy, which is the input variable
#' @param not Normal-occupancy threshold
#' @param hot High-occupancy threshold(*)
#' @param xot Extremely high-occupancy threshold
#' @param ndr The minimum acceptable duration in normal-occupancy periods(*)
#' @param hdr The minimum acceptable duration in high-occupancy periods(*)
#' @param xdr The minimum acceptable duration in extremely high-occupancy periods
#' @return Minimum acceptable duration
#' @export
do_stepwise <- function (occ, not, hot, xot, ndr, hdr, xdr) {
  # stepwise function
  output <- ndr*(occ < hot) + hdr*(occ >= hot)
  output
}

