#' Linear duration-occupancy relation
#'
#' This function is applied to calculate the minimum acceptable duration given the occupancy based on a linear duration-occupancy relation
#' @param occ Candidate occupancy, which is the input variable
#' @param lot Low-occupancy threshold(*)
#' @param hot High-occupancy threshold
#' @param xhot Extremely high-occupancy threshold(*)
#' @param ldr The minimum acceptable duration in low-occupancy periods(*)
#' @param hdr The minimum acceptable duration in high-occupancy periods
#' @param xhdr The minimum acceptable duration in extremely high-occupancy periods(*)
#' @return Minimum acceptable duration
#' @export
do_linear <- function (occ, lot, hot, xhot, ldr, hdr, xhdr) {
  # linear function
  output <- ldr*(occ < lot) + ((xhdr - ldr)/(xhot - lot)*(occ - lot) + ldr)*(occ >= lot & occ < xhot) + xhdr*(occ >= xhot)
  output
}