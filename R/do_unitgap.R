#' Unit gap duration-occupancy relation
#'
#' This function is applied to calculate the minimum acceptable duration given the occupancy based on an inverse linear duration-occupancy relation
#' @param occ Candidate occupancy, which is the input variable
#' @param lot Low-occupancy threshold(*)
#' @param hot High-occupancy threshold
#' @param xhot Extremely high-occupancy threshold(*)
#' @param ldr The minimum acceptable duration in low-occupancy periods(*)
#' @param hdr The minimum acceptable duration in high-occupancy periods
#' @param xhdr The minimum acceptable duration in extremely high-occupancy periods(*)
#' @return Minimum acceptable duration
#' @export
do_unitgap <- function (occ, lot, hot, xhot, ldr, hdr, xhdr) {
  # Unit gap function
  unit <- (1/ldr)*(occ < lot) + ((1/xhdr - 1/ldr)/(xhot - lot)*(occ - lot) + 1/ldr)*(occ >= lot & occ < xhot) + (1/xhdr)*(occ >= xhot)
  # output
  output <- 1/unit
  output
}