#' Get near-stationary states
#'
#' This function is applied to identify near-stationary states from candidate intervals based on the modified Cassidy's method
#' @param datasets The candidate intervals in list format
#' @param mindr The minimum acceptable duration of free-flow period
#' @param maxdr The minimum acceptable duration of congested period
#' @param hot High occupancy threshold
#' @param gapt Gap threshold
#' @return List of near-stationary states
#' @export
get_stationary <- function (datasets, mindr = 4, maxdr = 10, hot = 0.4, gapt) {
  # load package
  require(parallel)
  # parameters
  mindr <- mindr
  maxdr <- maxdr
  hot <- hot
  gapt <- gapt
  # check stationarity for each candidate
  get_stat1 <- function (dataset) {
    # gap criterion
    gap_cri = max(mean(dataset$gap_flow), mean(dataset$gap_occ)) <= (gapt/mean(dataset$sd_count))
    # duration criterion
    dur_cri = mean(dataset$dur) >= (maxdr*(mean(dataset$can_occ) < hot) + mindr*(mean(dataset$can_occ) >= hot))

    if (gap_cri == TRUE && dur_cri == TRUE) {
      dataset
    }
  }
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  outputs <- parLapply(cl, datasets, get_stat1)
  stopCluster(cl)
  outputs[!sapply(outputs, is.null)]
}
