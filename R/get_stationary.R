#' Get near-stationary states
#'
#' This function is applied to identify near-stationary states from candidate intervals based on the modified Cassidy's method
#' @param datasets The candidate intervals in list format
#' @param not Normal occupancy threshold (0.2 by default)
#' @param hot High occupancy threshold (0.4 by default)
#' @param xot Extremely high occupancy threshold (0.6 by default)
#' @param ndr The minimum acceptable duration in normal-occupancy periods (10 by default)
#' @param hdr The minimum acceptable duration in high-occupancy periods (4 by default)
#' @param xdr The minimum acceptable duration in extremely high-occupancy periods (2.5 by default)
#' @param gapt Gap threshold
#' @param relation Duration-occupancy relations; optional choices are "stepwise", "linear", "polynomial", and "unitgap"
#' @return List of near-stationary states
#' @export
get_stationary <- function (datasets, not = 0.2, hot = 0.4, xot = 0.6, ndr = NULL, hdr = NULL, xdr = NULL, gapt, relation) {
  # load package
  require(parallel)
  
  # call parameters
  not <- not
  hot <- hot
  xot <- xot
  ndr <- ndr
  hdr <- hdr
  xdr <- xdr
  gapt <- gapt
  relation <- relation
  
  # call function
  if (relation == "stepwise"){
    call_function <- do_stepwise
  }else if (relation == "linear") {
    call_function <- do_linear
  }else if (relation == "quadratic"){
    call_function <- do_quadratic
  }else if (relation == "unitgap"){
    call_function <- do_unitgap
  }

  # check stationarity for each candidate
  get_stat1 <- function (dataset) {
    # gap criterion
    gap_cri = max(mean(dataset$gap_count), mean(dataset$gap_occ)) <= (gapt/mean(dataset$sd_count))
    # duration criterion
    dur_cri = mean(dataset$dur) >= call_function(mean(dataset$can_occ), not, hot, xot, ndr, hdr, xdr)
    # judge
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
