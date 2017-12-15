#' Get candidate intervals
#'
#' This function is applied to detect candidate intervals based on the PELT changepoint detection method
#' @param datasets The normalized datasets in list format
#' @param penalty The penalty for the PELT changepoint detection method
#' @return List of candidate intervals
#' @export
get_candidate <- function (datasets, penalty) {
  # required packages
  require(parallel)
  # penalty
  penalty <- penalty
  # function to get candidates from one dataset
  get_cand1 <- function(dataset) {
    # required packages
    require(changepoint)
    require(parallel)
    # calculate changepoints
    fit_count <- cpt.mean(dataset$nor_count, method = "PELT",
                         penalty = "Manual", pen.value = paste(penalty, "*", "log(n)", sep = ""))
    fit_occ <- cpt.mean(dataset$nor_occ, method = "PELT",
                        penalty = "Manual", pen.value = paste(penalty, "*", "log(n)", sep = ""))
    cpt_count <- fit_count@cpts
    cpt_occ <- fit_occ@cpts
    # union of changepoints from count and occ series
    cpt <- sort(Reduce(union, list(cpt_count, cpt_occ, 0))) # include 0 and 2880
    # function to get characteristics from one candidate
    get_char <- function(x) {
      # load required package
      library(chron)
      # start and end time index
      spt = cpt[x] + 1
      ept = cpt[x + 1]
      # define a time axis
      time_seq <- c(as.character(times(0:2879/2880)), "24:00:00")
      # start and end time index
      stm = time_seq[spt]
      etm = time_seq[ept+1]
      # length
      len = ept - spt + 1
      # duration
      dur = 1/60 * 30 * len
      # previous information
      info = dataset[spt:ept, ]
      # candidate flowrate and occupancy
      can_flow = 60/dur * sum(info$avg_count)
      can_occ = 1/len * sum(info$avg_occ)
      # flow and occupancy gaps
      gap_flow = max(abs(cumsum(info$nor_count) - fitted.values(lm(cumsum(info$nor_count) ~ c(spt:ept)))))
      gap_occ = max(abs(cumsum(info$nor_occ) - fitted.values(lm(cumsum(info$nor_occ) ~ c(spt:ept)))))
      # combine output
      output <- data.frame(info, spt, ept, stm, etm, len, dur, can_flow, can_occ, gap_flow, gap_occ, stringsAsFactors = F)
      return(output)
    }
    # parallel computing
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores)
    outputs <- parLapply(cl, 1:(length(cpt)-1), get_char)
    #outputs <- lapply(1:(length(cpt)-1), get_char)
    stopCluster(cl)
    return(outputs)
  }
  # parallel computing
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  CI <- parLapply(cl, datasets, get_cand1)
  stopCluster(cl)
  unlist(CI, recursive = FALSE)
}


