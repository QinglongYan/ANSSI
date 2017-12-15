#' Get normalized series
#'
#' This function is applied to calculate normalized count and occupancy series
#' @param datasets The preprocessed list
#' @param rem Remove any lanes when calculating averaged count and occupancy series?
#' @return List of normalized datasets
#' @export
get_normalized <- function (datasets, rem = NULL) {
  # function for one dataset
  rem <- rem
  get_norm1 <- function (dataset) {
    # delete unused lanes?
    if (is.null(rem)) {
      dataset <- dataset
    } else {
      del_col <- -1*c(2*(rem+1), 2*(rem+1)+1)
      dataset <- dataset[,del_col]
    }
    # number of columns
    nc <- ncol(dataset)
    # number of lanes
    lanes <- (nc - 3)/2
    # averaged count and occupancy
    avg_count <- rowMeans(dataset[, seq(4, nc - 1, by = 2)])
    avg_occ <- rowMeans(dataset[, seq(5, nc, by = 2)])
    # normalized count and occupancy
    nor_count <- scale(avg_count)[, 1]
    nor_occ <- scale(avg_occ)[, 1]
    # means and standard deviations
    mean_count <- mean(avg_count)
    mean_occ <- mean(avg_occ)
    sd_count <- sd(avg_count)
    sd_occ <- sd(avg_occ)
    # dataset with more information
    more_dataset <- cbind(dataset, lanes, avg_count, avg_occ, nor_count, nor_occ,
                          mean_count, mean_occ, sd_count, sd_occ)
    return(more_dataset)
  }

  # apply the function onto all datasets
  output <- lapply(datasets, get_norm1)
  # return output
  return(output)
}
