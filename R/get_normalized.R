#' Get normalized datasets
#'
#' The function is to normalize total vehicle c
#' ounts and occupancies
#' @param datasets A list of filtered datasets
#' @return A list of normalized datasets
#' @export
get_normalized <- function (datasets) {
  # function for one dataset
  get_norm1 <- function (dataset) {
    # number of columns
    nc <- ncol(dataset)
    # number of lanes
    lanes <- (nc - 3)/2
    # averaged count and occupancy
      avg_count <- rowMeans(dataset[, seq(4, nc - 1, by = 2), drop = FALSE])
      avg_occ <- rowMeans(dataset[, seq(5, nc, by = 2), drop = FALSE])
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
