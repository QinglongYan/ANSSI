#' Read preprocessed data
#'
#' This function is applied to read preprocessed datasets in a list
#' @param dir The directory of the preprocessed datasets
#' @return List of preprocessed datasets
#' @export
read_filtered <- function (dir) {
  # list file names
  fnames <- list.files(dir)
  # read datasets
  datasets <- lapply(paste(dir, fnames, sep = ""), read.csv, header = T, as.is = T)
  # list names
  names(datasets) <- paste(gsub("_", "-", substr(fnames, nchar(fnames)-13, nchar(fnames)-4)), "-", sep = "")
  # return
  return(datasets)
}


