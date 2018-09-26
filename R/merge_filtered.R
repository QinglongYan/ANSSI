#' Merge filtered datasets
#'
#' This function is applied to bind datasets on the same day from two different VDS's in columns
#' @param inDir1 The directory where the first half datasets are stored
#' @param inDir2 The directory where the second half datasets are stored
#' @param outDir The directory where the merged datasets are stored
#' @return Merged datasets in the format of csv
#' @export
merge_filter <- function(inDir1, inDir2, outDir) {
  # load packages
  require(data.table)
  # check equivalence
  fnames1 <- list.files(inDir1)
  fnames2 <- list.files(inDir2)
  eq <- sort(substr(fnames1, nchar(fnames1) - 13, nchar(fnames1) - 4)) == sort(substr(fnames2, nchar(fnames2) - 13, nchar(fnames2) - 4))
  if (sum(eq == FALSE) != 0) {
    stop("Dates are not identical from two VDS's")
  }
  for (i in 1:length(fnames1)) {
    ## adjust dataset1
    # load data
    data1 <- as.data.frame(fread(paste(inDir1, fnames1[i], sep = "")))
    # lane number
    ln1 <- (ncol(data1) - 3) / 2
    # number of columns
    n1 <- ncol(data1)
    # rename count
    colnames(data1)[seq(4, n1 - 1, 2)] <- as.vector(outer(paste("count_", data1$id[1], "_", sep = ""), 1:ln1, FUN = "paste", sep = ""))
    # rename occ
    colnames(data1)[seq(5, n1, 2)] <- as.vector(outer(paste("occ_", data1$id[1], "_", sep = ""), 1:ln1, FUN = "paste", sep = ""))
    # change id
    data1$id <- "combined"
    ## adjust dataset2
    # load data
    data2 <- as.data.frame(fread(paste(inDir2, fnames2[i], sep = "")))
    # lane number
    ln2 <- (ncol(data2) - 3) / 2
    # number of columns
    n2 <- ncol(data2)
    # rename count
    colnames(data2)[seq(4, n2 - 1, 2)] <- as.vector(outer(paste("count_", data2$id[1], "_", sep = ""), 1:ln2, FUN = "paste", sep = ""))
    # rename occ
    colnames(data2)[seq(5, n2, 2)] <- as.vector(outer(paste("occ_", data2$id[1], "_", sep = ""), 1:ln2, FUN = "paste", sep = ""))
    # remove first three columns
    data2[, c(1:3)] <- NULL
    # combine
    final <- cbind(data1, data2)
    # save data
    write.csv(final, paste(outDir, "combined_", substr(fnames1[i], nchar(fnames1[i]) - 13, nchar(fnames1[i])), sep = ""), row.names = F)
  }
}




