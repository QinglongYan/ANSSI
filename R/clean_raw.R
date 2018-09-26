#' Clean extracted datasets
#'
#' This function is applied to filter datasets with low observation rate, long missing gap, impute missing values, and filter datasets with insufficient congestion periods
#' @param indir The directory where the extracted datasets are located
#' @param outdir The directory to save the preprocessed datasets
#' @param obs Minimum acceptable observation rate
#' @param hole Minimum acceptable hole size in minutes
#' @param tocc Occupancy threshold
#' @param len Minimum length of a congested period in minutes
#' @return Preprocessed datasets in csv format
#' @export
clean_raw <- function (indir, outdir, obs = 0.95, hole = 3, tocc = 0.3, len = 5) {
  # load package
  require(imputeTS)
  # list file names
  fnames <- list.files(indir)
  # number of files
  n <- length(fnames)
  # set up progress bar
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  # run loops
  for (i in 1:n) {
    # read one dataset
    data <- read.csv(paste(indir, fnames[i], sep = ""), header = T, as.is = T)
    # observation rate filter
    if (nrow(data) >= (2880*obs)) {
      # missing value filter
      if (max(diff(c(1, data$index, 2880))) <= (2*hole)) {
        # delete time column
        data$time <- NULL
        # number of columns
        nc <- ncol(data)
        ## generate full data frame with NA in count and occ
        full_index <- match(1:2880, data$index)
        na_index <- which(is.na(full_index))
        full_data <- data[full_index,]
        # fill in date and id
        full_data[na_index, c(1,2)] <- full_data[min(which(!is.na(full_index))),c(1,2)]
        # fill in index
        full_data[na_index, 3] <- na_index
        # fill in count and occ
        for (j in 4:nc) {
          if (j %% 2 == 0){
            full_data[,j] <- round(na.interpolation(full_data[,j]))
          }else{
            full_data[,j] <- round(na.interpolation(full_data[,j]), 4)
          }
        }
        # congestion filter
        crit <- max(rle(diff(which(as.numeric(rowMeans(full_data[,seq(5,nc,by=2),drop = FALSE]))>=tocc)))$lengths, -1)+1
        if (crit >= (2*len)) {
          write.csv(full_data, paste(outdir, gsub("extracted", "filtered", fnames[i]), sep = ""), row.names = F)
        }
      }
    }
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  # close progress bar
  close(pb)
}

