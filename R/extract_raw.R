#' Extract raw datasets from PeMS
#'
#' This function is applied to extract raw datasets from Data Clearinghouse of PeMS and save the extracted datasets in csv format.
#' @param indir The directory where the PeMS raw gz files are located
#' @param outdir The directory to save the extracted csv files
#' @param id station ID, which is the VDS in PeMS
#' @param lanes number of lanes
#' @return Extracted raw datasets in csv format
#' @export
extract_raw <- function (indir, outdir, id, lanes) {
  # list file names
  fnames <- list.files(indir)
  # number of files
  n <- length(fnames)
  # set up progress bar
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  # run loops
  for (i in 1:n) {
    # read one dataset
    data <- read.table(paste(indir, fnames[i], sep = ""), sep = ",", header = F, as.is = T)
    # select needed rows and columns
    sel_data <- data[data$V2 == id,sort(c(1,2,seq(3, 3*lanes, 3), seq(4, 3*lanes+1, 3)))]
    # column names
    colnames(sel_data) <- c("datetime", "id", as.vector(outer(c("count", "occ"), 1:lanes, paste, sep="_")))
    # create date column
    sel_data$date <- substr(sel_data$datetime, 1, 10)
    # create time column
    sel_data$time <- substr(sel_data$datetime, 12, 19)
    # create index column
    dt <- strptime(sel_data$datetime, format = "%m/%d/%Y %H:%M:%S", tz = "GMT")
    sel_data$index <- floor(as.numeric(dt)%%86400/30) + 1
    # drop datetime column
    sel_data$datetime <- NULL
    # adjust column position
    sel_data <- sel_data[,c(ncol(sel_data)-2, 1, ncol(sel_data)-1, ncol(sel_data), 2:(ncol(sel_data)-3))]
    # extract date from file name
    fdate <- substr(fnames[i], 22, 31)
    # save dataset
    write.csv(sel_data, paste(outdir, "extracted_", id, "_", fdate, ".csv", sep = ""), row.names = F)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  # close progress bar
  close(pb)
}


