#' Extract 30-sec raw data
#'
#' Extract 30-sec raw data of selected vehicle detector stations (VDS's) from gz files downloaded from PeMS data clearinghouse and save the daily extracted datasets in the format of csv.
#' @param inDir The directory where the PeMS raw gz files are located
#' @param metaDir The directory where meta data files are located. Meta data files should cover the selected period.
#' @param outDir The directory to save the extracted csv files
#' @param from From date in the format of YYYY_MM_DD
#' @param to To date in the format of YYYY_MM_DD
#' @param vds A vector of vehicle detector station (VDS) ID's
#' @return Extracted raw datasets in csv format
#' @export
extract_raw <- function (inDir, metaDir, outDir, from = NULL, to = NULL, vds) {
  # required packages
  require(data.table)
  # meta data file names
  meta_names <- list.files(metaDir)
  # obtain the number of lanes for each station
  lanes <- sapply(vds, function(x){
    lane <- sapply(meta_names, function(y){
      # read meta data
      meta <- fread(paste(metaDir, y, sep = ""))
      meta[meta$ID == x]$Lanes
    })
    ulane <- unique(lane)
    if (length(ulane) == 1){
      ulane
    }else{
      stop(paste("The number of lanes of VDS", x, "has changed in the selected period."))
    }
  })
  # list file names
  fnames <- list.files(inDir)
  if (is.null(c(from, to))){
    fnames <- fnames
  }else{
    fnames <- fnames[which(grepl(from, fnames)):which(grepl(to, fnames))]
  }
  # number of files
  n <- length(fnames)
  # set up progress bar
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  # run loop
  for (i in 1:n){
    # read one dataset
    data <- read.table(paste(inDir, fnames[i], sep = ""), sep = ",", header = F, as.is = T)
    lapply(vds, function(j){
      # select needed rows and columns
      sel_data <- data[data$V2 == j,sort(c(1,2,seq(3, 3*lanes[which(vds==j)], 3), seq(4, 3*lanes[which(vds==j)]+1, 3)))]
      # column names
      colnames(sel_data) <- c("datetime", "id", as.vector(outer(c("count", "occ"), 1:lanes[which(vds==j)], paste, sep="_")))
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
      write.csv(sel_data, paste(outDir, "extracted_", j, "_", fdate, ".csv", sep = ""), row.names = F)
    })
      # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
}


