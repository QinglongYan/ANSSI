#' Cut filtered datasets
#'
#' The function is to select a subset of lane traffic flow characteristics from each dataset
#' @param inDir The directory where filtered datasets are stored
#' @param outDir The directory where cut datasets are stored
#' @param remove A vector of lane numbers that you want to remove
#' @return Cut datasets in the format of csv
#' @export
cut_filtered <- function (inDir, outDir, remove) {
  # load package
  require(data.table)
  # file names
  fnames <- list.files(inDir)
  # implement cut to all datasets
  output <- lapply(fnames, function(fname){
    # read data
    dataset <- as.data.frame(fread(paste(inDir, fname, sep = "")))
    # deleted column numbers
    delete_col <- -1*c(2*(remove+1), 2*(remove+1)+1)
    # sub data
    sub <- dataset[,delete_col]
    # save
    fwrite(sub, paste(outDir, gsub("filtered", "cut", fname), sep = ""))
  })
}


