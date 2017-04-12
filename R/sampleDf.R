


#' #' @export
#' #'
#' availableSampleData <- function(){
#'   dir <- system.file("sampledata",package="fringer", mustWork=TRUE)
#'   files <- list.files(dir)
#'   files <- files[grepl("^data.*csv$",files)]
#'   x <- gsub("data","",files)
#'   x <- gsub(".csv","",x)
#'   x
#' }


