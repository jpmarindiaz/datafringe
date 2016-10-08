#' Creates a new Datafringe from a dataframe
#' @name sampleData
#' @description Creates a new datapackage from json, data fringe, list of data fringes, list of data fringees (see Datafringe reference class)
#' @param d might be a json string, data fringe or list of data fringes.
#' @return dp
#' @export
#' @examples \dontrun{
#' fringe <- newDatafringeFromDatafringe(mtcars)
#' }
sampleData <- function(ftype, asFringe=FALSE){
  dir <- system.file("sampledata",package="fringer", mustWork=TRUE)
  if(!ftype %in% availableSampleData()){
    stop("No data for this ftype")
    }
  else{
    filename <- paste0("data",ftype,".csv")
    out <- read.csv(file.path(dir,filename), stringsAsFactors=FALSE)
  }
  if(asFringe){out <- fringe(out)}
  out
}


#' @export
#'
availableSampleData <- function(){
  dir <- system.file("sampledata",package="fringer", mustWork=TRUE)
  files <- list.files(dir)
  files <- files[grepl("^data.*csv$",files)]
  x <- gsub("data","",files)
  x <- gsub(".csv","",x)
  x
}


