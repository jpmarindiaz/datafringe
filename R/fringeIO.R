#' @export
writeFringe <- function(f,path = NULL){
  if(!isFringe(f))
    stop("not a fringe")
  name <- f$writeCSV(path)
  name
}

#' @export
readFringe <- function(path, forceDic = TRUE, name = NULL){
  name <- name %||% basename(path)
  dic <- NULL
  dataFile <- paste0(path,"-data.csv")
  dicFile <- paste0(path,"-dic_.csv")
  data <- read_csv(dataFile)
  if(forceDic){
    dic <- read_csv(dicFile)
  }
  fringe(data = data,dic = dic, name = name)
}


