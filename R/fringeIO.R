#' @export
writeFringe <- function(f,path = NULL){
  if(!isFringe(f))
    stop("not a fringe")
  name <- f$writeCSV(path)
  name
}

#' @export
readFringe <- function(path, forceDic = TRUE, name = NULL, verbose = FALSE, n_max = Inf, excludeCols = NULL){
  name <- name %||% basename(path)
  if(verbose) message("Reading: ",name,"\n")
  dic <- NULL
  dataFile <- paste0(path,"_data.csv")
  dicFile <- paste0(path,"_dic_.csv")
  data <- read_csv(dataFile,n_max = n_max)
  if(forceDic){
    dic <- read_csv(dicFile)
  }
  if(!is.null(excludeCols)){
    keepCols <- names(data)[!names(data) %in% excludeCols]
    data <- data[keepCols]
    dic <- dic %>% filter(!id %in% excludeCols)
  }
  fringe(data = data,dic = dic, name = name)
}


