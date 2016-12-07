#' Creates a new Datafringe from a dataframe
#' @name sampleData
#' @description Creates a new datapackage from json, data fringe, list of data fringes, list of data fringees (see Datafringe reference class)
#' @param d might be a json string, data fringe or list of data fringes.
#' @return dp
#' @export
#' @examples \dontrun{
#' fringe <- newDatafringeFromDatafringe(mtcars)
#' }
sampleData <- function(ftype,nrow = 20,asFringe=FALSE,...){
  #nrow <- 100
  #ftype <- "Ca-Ye-Nu-Da"
  ftypes <- strsplit(ftype,"-")[[1]]
  if(!all(ftypes %in% names(availableCtypes()))) stop("Wrong ftype")
  ncols <- length(ftypes)
  ca <- function(n){
    prefix <- sample(c("Cat","Type","X_","Form","Ilk"),1)
    sample(paste0(prefix,LETTERS[1:5]),n,replace = TRUE)
    }
  nu <- function(n,gt0 = NULL){
    gt0 <- gt0 %||% FALSE
    if(!gt0){
      v <- rnorm(n,1000,300)-400
      v[1] <- -10
      return(v)
    }
    round(rnorm(n,1000,300)*1)
    }
  da <- function(n,type = "seq"){
    # type = seq || random
    seqdates <- seq(as.Date('2000-01-01'),length.out=n, by="day")
    if(type == "seq") return(seqdates)
    sample(seq(as.Date('2000-01-01'),length.out=n/10, by="day"),n=n,replace = TRUE)
  }
  ye <- function(n, type = "seq"){
    seqyear <- seq(1900,length.out = n)
    if(type == "seq") return(seqyear)
    sample(seq(1900,length.out = n/10),n, replace = TRUE)
  }
  s <- list(Ca = ca, Nu = nu, Da = da, Ye = ye)
  sel <- s[ftypes]
  # args <- list(gt0 = TRUE)
  args <- list(...)
  makeFtypeParams <- function(ftype){
    if(ftype == "Nu")
      return(list(n = nrow,gt0 = args$gt0))
    list(n = nrow)
  }
  params <- map(ftypes,makeFtypeParams)
  d <- invoke_map(sel, params)
  names(d) <- letterNames(ncols)
  out <- as_data_frame(d)
  if(asFringe){out <- fringe(d)}
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


