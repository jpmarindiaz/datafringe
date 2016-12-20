#' Creates a new Datafringe from a dataframe
#' @name sampleData
#' @description Creates a new datapackage from json, data fringe, list of data fringes, list of data fringees (see Datafringe reference class)
#' @param d might be a json string, data fringe or list of data fringes.
#' @return dp
#' @export
#' @examples \dontrun{
#' fringe <- newDatafringeFromDatafringe(mtcars)
#' }
sampleData <- function(ftype,nrow = 20,asFringe=FALSE, loremNames = TRUE,addNA = TRUE, rep = FALSE,...){
  #nrow <- 100
  #ftype <- "Ca-Ye-Nu-Da"
  #ftype <- "Ca-Ye-NuP"
  ftypes <- strsplit(ftype,"-")[[1]]
  ftypes <- flatten_chr(purrr::map(ftypes, function(ftype){
    if(grepl("P",ftype)){
      return(rep(gsub("P","",ftype),sample(1:6,1)))
    }
    ftype
  }))
  if(!all(ftypes %in% names(availableCtypes()))) stop("Wrong ftype")
  ncols <- length(ftypes)
  ca <- function(n, nlevels = 5, addNA = TRUE,...){
    prefix <- sample(c("Cat","Type","X_","Form","Ilk"),1)
    v <- sample(paste0(prefix,LETTERS[1:nlevels]),n,replace = TRUE)
    if(addNA) v[sample(n,round(n/10))] <- NA
    v
  }
  nu <- function(n,gt0 = NULL, addNA = TRUE,...){
    gt0 <- gt0 %||% FALSE
    v <- round(rnorm(n,1000,300)*1)
    if(!gt0) v[sample(n,1)] <- -10
    if(addNA) v[sample(n,round(n/10))] <- NA
    v
  }
  da <- function(n,rep = FALSE, addNA = TRUE,...){
    # type = seq || random
    if(!rep){
      v <- seq(as.Date('2000-01-01'),length.out=n, by="day")
    }else{
      v <- sample(seq(as.Date('2000-01-01'),length.out=n/10, by="day"),n=n,replace = TRUE)
    }
    if(addNA) v[sample(n,round(n/10))] <- NA
    v
  }
  ye <- function(n, rep = FALSE, addNA = TRUE,...){
    if(!rep) {
      v <- seq(1900,length.out = n)
    }else{
      v <- sample(seq(1900,length.out = n/10),n, replace = TRUE)
    }
    if(addNA) v[sample(n,round(n/10))] <- NA
    v
  }
  s <- list(Ca = ca, Nu = nu, Da = da, Ye = ye)
  sel <- s[ftypes]
  # args <- list(gt0 = TRUE)
  args <- list(...)
  makeFtypeParams <- function(ftype){
    if(ftype == "Nu")
      return(list(n = nrow,gt0 = args$gt0,addNA = addNA))
    list(n = nrow, addNA = addNA, rep = rep)
  }
  params <- purrr::map(ftypes,makeFtypeParams)
  d <- invoke_map(sel, params)
  if(!loremNames){
    names(d) <- letterNames(ncols)
  }else{
    names(d) <- loremNames(ncols)
  }
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


