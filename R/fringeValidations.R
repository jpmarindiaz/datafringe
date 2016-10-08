

#' @export
parseValidator <- function(str){
  str
}



#' @export
fringeValidateFuns <- function(){
  fringeVal_funs <- as.character(lsf.str("package:fringer"))
  fringeVal_funs <- fringeVal_funs[grepl("^fringeVal_",fringeVal_funs)]
  gsub("fringeVal_","",fringeVal_funs, fixed = TRUE)
}


#' fringeVal_hasFtype
#' @name fringeVal_hasFtype
#' @description fringeVal_hasFtype
#' @export
fringeVal_hasFtype <- function(fringe,ftype){
  if(missing(ftype)) stop("need ftype as a parameter")
  identical(ftype,getFtype(fringe))
}

#' fringeVal_hasAnyFtype
#' @name fringeVal_hasAnyFtype
#' @description fringeVal_hasAnyFtype
#' @export
fringeVal_hasAnyFtype <- function(fringe,ftype){
  if(missing(ftype)) stop("need ftype as a parameter")
  any(getFtype(fringe) %in% ftype)
}

#' fringeVal_hasCtypes
#' @name fringeVal_hasCtypes
#' @description fringeVal_hasCtypes
#' @export
fringeVal_hasCtypes <- function(fringe,ctypes){
  if(missing(ctypes)) stop("need ctypes as a parameter")
  identical(getCtypes(fringe),ctypes)
}

#' fringeVal_allNumeric
#' @name fringeVal_allNumeric
#' @description fringeVal_allNumeric
#' @export
fringeVal_allNumeric <- function(fringe){
  identical(unique(getCtypes(fringe)),"Nu")
}


#' fringeVal_hasColnames
#' @name fringeVal_hasColnames
#' @description fringeVal_hasColnames
#' @export
fringeVal_hasColnames <- function(fringe,cnames){
  if(missing(cnames)) stop("need cnames as a parameter")
  identical(getCnames(fringe),cnames)
}

#' fringeVal_colnamesInFringe
#' @name fringeVal_colnamesInFringe
#' @description fringeVal_colnamesInFringe
#' @export
fringeVal_colnamesInFringe <- function(fringe,cols){
  if(missing(cols)) stop("need cnames as a parameter")
  cols %in% getCnames(fringe)
}


#' @export
fringeValidate <- function(t, validation, ...){
  if(!isFringe(t)) stop("must be a fringe")
  args <- list(...)
  availableValidations <- fringeValidateFuns()
  if(!validation %in% availableValidations)
    stop("no validation with that name")
  fun <- paste0("fringeVal_",validation)
  do.call(fun,c(t,list(...)))
}








