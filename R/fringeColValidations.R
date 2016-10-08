#' @export
fringeColValidateFuns <- function(){
  colVal_funs <- as.character(lsf.str("package:fringer"))
  colVal_funs <- colVal_funs[grepl("^fringeColVal_",colVal_funs)]
  colVal_funs <- gsub("fringeColVal_","",colVal_funs, fixed = TRUE)
  colVal_funs
}

#' fringeColVal_unique
#' @name fringeColVal_unique
#' @description fringeColVal_unique
#' @export
fringeColVal_unique <- function(fringe,cols){
  data <- getDatafringe(fringe)
  all(!duplicated(data[cols]))
}

#' fringeColVal_greaterThan0
#' @name fringeColVal_greaterThan0
#' @description fringeColVal_greaterThan0
#' WORKS_WITH_CTYPE: N
#' @export
fringeColVal_greaterThan0 <- function(fringe,cols){
  data <- getDatafringe(fringe)
  data <- data[cols]
  all(sapply(data,function(i) i>0))
}

#' fringeColVal_hasGenderLevelsEs
#' @name fringeColVal_hasGenderLevelsEs
#' @description fringeColVal_hasGenderLevelsEs
#' @export
fringeColVal_hasGenderLevelsEs <- function(fringe,cols){
  data <- getDatafringe(fringe)
  f <- function(i){i %in% c("Masculino","Femenino","")}
  all(sapply(data,f))
}

#' fringeColVal_different
#' @name fringeColVal_different
#' @description fringeColVal_different
#' @export
fringeColVal_different <- function(fringe,cols){
  data <- getDatafringe(fringe)
  l <- lapply(cols,function(c){
    length(unique(data[,c])) == length(data[,c])
  })
  all(unlist(l))
}

#' fringeColVal_hasCtype
#' @name fringeColVal_hasCtype
#' @description fringeColVal_hasCtype
#' @export
fringeColVal_hasCtype <- function(fringe,cols,ctype){
  ctypes <- getCtypes(fringe)
  idx <- match(cols,getCnames(fringe))
  ctypes <- ctypes[idx]
  fringe <- selectFringeCols(fringe,cols)
  all(getCtypes(fringe) %in% ctype)
}



#' @export
fringeColValidate <- function(t, cols = NULL, validation, ...){
  availableValidations <- fringeColValidateFuns()
  if(!validation %in% availableValidations)
    stop("no validation with that name")
  if(!isFringe(t)) stop("must be a fringe")
  cols <- cols %||% getCnames(t)
  if(class(cols) %in% c("numeric","integer"))
    cols <- getCnames(t)[cols]
  if(!all(cols %in% getCnames(t)))
    stop('cols not in fringe')
  args <- list(...)
  fun <- paste0("fringeColVal_",validation)
  p <- c(list(t,cols),args)
  do.call(fun,p)
}


