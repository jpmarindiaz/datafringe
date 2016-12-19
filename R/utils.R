
loremNames <- function(ncol){
  sample2(read_lines(sysfile("aux/lorem.txt")),ncol)
}


trim_punct <- function (x){
  gsub("[[:punct:]]", "", x)
}

dfFactorsToCharacters <- function(d){
  i <- sapply(d, is.factor)
  d[i] <- lapply(d[i], as.character)
  d
}


colInDf <- function(col,d){
  !col %in% colnames(d)
}

has_warning <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  length(warn) > 0
}


`%||%` <- function (x, y){
  suppressWarnings({
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if( class(x)=="character" && all(nchar(x)==0))
    return(y)
  else x
  })
}

is.empty <- function(x){
  #   !is.null(x)
  !as.logical(length(x))
}


naToEmpty <- function(df, empty = c(" ")){
  df[is.na(df)] <- ""
  df[df %in% empty] <- ""
  df
}

#' @export
file_path_sans_ext <- function (x)
{
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}


#' Read contents of a system file into a character string
#' @name sysfile
#' @description sysfile
#' @param string string
#' @return string
#' @export
#' @examples \dontrun{
#' }
sysfile <- function(..., package = "datafringe"){
  if (is.null(package)){
    path = file.path(...)
  } else {
    path = system.file(..., package = package)
  }
  path
}
