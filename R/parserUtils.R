
isJSONVector <- function(str){
  a <- tryCatch(jsonlite::fromJSON(str),
                error = function(e) NULL)
  if(is.null(a)) return(FALSE)
  is.vector(a)
}

getArgsFromFunctionString <- function(str){
  #str <-  "select(1,2)"
  #str <-  "select(a,b)"
  #str <- "cdsafas"
  #str <- 'select("speed")'
  #str <- "select()"
  s <- regmatches(str, gregexpr("(?<=\\().*?(?=\\))", str, perl=T))[[1]]
  s <- gsub('"','',s)
  s <- gsub("'",'',s)
  if(length(s)==0) return(NULL)
  if(s=="") return(NULL)
  s <- strsplit(s,",")[[1]]
  out <- tryCatch(as.numeric(s),warning = function(w) NULL)
  if(!is.null(out)){ return(as.list(out))}
  out <- s
  as.list(out)
}

#getArgsFromFunctionString('select("speed")')
#getArgsFromFunctionString("select(1,2)")
#getArgsFromFunctionString( "select(a,b)")
#getArgsFromFunctionString("cdsafas")
#getArgsFromFunctionString("select()")

try2 <- function(code, silent = FALSE) {
  tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent) gsub( "Error in try({ : ","",message(c), fixed="TRUE")
    invisible(structure(msg, class = "try-error"))
  })
}
