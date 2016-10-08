


# v <- c("2014-03-01","2043-04-04","2014-04-04")
# isDate(v)
# isDatetime(v)
# isTime(v)
# whichDTH(v)
# v <- c("2014-03-01","2043-04-04","20140404")
# parseDatetime(v, "D")
# v <- c("2014-03-01 5:04:00","2043-04-04 5:04:00","2014-04-04 5:04:00")
# parseDatetime(v, "T")
# v <- c("04:00","13:05:00","5:04:00")
# parseDatetime(v, "H")


parseDatetime <- function(v, datetimeType){
  if(datetimeType == "D"){
    format <- guess_formats(v, "ymd")
    outVals <- as.Date(v, format = format)
  }
  if(datetimeType == "H"){
    format <- guess_formats(v, "HMS")
    v <- as.POSIXct(v, format = format)
    outVals <- format(v, format="%H:%M:%S")
  }
  if(datetimeType == "T"){
    format <- guess_formats(v, "ymd HMS")
    outVals <- as.POSIXct(v, format = format)
  }
  outVals
}


guessDateFormat <- function(v) parseDateTime(v)$format





whichDTH <-function(x){
  d <- c(isDate(x),isDatetime(x), isTime(x))
  if(!any(d)) return(NULL)
  #Reduce(`||`,d)
  dth <- c("Da","Dt","Ho")
  dth[d]
}

isD_format <- function(format, expectedFormat){
  function(v){
    guess = guess_formats(v,format)
    if(length((guess))< length(v))
      #stop("Dates do not seem to have the same format")
      return(FALSE)
    if(unique(guess) != expectedFormat)
      return(FALSE)
    !is.null(guess)
  }
}

isDate <- isD_format("ymd","%Y-%m-%d")
isTime <- isD_format("HMS","%H:%M:%S")
isDatetime <- isD_format("ymd HMS","%Y-%m-%d %H:%M:%S")

# v <- c("2015-04-03"," fdsaf")
# format <- "ymd"
# isDate(v)

