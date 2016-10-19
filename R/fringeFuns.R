

#' @export
keepFringeRows <- function(fringeIn,col,values){
  if(!class(fringeIn)[1] %in% c("Fringe","data.frame"))
    stop("fringe must be either a Fringe of a data.frame")
  if(!isFringe(fringeIn)) fringe <- fringe(fringeIn)
  else fringe <- fringeIn
  if(class(col) %in% c("numeric","integer"))
    cols <- getCnames(fringe)[col]
  if(!all(col %in% getCnames(fringe)))
    stop("col not in fringe")
  colPos <- match(col,getCnames(fringe))
  colLetter <- letterNames(colPos)[colPos]
  filter_criteria <- interp(~ col %in% values, col = as.name(getCnames(fringe)[colPos]))
  data <- fringe$data %>% filter_(filter_criteria)
  dic <- fringe$dic_$d
  fringe(data,dic)
}


#' @export
selectFringeCols <- function(fringeIn,cols){
  if(!class(fringeIn)[1] %in% c("Fringe","data.frame"))
    stop("fringe must be either a Fringe of a data.frame")
  if(!isFringe(fringeIn)) fringe <- fringe(fringeIn)
  else fringe <- fringeIn
  if(class(cols) %in% c("numeric","integer"))
    cols <- getCnames(fringe)[cols]
  if(! all(cols %in% getCnames(fringe)))
    stop("cols not in fringe")
  d <- getDatafringe(fringe)
  dic <- fringe$dic_$d %>% filter(id %in% cols) %>%
    slice(match(cols, id)) # added to rearrange dictionary given cols
  out <- d[cols]
  if(isFringe(fringeIn)) return(fringe(out,dic))
  out
}

#' @export
selectFringeCtypes <- function(f,ctypes){
  dic <- selectDicCtypes(f,ctypes)
  data <- f$data %>%  select_(.dots = dic$id)
  fringe(data,dic)
}

#' @export
selectDicCtypes <- function(f,ctypes, as_list = FALSE){
  out <- f$dic_$d %>% filter(ctype %in% ctypes)
  if(as_list){
    # setNames(transpose(out),out$id) # in case we want the full dic as list
    out_list <- as.list(setNames(out$id, out$label))
    return(out_list)
  }
  out
}
