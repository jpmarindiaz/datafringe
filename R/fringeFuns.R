

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
    out_list <- as.list(setNames(out$id, out$name))
    return(out_list)
  }
  out
}
