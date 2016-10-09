

#' @export
#'
selectFringeCtypes <- function(f,ctypes){
  dic <- f$dic_$d %>% filter(ctype %in% ctypes)
  data <- f$data %>%  select_(.dots = dic$id)
  fringe(data,dic)
}

