

getColumnNames <- function(fringe){
  if(!isFringe(fringe)) stop('class is not Fringe')
  unlist(purrr::map(fringe$fields,function(i){i$name}))
}


#' @export
sample2 <- function(v, n,replace = TRUE){
  if(length(v)==1) return(v)
  sample(v,n,replace = replace)
}

#' @export
discard_all_na_rows <- function(d){
  d %>% filter(apply(., 1, function(x) !all(is.na(x))))
}

#' @export
discard_any_na_rows <- function(d){
  d %>% filter(apply(., 1, function(x) !any(is.na(x))))
}


#' @export
discard_all_na_cols <- function(d){
  f <- function(x)!all(is.na(x))
  d %>% keep(f)
}


#' @export
getDictionary <- function(d){
  f$dic_$d
}


