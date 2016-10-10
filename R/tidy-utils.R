

#' @export
fct_recode_df <- function(d,col,codes){
  left_join(d[col],codes, by=col) %>% .$to
}

#' @export
fct_to_chr <- function(d){
  d %>% mutate_if(is.factor, as.character)
}
