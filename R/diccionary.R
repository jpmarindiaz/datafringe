

createDic <- function(d, dic = NULL, as_data_frame = TRUE,...){
  if(!is.null(dic)){
    if(!all( names(d) %in% dic$id))
      stop("Vars in data not in diccionary")
    dic$ctype <- dic$ctype %||% guessCtypes(d)
  }else{
    dic <- data_frame(id=names(d), ctype = guessCtypes(d))
  }
  dic <- dic %>% filter(id %in% names(d))
  dic_ <- Dic$new(dic,...)
  if(as_data_frame) return(dic_$d)
  dic_
}

