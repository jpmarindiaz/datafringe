

createDic <- function(d, dic = NULL, as_data_frame = TRUE,...){
  if(!is.null(dic)){
    if(!all(dic$id %in% names(d) ))
      stop("Vars in diccionary not in data")
    dic$ctypes <- dic$ctypes %||% guessCtypes(d)
  }else{
    dic <- data_frame(id=names(d), ctypes = guessCtypes(d))
  }
  dic_ <- Dic$new(dic,...)
  if(as_data_frame) return(dic_$d)
  dic_
}

