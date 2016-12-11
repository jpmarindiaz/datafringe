
#' @export
keep_not_na_FringeRows <- function(fringeIn){
  if(!class(fringeIn)[1] %in% c("Fringe","data.frame"))
    stop("fringe must be either a Fringe of a data.frame")
  if(!isFringe(fringeIn)) fringe <- fringe(fringeIn)
  else fringe <- fringeIn
  data <- fringe$data %>% discard_any_na_rows()
  dic <- fringe$dic_$d
  fringe(data,dic)
}

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
  if(is.null(fringeIn)) return()
  if(!class(fringeIn)[1] %in% c("Fringe","data.frame"))
    stop("fringe must be either a Fringe of a data.frame")
  if(!isFringe(fringeIn)) fringe <- fringe(fringeIn)
  else fringe <- fringeIn
  if(class(cols) %in% c("numeric","integer"))
    cols <- getCnames(fringe)[cols]
  if(! all(cols %in% getCnames(fringe)))
    stop("cols: ",cols[!cols %in% getCnames(fringe)]," not in fringe ",fringe$name)
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
selectDicCtypes <- function(f,ctypes, as_list = FALSE, filter = NULL){
  out <- f$dic_$d %>% filter(ctype %in% ctypes)
  if(!is.null(filter)){
    if(!filter %in% names(out)) stop("Filter not in diccionary")
    filter_criteria <- interp(~ filter == TRUE, filter = as.name(filter))
    out <- out %>% filter_(filter_criteria)
  }
  if(as_list){
    # setNames(transpose(out),out$id) # in case we want the full dic as list
    out_list <- as.list(setNames(out$id, out$label))
    return(out_list)
  }
  out
}


#' @export
joinFringes <- function(f1,f2,prefix1 = NULL, prefix2 = NULL, type = "full",...){
  if(!type %in% c("full","inner","left","right","semi","anti"))
    stop("join type not known")
  prefix1 <- prefix1 %||% substring(f1$name,1,3)
  prefix2 <- prefix2 %||% substring(f2$name,1,3)
  if(type == "full"){
    d <- full_join(f1$data,f2$data,...)
  }
  if(type == "inner"){
    d <- inner_join(f1$data,f2$data,...)
  }
  if(type == "left"){
    d <- left_join(f1$data,f2$data,...)
  }
  if(type == "right"){
    d <- right_join(f1$data,f2$data,...)
  }
  if(type == "semi"){
    d <- semi_join(f1$data,f2$data,...)
  }
  if(type == "anti"){
    d <- anti_join(f1$data,f2$data,...)
  }
  joinBy <- intersect(getCnames(f1),getCnames(f2))
  f1Names <- setdiff(getCnames(f1),getCnames(f2))
  f2Names <- setdiff(getCnames(f2),getCnames(f1))
  dic1 <- f1$dic_$d
  idx <- dic1$id %in% f1Names
  #dic1$id[idx] <- paste0(prefix1,"_",dic1$id[idx])
  dic1$label[idx] <- paste0(prefix1,": ",dic1$label[idx])
  dic2 <- f2$dic_$d
  idx <- dic2$id %in% f2Names
  #dic2$id[idx] <- paste0(prefix2,"_",dic2$id[idx])
  dic2$label[idx] <- paste0(prefix2,": ",dic2$label[idx])

  dic <- bind_rows(dic1,dic2) %>% distinct(id,.keep_all = TRUE)
  fringe(d, dic = dic)
}


