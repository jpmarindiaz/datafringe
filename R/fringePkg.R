
#' @export
readFringeSqlite <- function(name,db, excludeCols = NULL){
  #name <- "objetivos_bogota"
  data <- tbl(db,paste0(name,"_data")) %>% collect()
  dic <- tbl(db,paste0(name,"_dic_")) %>% collect()
  if(!is.null(excludeCols)){
    keepCols <- names(data)[!names(data) %in% excludeCols]
    data <- data[keepCols]
    dic <- dic %>% filter(!id %in% excludeCols)
  }
  fringe(data,dic = dic,name = name)
}

#' @export
list_fringes_sqlite <- function(path,groups = NULL, fringe_idx = NULL){
  fringe_idx <- fringe_idx %||% "fringe_idx"
  db <- src_sqlite(path)
  x <- src_tbls(db)
  x <- x[grepl("_data",x)]
  x <- gsub("_data","",x)
  fringe_idx <- tbl(db,fringe_idx) %>% collect()
  fringe_idx <- filter(fringe_idx, id %in% x)
  if(!is.null(groups)){
    fringe_idx <- fringe_idx %>% filter(group %in% groups)
  }
  fringe_idx
}

#' @export
read_fringe_idx_sqlite <- function(path,fringe_idx = NULL){
  fringe_idx <- fringe_idx %||% "fringe_idx"
  db <- src_sqlite(path)
  tbl(db,fringe_idx) %>% collect()
}


#' @export
list_fringes <- function(path, groups = NULL, fringe_idx = NULL){
  fringe_idx <- fringe_idx %||% "fringe_idx.csv"
  fidxpath <- file.path(path,"fringe_idx.csv")
  fidx <- read_csv(fidxpath)
  fidx <- fidx %>% filter(!exclude)
  dbs <- fidx  %>% filter(!is.na(id))
  groups <- groups %||% unique(dbs$group)
  if(!is.null(groups)){
    dbs <- dbs %>% filter(group %in% groups)
  }
  fs <- list.files(path,recursive = TRUE)
  dbFiles <- dbs %>% select(id,withDic) %>%
    mutate(data = paste0(id,"_data.csv"), dic = paste0(id,"_dic_.csv"))
  dbFilesWithDic <- dbFiles %>% filter(withDic) %>%
    select(data,dic) %>% flatten_chr
  if(!all(dbFilesWithDic %in% fs))
    stop("db: data and dic not in folder :",
         paste(dbFilesWithDic[!dbFilesWithDic %in% fs],collapse="\n"))
  #dbs %>% separate(id,c("type","name"),extra = "merge")
  dbs
}

#' @export
load_fringes <- function(path, groups = NULL, n_max = Inf, fringe_idx = NULL){
  frs <- list_fringes(path, fringe_idx = fringe_idx)
  groups <- groups %||% unique(frs$group)
  frs <- list_fringes(path, groups = groups)
  paths <- file.path(path,frs$id)
  names(paths) <- frs$id
  #f <- readFringe(paths[5],name="hola")
  fpkg <- map2(paths,frs$withDic, ~ readFringe(.x, forceDic = .y,verbose = TRUE, n_max = n_max))
  fpkg
}

#' @export
write_fpkg_sqlite <- function(fringes_path, sqlite_path, fringe_idx = NULL){
  fringe_idx <- fringe_idx %||% "fringe_idx"
  frs <- load_fringes(fringes_path)
  db <- src_sqlite(sqlite_path, create = T)
  # fr <- fpkg[[1]]
  # db_drop_table(db$con,table='objetivos_comparada_data')
  copyFringeToSQlite <- function(fr){
    name <- gsub("-","_",fr$name)
    message("copying: ",name)
    copy_to(db,fr$data, name = paste0(name,"_data"), temporary=FALSE)
    copy_to(db,fr$dic_$d, name = paste0(name,"_dic_"), temporary=FALSE)
    NULL
  }
  purrr::map(frs, copyFringeToSQlite)
  if(!is.null(fringe_idx)){
    fridx <- read_csv(fringe_idx)
    copy_to(db,fridx, name = "fringe_idx", temporary=FALSE)
  }
  sqlite_path
}


