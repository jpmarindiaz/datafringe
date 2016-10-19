#' #' @export Fringe
#' #' @exportClass Fringe
#' Fringe <- R6Class("Fringe",
#'                   public = list(
#'                     name = NA,
#'                     description = NA,
#'                     recordName = NA,
#'                     ftype = NA,
#'                     dic = NA,
#'                     d = NA,
#'                     initialize = function(d,
#'                                           ctypes = NULL,
#'                                           cformats = NULL,
#'                                           cdescriptions = NULL,
#'                                           name = NULL,
#'                                           description = NULL,
#'                                           recordName = NULL
#'                     ) {
#'                       if(missing(d)) stop("Need a dataframe")
#'                       rownames(d) <-  NULL
#'                       ctypes <- ctypes %||% guessCtypes(d)
#'                       cformats <- cformats %||% guessCformats(d)
#'                       cdescriptions <- cdescriptions %||% character(ncol(d))
#'                       self$name <- name %||% trim_punct(deparse(substitute(d)))
#'                       self$description <- description  %||% ""
#'                       self$recordName <- recordName %||% "observation"
#'                       fieldNames <- names(d)
#'                       fieldList <- lapply(seq_along(fieldNames),
#'                                           function(i,ctypes,cformats, cdescriptions){
#'                                             #message(ctypes[i], "_",cformats[i],"_")
#'                                             Column$new(name=fieldNames[i],
#'                                                        ctype=ctypes[i],
#'                                                        cformat=cformats[i],
#'                                                        cdescription=cdescriptions[i])
#'                                           },
#'                                           ctypes = ctypes, cformats = cformats,
#'                                           cdescriptions = cdescriptions)
#'                       self$fields <- fieldList
#'                       self$ftype <- paste0(sort(self$getCtypes()),collapse = "-")
#'                       d <- discard_all_na(d)
#'                       d <- naToEmpty(d)
#'                       fd <- forceCtypes(as.data.frame(d), ctypes)
#'                       names(fd) <- letters[1:ncol(fd)]
#'                       self$d <- fd
#'                       self$validate()
#'                     },
#'                     validate = function(){
#'                       if(length(self$getCnames()) > length(unique(self$getCnames())) )
#'                         stop("cnames must be unique")
#'                     },
#'                     getCnames = function(){
#'                       unlist(Map(function(i){i$name},self$fields))
#'                     },
#'                     getCtypes = function(){
#'                       unlist(Map(function(i){i$ctype},self$fields))
#'                     },
#'                     getCformats = function(){
#'                       unlist(Map(function(i){i$cformat},self$fields))
#'                     },
#'                     getCdescriptions = function(){
#'                       unlist(Map(function(i){i$cdescription},self$fields))
#'                     },
#'                     setCnames = function(cnames, idx = NULL){
#'                       originalCnames <- self$getCnames()
#'                       if(length(cnames)!= length(originalCnames) && is.null(idx))
#'                         stop("cnames must be the same length as original")
#'                       idx <- idx %||% seq_along(originalCnames)
#'                       j <- 1
#'                       originalCnames[idx] <- cnames
#'                       newNames <- originalCnames
#'                       lapply(seq_along(newNames), function(i){
#'                         self$fields[[i]]$name <- newNames[i]
#'                       })
#'                       names(self$d) <- self$getCnames()
#'                       self$validate()
#'                     },
#'                     setCdescriptions = function(cdescriptions, idx = NULL){
#'                       originalCdescriptions <- self$getCdescriptions()
#'                       if(length(cdescriptions)!= length(originalCdescriptions) && is.null(idx))
#'                         stop("descriptions must be the same length as original")
#'                       idx <- idx %||% seq_along(originalCdescriptions)
#'                       j <- 1
#'                       originalCdescriptions[idx] <- cdescriptions
#'                       newNames <- originalCdescriptions
#'                       lapply(seq_along(newNames), function(i){
#'                         self$fields[[i]]$cdescription <- newNames[i]
#'                       })
#'                       names(self$d) <- self$getCdescriptions()
#'                       self$validate()
#'                     },
#'                     writeCSV = function(file = NULL, path = "."){
#'                       file <- file_path_sans_ext(file) %||% self$name
#'                       file <- file.path(path,paste0(file,".csv"))
#'                       write.csv(self$data,file,row.names = FALSE)
#'                       file
#'                     },
#'                     asList = function(){
#'                       list(
#'                         name = self$name,
#'                         description = self$description,
#'                         cnames = self$getCnames(),
#'                         ctypes = self$getCtypes(),
#'                         cformats = self$getCformats(),
#'                         cdescriptions = self$getCdescriptions(),
#'                         ftype = self$ftype,
#'                         nrows = nrow(self$d),
#'                         ncols = ncol(self$d),
#'                         data = self$data
#'                       )
#'                     },
#'                     writeYAML = function(file = NULL, path = "."){
#'                       file <- file_path_sans_ext(file) %||% self$name
#'                       file <- file.path(path,paste0(file,".yaml"))
#'                       l <- self$asList()
#'                       l$data <- paste0(file,".csv")
#'                       yaml <- as.yaml(l)
#'                       writeLines(yaml,file)
#'                       file
#'                     },
#'                     print = function(...) {
#'                       p <- paste0(
#'                         "<Fringe>",
#'                         "\nname: ", self$name,
#'                         "\ndescription: ", self$description,
#'                         "\ncnames: ", paste(paste0('"',self$getCnames(),'"'),collapse=", "),
#'                         "\nctypes: ", paste(paste0('"',self$getCtypes(),'"'),collapse=", "),
#'                         "\ncformats: ", paste(paste0('"',self$getCformats(),'"'),collapse=", "),
#'                         "\ncdescriptions: ", paste(paste0('"',self$getCdescriptions(),'"'),collapse=", "),
#'                         "\nftype: ", self$ftype,
#'                         "\nnrows: ", nrow(self$data),
#'                         "\ndata:\n ", paste(capture.output(head(self$data,4)),collapse="\n")
#'                       )
#'                       cat(p)
#'                       invisible(self)
#'                     }
#'                   ),
#'                   active = list(
#'                     data = function(){
#'                       d <- self$d
#'                       names(d) <- self$getCnames()
#'                       row.names(d) <- NULL
#'                       d
#'                     }
#'                   )
#' )
#'
