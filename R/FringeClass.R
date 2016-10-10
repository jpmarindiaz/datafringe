#' @export Fringe
#' @exportClass Fringe
Fringe <- R6Class("Fringe",
                  # portable = FALSE,
                  #lock_objects = FALSE,
                  public = list(
                    name = NA,
                    description = NA,
                    recordName = NA,
                    ftype = NA,
                    dic_ = NA,
                    d = NA,
                    initialize = function(d,dic_ = NULL,
                                          name = NULL,
                                          description = NULL,
                                          recordName = NULL,
                                          ...
                    ) {
                      if(missing(d)) stop("Need a dataframe")
                      d <- as_tibble(d)
                      d <- remove_rownames(d)
                      attr(d, "spec") <- NULL
                      attr(dic_, "spec") <- NULL
                      if(!"tbl_df" %in% class(d)) stop("Need a tbl_df")
                      if(is.null(dic_)){
                        dic_ <- createDic(d, as_data_frame = FALSE)
                      }
                      else{
                        dic_ <- createDic(d, dic = dic_, as_data_frame = FALSE)
                      }

                      ctypes <- dic_$d$ctype %||% guessCtypes(d)
                      cformats <- dic_$d$cformats %||% guessCformats(d)
                      self$name <- name %||% ""
                      self$description <- description  %||% ""
                      self$recordName <- recordName %||% "observation"
                      self$dic_ <- dic_
                      self$ftype <- paste0(sort(self$getCtypes()),collapse = "-")
                      d <- removeRowAllNA(d)
                      #d <- naToEmpty(d)
                      d <- forceCtypes(d, ctypes)
                      names(d) <- letterNames(ncol(d))
                      self$d <- as_tibble(d)
                      self$validate()
                    },
                    validate = function(){
                      if(length(self$getCnames()) > length(unique(self$getCnames())) )
                        stop("cnames must be unique")
                    },
                    getCnames = function(){
                      self$dic_$d$id
                    },
                    getCtypes = function(){
                      self$dic_$d$ctype
                    },
                    getCformats = function(){
                      self$dic_$d$cformat
                    },
                    getCdescriptions = function(){
                      self$dic_$d$cdescription
                    },
                    setCnames = function(newNames, idx = NULL){
                      originalCnames <- self$dic_$d$id
                      if(length(newNames)!= length(originalCnames) && is.null(idx))
                        stop("cnames must be the same length as original")
                      if(is.null(idx)) self$dic_$d$id <- newNames
                      else self$dic_$d$id[idx] <- newNames
                      self$validate()
                    },
                    setCdescriptions = function(newDescriptions, idx = NULL){
                      originalCdescriptions <- self$dic_$d$cdescription
                      if(length(newDescriptions)!= length(originalCdescriptions) && is.null(idx))
                        stop("cdescriptions must be the same length as original")
                      if(is.null(idx)) self$dic_$d$cdescription <- newDescriptions
                      else self$dic_$d$cdescription[idx] <- newDescriptions
                      self$validate()
                    },
                    writeCSV = function(path, type = "all"){
                      path <- path %||% "."
                      name <- self$name %||% "fringe"
                      file <- file.path(path,paste0(name,"-data.csv"))
                      write_csv(self$data,file)
                      file <- file.path(path,paste0(name,"-dic_.csv"))
                      write_csv(self$dic_$d,file)
                      file.path(path,name)
                    },
                    asList = function(){
                      list(
                        name = self$name,
                        description = self$description,
                        cnames = self$getCnames(),
                        ctypes = self$getCtypes(),
                        cformats = self$getCformats(),
                        cdescriptions = self$getCdescriptions(),
                        ftype = self$ftype,
                        nrows = nrow(self$d),
                        ncols = ncol(self$d),
                        data = self$data
                      )
                    },
                    writeYAML = function(file = NULL, path = "."){
                      file <- file_path_sans_ext(file) %||% self$name
                      file <- file.path(path,paste0(file,".yaml"))
                      l <- self$asList()
                      l$data <- paste0(file,".csv")
                      yaml <- as.yaml(l)
                      writeLines(yaml,file)
                      file
                    },
                    print = function(...) {
                      p <- paste0(
                        "<Fringe>",
                        "\nname: ", self$name,
                        "\ndescription: ", self$description,
                        "\nftype: ", self$ftype,
                        "\nnrows: ", nrow(self$data),
                        "\ndic:\n ", paste(capture.output(self$dic_$d),collapse="\n"),
                        "\ndata:\n ", paste(capture.output(self$data),collapse="\n")
                      )
                      cat(p)
                      invisible(self)
                    }
                  ),
                  active = list(
                    data = function(){
                      d <- self$d
                      names(d) <- self$getCnames()
                      d
                    }
                  )
)

