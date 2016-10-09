#' @export Dic
#' @exportClass Dic
Dic <- R6Class("Dic",
               public = list(
                 d = NA,
                 meta = NA,
                 initialize = function(d,...) {
                   self$d <- as_tibble(d)
                   self$validate(d)
                   self$meta <- list(...)
                 },
                 validate = function(d){
                   attr(d, "spec") <- NULL
                   if(!all(c("id","ctype") %in% names(d) ))
                     stop("Need id and ctype for dictionary")
                   availableCtypeNames <- names(availableCtypes())
                   if(!all(self$d$ctype %in% availableCtypeNames))
                     stop("ctypes not in ",availableCtypeNames)
                   self$d$cformat <- d$cformat %||% NA
                   self$d$cdescription <- d$cdescription %||% NA

                   #if(self$d$ctypes == "D")
                   #if(!self$cformat %in% availableCformats$D)
                   #  stop("cformat not in ",paste(availableCformats$D,collapse = ", "))
                 },
                 print = function(...) {
                   nrow <- ifelse(nrow(self$d)>6,6,nrow(self$d))
                   ellipsis <- ""
                   prettyDic <- as.data.frame(self$d[1:nrow,])
                   if(nrow>6) ellipsis <- "..."
                   cat(
                     paste("<Dic>",
                           "  Dic:",
                           paste(capture.output(prettyDic),collapse="    \n"),
                           ellipsis,sep="\n"
                     ),
                     " Meta:",
                     paste(capture.output(str(self$meta)),collapse = "\n")
                   )
                   invisible(self)
                 }
               )
)


