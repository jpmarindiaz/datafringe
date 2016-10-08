#' @export Column
#' @exportClass Column
Column <- R6Class("Column",
                  public = list(
                    name = NA,
                    ctype = NA,
                    cformat = NA,
                    cdescription = NA,
                    initialize = function(name, ctype, cformat, cdescription = NULL) {
                      if (!missing(name)) self$name <- name
                      if (!missing(ctype)) self$ctype <- ctype
                      self$cformat <- cformat %||% getDefaultCformats(self$ctype)
                      self$cdescription <- cdescription %||% ""
                      self$validate()
                    },
                    validate = function(){
                      availableCtypeNames <- names(availableCtypes())
                      if(!self$ctype %in% availableCtypeNames)
                        stop("ctypes not in ",availableCtypeNames)
                      #if(self$ctype == "D")
                        #if(!self$cformat %in% availableCformats$D)
                        #  stop("cformat not in ",paste(availableCformats$D,collapse = ", "))
                    },
                    print = function(...) {
                      p <- paste0(
                        "<Column>",
                        "\nname: ", self$name,
                        "\nctype: ", self$ctype,
                        "\ncformat: ", self$cformat,
                        "\ncdescription: ", self$cdescription,
                        "\n"
                      )
                      cat(p)
                      invisible(self)
                    }
                  )
)


