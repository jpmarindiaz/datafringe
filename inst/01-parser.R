library(devtools)
load_all()


library(fringer)
library(jsonlite)
library(sourcetools)

# http://kevinushey.github.io/blog/2016/02/12/top-down-operator-precedence-parsing-with-r/
#http://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing

tokens <- c()
index <- 0

tokenize <- function(program) {
  tokens <<- unlist(strsplit(program, "\\s+"))
  index <<- 1
  tokens
}
current <- function() {
  tokens[index]
}
consume <- function() {
  token <- current()
  index <<- index + 1
  token
}


binaryPrecedence <- function(token) {
  #switch(token, "+" = 10, "*" = 20, 0)
  if(token %in% c("==","equals","in","notIn",">","<","<=",">="))
    return(10)
  if(token %in% c("select","allUnique","uniques","nrow","ncol","cnames","ctypes","ftype"))
    return(20)
  if(token %in% c("%>%","pipe"))
    return(15)
  return(0)
}

parseExpressionStart <- function() {
  #force(d)
  token <- consume()
  message("Token Start: ",token)
  # dOperators <- list(
  #   "select" = function(x,...) dplyr::select_(x,...),
  #   "allUnique" = function(x) nrow(x) == nrow(unique(x)),
  #   "uniques" = function(x) nrow(unique(x)),
  #   "nrow" = function(x) nrow(x),
  #   "ncol" = function(x) ncol(x),
  #   "cnames" = function(x) getCnames(x),
  #   "ctypes" = function(x) guessCtypes(x),
  #   "ftype" = function(x) getFtype(x)
  # )
  dOperators <- list(
    "select" = "select",
    "allUnique" = "allUnique",
    "uniques" = "uniques",
    "nrow" = "nrow",
    "ncol" = "ncol",
    "cnames" = "cnames",
    "ctypes" = "ctypes",
    "ftype" = "ftype"
  )

  if(token %in% names(dOperators)){
    #f <- dOperators[[tokenFunName]]
    #fArgs <- c(list(),tokenArgs)
    return(deparse(sys.calls()[[sys.nframe()-1]]))
  }
  lOperators <- c("in","equals")
  if(token %in% lOperators)
    return(NULL)
  if(isJSONVector(token))
    return(fromJSON(token))
  num <- tryCatch(as.numeric(token), warning = function(e) NULL)
  if(!is.null(num))
    return(num)
  return(token)
}

parseExpressionContinuation <- function(node) {
  token <- consume()
  message("TOKEN PARSE EXPRESSION CONT: ",token)
  call(token,node,
       parseExpression(
         precedence = binaryPrecedence(token))
  )
}

parseExpression <- function(precedence = 0) {
  #str(d)
  node <- parseExpressionStart()
  str(node)
  message("IDX: ",index,"  NODE: ",node)
  while (precedence < binaryPrecedence(current()))
    node <- parseExpressionContinuation(node)
  node
}

# Our entry-point for parsing programs.
parse <- function(program) {
  tokens <<- tokenize(program)
  index <<- 1
  parseExpression()
}

`in` <- function (x, table) all(match(x, table, nomatch = 0L) > 0L)
`equals` <- function(x,y) all(x == y)
`pipe` <- function(d,f) f(d)

  "select" = function(x,...) dplyr::select_(x,...)
  "allUnique" = function(x) nrow(x) == nrow(unique(x))
  "uniques" = function(x) nrow(unique(x))
  "nrow" = function(x) nrow(x)
  "ncol" = function(x) ncol(x)
  "cnames" = function(x) getCnames(x)
  "ctypes" = function(x) guessCtypes(x)
  "ftype" = function(x) getFtype(x)



parse("cars pipe nrow")
eval(parse("cars pipe nrow"))


parse("1 in [1,2]")
parse("1 pipe [1,2]")
parse("cars pipe select(1)")

parse("nrow(cars)")
eval(parse("nrow(cars)"))

parse("cars pipe nrow")

parse("cars %>% nrow")
eval(parse("cars %>% nrow"))




#d <- cars
program <- 'cnames equals ["speed","dist"]'
parse(program)
eval(parse(program))
program <- 'select(1)'
parse(program,d)
program <- 'select("speed")'
parse(program,d)

program <- 'select("speed") %>% ncol'
parse("cars %>% select(1)")
parse(program,d)
eval(parse(program,d))




all(letters == letters)

eval(parse("uniques",d))
eval(parse("allUnique",d))
eval(parse("uniques > 1",d))
parse("uniques > 1",d)
parse("uniques",d)
parse("cnames",d)
parse('cnames in ["speed","dist"]',d)
eval(parse('cnames in ["speed","dist"]',d))
parse('cnames equals ["spesed","dist"]',d)
eval(parse('cnames equals ["speed","dist"]',d))
parse("Ca equals ftype",d)
eval(parse("Ca equals ftype",d))
eval(parse("Nu-Nu equals ftype",d))
eval(parse("Ca in ctypes",iris))
parse("Ca",d)

parse("ctypes",d)






vals <- list(
  nrows = "nrow > 1",
  uniques =  "uniques > 1",
  firstColAllUnique = "select(1) %>% allUnique",
  fixedColNames = "cnames == ['firstName','lastName','*']",
  colNamesIn = "cnames in ['one','two','three'] ",
  colNamesIn = "cnames notIn ['one','two','three'] ",
  hasFtype = "ftype == 'CaCaNu' ",
  hasCaFtype = "Ca in ftype",
  hasCtypes = "ctypes == 'NuCaNu'",
  allNumeric = "allCtypes == 'Nu'",
  nrowGreaterTahn = "nrow > 10",
  twoColsCombinationsAllUnique = "select('n1','n2') %>% allUnique"
)
program <- vals$nrows
asp <- parse(program, d)
eval(asp)



tokenize(vals$nrows)
tokenize(vals$uniques)
tokenize(vals$fixedColNames)
tokenize(vals$twoColsCombinationsAllUnique)
parserOperators <- c("select","allUnique","==","in","notIn",
                     ">","<","<=",">=")


tokens <- c()
index <- 0

# Run it!
program <- "1 + 2 * 3 + 4 * 5 + 6"
ast <- parse(program)
ast
