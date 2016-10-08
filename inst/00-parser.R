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
  if(token %in% c("%>%"))
    return(15)
  return(0)
}

parseExpressionStart <- function(d=NULL) {
  #force(d)
  token <- consume()
  message("Token Start: ",token)
  dOperators <- list(
    "select" = function(d,...) dplyr::select_(d,...),
    "allUnique" = function(d) nrow(d) == nrow(unique(d)),
    "uniques" = function(d) nrow(unique(d)),
    "nrow" = function(d) nrow(d),
    "ncol" = function(d) ncol(d),
    "cnames" = function(d) getCnames(d),
    "ctypes" = function(d) guessCtypes(d),
    "ftype" = function(d) getFtype(d)
  )

  tokenFunName <- gsub('\\(.*?\\)', '', token) # remove everything in '(*)'
  tokenFunName <- names(dOperators)[match(tokenFunName,names(dOperators))]
  tokenArgs <- getArgsFromFunctionString(token)
  if(!is.na(tokenFunName)){
    f <- dOperators[[tokenFunName]]
    fArgs <- c(list(d),tokenArgs)
    return(do.call(f,fArgs))
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

parseExpressionContinuation <- function(node, d = NULL) {
  token <- consume()
  message("TOKEN PARSE EXPRESSION CONT: ",token)
  call(token,node,
       parseExpression(
         precedence = binaryPrecedence(token),
         d = d)
       )
}

parseExpression <- function(precedence = 0,d = NULL) {
  #str(d)
  node <- parseExpressionStart(d)
  str(node)
  message("IDX: ",index,"  NODE: ",node)
  while (precedence < binaryPrecedence(current()))
    node <- parseExpressionContinuation(node, d = d)
  node
}

# Our entry-point for parsing programs.
parse <- function(program,d) {
  tokens <<- tokenize(program)
  index <<- 1
  parseExpression(d = d)
}

`in` <- function (x, table) all(match(x, table, nomatch = 0L) > 0L)
`equals` <- function(x,y) all(x == y)



d <- cars
program <- 'd %>% select("speed") %>% ncol'
parse(program,d)


program <- 'cnames equals ["speed","dist"]'
eval(parse(program,d))
program <- 'select(1)'
parse(program,d)
program <- 'select("speed")'
parse(program,d)


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
