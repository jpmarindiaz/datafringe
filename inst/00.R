library(devtools)
load_all()
document()
#install()
#test()


v <- c("1",NA,"2")
v <- factor(letters[1:5])
guessCtype(v)

guessCtypes(df)



###

f <- readFringe(path = "~/Desktop/objetivos-bogota")
names(f$dic_$d)
dic <- f$dic_$d
getCtypes(f)

varsNu <- selectDicCtypes(f,"Nu",as_list = TRUE)
selectedVarNu <- flatten_chr(sample(varsNu,3))
fchrono <- selectFringeCols(f,c("Anio" ,selectedVarNu))


selectDicCtypes(f,"Ca")
selectDicCtypes(f,"Ca", as_list = TRUE)
selectFringeCtypes(f,"Ca")
getCnames(f)


## ADD TEST FOR SELECT COLS
f2 <- selectFringeCols(f,c("Ciudad" ,"v1_d_001"))
getClabels(f2)

getDictionary(f)
getCinfo(f,"label")


f <- sampleData("Ca-Ye", asFringe = TRUE)
selectDicCtypes(f,"Ca", as_list = TRUE)




x <- sampleData("Ca-Ye")
dic <- data_frame(id = letters[1:2],ctype = c("Ca","Ye"))
f <- fringe(x,dic)

writeFringe(f,"~/Desktop")
readFringe("~/Desktop/x")

t <- fringe(mtcars)
writeFringe(t)
f <- readFringe("mtcars", forceDic = FALSE)
f
f <- readFringe("mtcars", forceDic = FALSE, name = "HOLA")
f




ctypes <- c("Ca","Nu")
dic <- data_frame(id=letters[1:2],ctypes = ctypes)
d <- data_frame(a="x",b=1)
fr <- fringe(d,dic)
writeFringe(fr,"~/Desktop")
readFringe(path ="~/Desktop/d")


# FRINGE
void = data.frame(col1 = character(0), col2 = character(0))
guessCtype(void[1])

d <- data.frame(LET = letters, stringsAsFactors = FALSE)
fringe <- fringe(d)

# DIC
createDic(data_frame(a="x",b=1),dic, as_data_frame = FALSE)
ctypes <- c("Ca","Nu")
dic <- data_frame(id=letters[1:2],ctypes = ctypes)
dic1 <- createDic(data_frame(a="x",b=1),dic)
dicMeta <- Dic$new(dic,ncol = 10)
dicMeta$meta
d <- mtcars
createDic(d)
createDic(d, as.data.frame = FALSE)
createDic(d, dic)
dic4 <- createDic(d, ncol = ncol(d),as.data.frame = FALSE)
dic4






df <- sampleData("CaYeNu")
getDictionary(df)

## Validators parser

vals <- list(
  nrows = "v_ %>% nrow > 1",
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










str(iris)
fringe(iris)
fringe(mtcars)

## ADD TESTS
t <- fringe(mtcars, cdescriptions = 1:11)
# t$asList()
# t$writeCSV()
# t$writeYAML()
writeFringe(t,"mtcars")

f <- readFringe(file = "mtcars")

sameFringes(t,f)

f1 <- t
f2 <- f
sameFringes(f1,f2)



str(t)
str(f)

self <- t
t$data
selectFringeCols(t,1:3)


f <- fringe(mtcars, cdescriptions = names(mtcars))
fringeValidate(f,"allNumeric")

t <- sampleData("CCN", asFringe = FALSE)


cdescriptions <- c("","","2","daf","f")
fringe(iris, cdescriptions = cdescriptions)



d <- sampleData("DXXNNNN")
guessCtypes(d)
getCaCnames(fringe(d),n = 15)


t <- sampleData("CCN", asFringe = TRUE)
cnames <- c("res","sec")
setCnames(t,c("first","second"))
setCnames(t,c("first","second","third"))





## Add
# SELECTNOT COLUMNS
setdiff(names(all),c("cv_dpto","depto","mupio"))










