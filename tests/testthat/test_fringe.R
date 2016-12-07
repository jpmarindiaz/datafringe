context("fringes")

test_that("Fringe utils", {
  ctype <- c("Ca","Nu")
  dic <- data_frame(id=letters[1:2],ctype = ctype)
  d <- data_frame(a="x",b=1)
  fr <- Fringe$new(d, dic_ = dic, name = "d")
  fr2 <- fringe(d,dic)
  expect_equal(fr,fr2)
  expect_equal(getFtype(fr),"Ca-Nu")
  expect_equal(fr$dic_,createDic(d, as_data_frame = FALSE))
  dic2 <- rbind(dic,c("c","Ge"))
  fr3 <- fringe(d,dic2)
  expect_equal(fr$dic_,fr3$dic_)
})

test_that("Create Fringe", {
  #test empty fringees
  void = data_frame(col1 = character(0), col2 = character(0))
  expect_equal(guessCtype(void[1]),"_")
  expect_equal(guessCtype(void %>% .[[1]]),"_")
  expect_equal(guessCtypes(void),c("_","_"))
  tv <- fringe(void)
  Fringe$new(void)
  expect_equal(tv$data,void)

  # test fringes with only one column
  d <- data_frame(LET = letters)
  Fringe$new(d)
  fringe <- fringe(d)
  expect_equal(fringe$data,d)
  Fringe$new(iris)
  fringe <- fringe(iris)
  expect_equal("iris",fringe$name)
  expect_equal(fringe$data, fct_to_chr(iris))
  df <- sampleData('Ca-Nu', asFringe = TRUE)
  expect_equal(getCtypes(df),c('Ca','Nu'))
  expect_equal(getCnames(df),c('a','b'))
  #expect_equal(getCformats(df),c('','')) ## OJO FORMATS

  t <- sampleData("Ca-Ca-Nu", asFringe = TRUE)
  cnames <- c("res","sec")
  t$setCnames(cnames, idx = c(3,1))
  expect_equal(getCnames(t),c('sec','b','res'))
  cnames = c("a","v","vd")
  t$setCnames(cnames)
  expect_equal(getCnames(setCnames(t,cnames)),getCnames(t))
  expect_error(t$setCnames(c("res","res")))
  expect_error(setCnames(t,c("first","second")))

  t <- sampleData("Ca-Ca-Nu", asFringe = TRUE)
  newDescriptions <- c("res","sec")
  t$setCdescriptions(newDescriptions, idx = c(3,1))
  expect_equal(getCdescriptions(t),c('sec',NA,'res'))
  cdescriptions = c("a","v","vd")
  t$setCdescriptions(cdescriptions)
  expect_equal(getCdescriptions(setCdescriptions(t,cdescriptions)),getCdescriptions(t))
  expect_error(t$setCdescriptions(c("res","res")))
  expect_error(setCdescriptions(t,c("first","second")))
})



test_that("Create fringe with dic", {

  ctypes <- c("Ca","Nu")
  dic <- data_frame(id=letters[1:2],ctypes = ctypes)
  d <- data_frame(a="x",b=1)
  fr <- fringe(d,dic)
  expect_equal(getFtype(fr),"Ca-Nu")
  expect_equal(fr$dic_,createDic(d, dic, as_data_frame = FALSE))
  # test fringe with dic not in same order
  ctypes <- c("Nu","Ca")
  dic <- data_frame(id=c("b","a"),ctypes = ctypes)
  d <- data_frame(a="x",b=1)
  fr <- fringe(d,dic)
  expect_equal(getFtype(fr),"Ca-Nu")
  expect_equal(fr$dic_,createDic(d, dic, as_data_frame = FALSE))

  # test fringe error vars in data not in dic
  ctypes <- c("Nu","Ca")
  dic <- data_frame(id=c("b","a"),ctypes = ctypes)
  d <- data_frame(a="x",x=1)

  expect_error(fringe(d,dic),"Vars in data not in diccionary: x")
})

test_that("Sample Data", {
  t <- sampleData("Ca-Nu", asFringe = TRUE)
  expect_equal(getFtype(t),"Ca-Nu")
  expect_error(sampleData("XXXXXX", asFringe = TRUE))
  t <- sampleData("Ca-Nu", gt0 = FALSE)
  expect_true(any(t$b < 0))
})

test_that("Fringe funs", {
  f <- sampleData("Ca-Nu-Ye", asFringe = TRUE)
  ctypes <- c("Ca","Ye")
  dic2 <- selectDicCtypes(f,ctypes)
  expect_equal(dic2$ctype,"Ca")
  f2 <- selectFringeCtypes(f,ctypes)
  expect_equal(getFtype(f2),"Ca")
  f3 <- sampleData("Ca-Ca-Ca-Nu")
  dic <- data_frame(id=letters[1:4],
                    ctype = c("Ca","Ca","Ca","Nu"),
                    visualize = c(NA,TRUE,FALSE,TRUE))
  f3 <- fringe(f3,dic)
  dic3 <- selectDicCtypes(f3,ctypes = c("Ca","Nu"), filter = "visualize")
  expect_equal(dic3$id, c("b","d"))
  })



# test_that("fringeValidations", {
#   t <- sampleData("Ca-Nu",asFringe = TRUE)
#   expect_true(fringeValidate(t,"hasCtypes",c("Ca","Nu")))
#   expect_true(fringeValidate(t,"hasFtype","Ca-Nu"))
#   expect_false(fringeValidate(t,"hasAnyFtype",c("Ca-Ca-Nu","Nu-Nu","Nu-Im")))
#   expect_true(fringeValidate(t,"hasColnames",c("a","number")))
#
#   t <- sampleData("CN",asFringe = TRUE)
#   fringeValidate(t,"colnamesInFringe","a")
#   expect_false(fringeColValidate(t,1,"unique"))
#   expect_false(fringeColValidate(t,"a","unique"))
#   expect_true(fringeColValidate(t,"number","unique"))
# })

test_that("fringeIO",{
  f1 <- fringe(mtcars)
  writeFringe(f1)
  f2 <- readFringe("mtcars", forceDic = FALSE)
  f2 <- readFringe("mtcars")
  unlink("mtcars_data.csv")
  unlink("mtcars_dic_.csv")
  expect_true(sameFringes(f1,f2))
  # write
  path <- tempdir()
  writeFringe(f1,path)
  f3 <- readFringe(file.path(path,f1$name))
  expect_equal(f1$name, f3$name)
  expect_true(sameFringes(f1,f3))
  expect_true(file.exists(file.path(path,paste0(f1$name,"_data.csv"))))
  expect_true(file.exists(file.path(path,paste0(f1$name,"_dic_.csv"))))
  unlink(file.path(path,paste0(f1$name,"_data.csv")))
  unlink(file.path(path,paste0(f1$name,"_dic_.csv")))

})

