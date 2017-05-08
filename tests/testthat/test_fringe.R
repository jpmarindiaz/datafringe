context("fringes")

test_that("Fringe utils", {
  ctype <- c("Cat","Num")
  dic <- data_frame(id=letters[1:2],ctype = ctype)
  d <- data_frame(a="x",b=10)
  fr <- Fringe$new(d, dic_ = dic, name = "d")
  fr2 <- fringe(d,dic)
  expect_equal(fr,fr2)
  expect_equal(getFtype(fr),"Cat-Num")
  expect_equal(fr$dic_,createDic(d, as_data_frame = FALSE))
  dic2 <- rbind(dic,c("c","Gnm"))
  fr3 <- fringe(d,dic2)
  expect_equal(fr$dic_,fr3$dic_)
})


test_that("Create Fringe", {
  #test empty fringees
  void = data_frame(col1 = character(0), col2 = character(0))
  tv <- fringe(void)
  Fringe$new(void)
  expect_equal(tv$data,void)

  ftype <- "Cat-Dat"
  data <- sampleData(ftype)
  fringe(data)

  # test fringes with only one column
  d <- data_frame(LET = letters)
  Fringe$new(d)
  fringe <- fringe(d)
  expect_equal(fringe$data,d)
  Fringe$new(iris)
  fringe <- fringe(iris)
  expect_equal("iris",fringe$name)
  expect_equal(fringe$data, fct_to_chr(iris))
  df <- sampleFringe('Cat-Num')
  expect_equal(getCtypes(df),c('Cat','Num'))
  expect_equal(getCnames(df),names(df$data))
  #expect_equal(getCformats(df),c('','')) ## OJO FORMATS

  data <- sampleFringe("Cat-Num",nrow = 4)
  expect_equal(nrow(data$data), 4)

  # test fringes with shuffled dic
  ctype <- c("Cat","Num")
  dic <- data_frame(id=letters[1:2],ctype = ctype)
  dic_shuffle <- dic %>% arrange(desc(id))
  d <- data_frame(a="x",b=10)
  fr <- fringe(d,dic = dic)
  fr2 <- Fringe$new(d, dic_ = dic_shuffle, name = "d")
  expect_equal(fr,fr2)
  expect_equal(getFtype(fr),"Cat-Num")
  expect_equal(fr$dic_,createDic(d, as_data_frame = FALSE))

  dic2 <- rbind(dic,c("c","Gnm"))
  fr3 <- fringe(d,dic2)
  expect_equal(fr$dic_,fr3$dic_)

  t <- sampleFringe("Cat-Cat-Num")
  cnames <- c("res","sec")
  t$setCnames(cnames, idx = c(3,1))
  expect_equal(getCnames(t),c('sec',names(t$data)[2],'res'))
  cnames = c("a","v","vd")
  t$setCnames(cnames)
  expect_equal(getCnames(setCnames(t,cnames)),getCnames(t))
  expect_error(t$setCnames(c("res","res")))
  expect_error(setCnames(t,c("first","second")))

  t <- sampleFringe("Cat-Cat-Num")
  newDescriptions <- c("res","sec")
  t$setCdescriptions(newDescriptions, idx = c(3,1))
  expect_equal(getCdescriptions(t),c('sec',NA,'res'))
  cdescriptions = c("a","v","vd")
  t$setCdescriptions(cdescriptions)
  expect_equal(getCdescriptions(setCdescriptions(t,cdescriptions)),getCdescriptions(t))
  expect_error(t$setCdescriptions(c("res","res")))
  expect_error(setCdescriptions(t,c("first","second")))

  # t <- sampleData("Cat-Dat-Yea-Num")
  # dic <- data_frame(id = names(t), ctype = c("Cat","Dat","Yea","Num"))
  # fringe(t,dic = dic, name = "xx")
})



test_that("Create fringe with dic", {

  ctypes <- c("Cat","Num")
  dic <- data_frame(id=letters[1:2],ctypes = ctypes)
  d <- data_frame(a="x",b=10)
  fr <- fringe(d,dic)
  expect_equal(getFtype(fr),"Cat-Num")
  expect_equal(fr$dic_,createDic(d, dic, as_data_frame = FALSE))
  # test fringe with dic not in same order
  ctypes <- c("Num","Cat")
  dic <- data_frame(id=c("b","a"),ctypes = ctypes)
  d <- data_frame(a="x",b=10)
  fr <- fringe(d,dic)
  expect_equal(getFtype(fr),"Cat-Num")
  expect_equal(fr$dic_,createDic(d, dic, as_data_frame = FALSE))

  # test fringe error vars in data not in dic
  ctypes <- c("Num","Cat")
  dic <- data_frame(id=c("b","a"),ctypes = ctypes)
  d <- data_frame(a="x",x=10)

  expect_error(fringe(d,dic),"Vars in data not in diccionary: x")
})

# test_that("fringeValidations", {
#   t <- sampleFringe("Ca-Nu")
#   expect_true(fringeValidate(t,"hasCtypes",c("Ca","Nu")))
#   expect_true(fringeValidate(t,"hasFtype","Ca-Nu"))
#   expect_false(fringeValidate(t,"hasAnyFtype",c("Ca-Ca-Nu","Nu-Nu","Nu-Im")))
#   expect_true(fringeValidate(t,"hasColnames",c("a","number")))
#
#   t <- sampleFringe("CN")
#   fringeValidate(t,"colnamesInFringe","a")
#   expect_false(fringeColValidate(t,1,"unique"))
#   expect_false(fringeColValidate(t,"a","unique"))
#   expect_true(fringeColValidate(t,"number","unique"))
# })

test_that("fringeIO",{
  f1 <- fringe(mtcars)
  writeFringe(f1)
  f2 <- readFringe("mtcars", forceDic = FALSE)
  #f2 <- readFringe("mtcars")
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

