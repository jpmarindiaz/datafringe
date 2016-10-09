context("dic")

test_that("Create Dic", {

  ctypes <- c("Ca","Nu")
  dic <- data_frame(id=letters[1:2],ctype = ctypes)
  dic1 <- createDic(data_frame(a="x",b=1),dic)
  expect_equal(dic1 %>% select(id,ctype),dic)

  dicMeta <- Dic$new(dic,ncol = 10, moreInfo = "more info")
  expect_equal(dicMeta$meta,list(ncol = 10, moreInfo = "more info"))

  d <- mtcars
  dic2 <- createDic(d)
  expect_equal(unique(dic2$ctype),"Nu")

  dic_ <- createDic(d, as_data_frame = FALSE)
  expect_equal(class(dic_),c("Dic","R6"))
  expect_error(createDic(d, dic))
  dicUser = data.frame(id = names(d),description = paste0("desc_",names(d)))
  dic3 <- createDic(d,dicUser)
  expect_true(all(names(dicUser) %in% names(dic3)))

  dic4 <- createDic(d, ncol = ncol(d),as_data_frame = FALSE)
  expect_true(dic4$meta$ncol == 11)

  # #test empty fringees
  # void = data.frame(col1 = character(0), col2 = character(0))
  # expect_equal(guessCtype(void[1]),"_")
  # expect_equal(guessCtypes(void),c("_","_"))
  # tv <- fringe(void)
  # expect_equal(tv$data,void)

  # # test fringes with only one column
  # d <- data.frame(LET = letters, stringsAsFactors = FALSE)
  # fringe <- fringe(d)
  # expect_equal(fringe$data,d)
  # fringe <- fringe(iris)
  # expect_equal("iris",fringe$name)
  # iris2 <- dfFactorsToCharacters(iris)
  # expect_equal(fringe$data, iris2)
  # df <- sampleData('CN', asFringe = TRUE)
  # expect_equal(getCtypes(df),c('Ca','Nu'))
  # expect_equal(getCnames(df),c('a','number'))
  # #expect_equal(getCformats(df),c('','')) ## OJO FORMATS
  #
  # t <- sampleData("CCN", asFringe = TRUE)
  # cnames <- c("res","sec")
  # t$setCnames(cnames, idx = c(3,1))
  # expect_equal(getCnames(t),c('sec','category2','res'))
  # cnames = c("a","v","vd")
  # t$setCnames(cnames)
  # expect_equal(getCnames(setCnames(t,cnames)),getCnames(t))
  # expect_error(t$setCnames(c("res","res")))
  # expect_error(setCnames(t,c("first","second")))
  #
  # t <- sampleData("CCN", asFringe = TRUE)
  # cdescriptions <- c("res","sec")
  # t$setCdescriptions(cdescriptions, idx = c(3,1))
  # expect_equal(getCdescriptions(t),c('sec','','res'))
  # cdescriptions = c("a","v","vd")
  # t$setCdescriptions(cdescriptions)
  # expect_equal(getCdescriptions(setCdescriptions(t,cdescriptions)),getCdescriptions(t))
  # expect_error(t$setCdescriptions(c("res","res")))
  # expect_error(setCdescriptions(t,c("first","second")))
})




