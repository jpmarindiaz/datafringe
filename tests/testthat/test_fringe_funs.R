context("fringe funs")


test_that("Fringe funs", {
  f1 <- sampleData("Ca-Nu-Ye", asFringe = TRUE)
  ctypes <- c("Ca","Ye")
  dic2 <- selectDicCtypes(f1,ctypes)
  expect_equal(dic2$ctype,c("Ca","Ye"))
  f2 <- selectFringeCtypes(f1,ctypes)
  expect_equal(getFtype(f2),"Ca-Ye")
  f3 <- sampleData("Ca-Ca-Ca-Nu")
  dic <- data_frame(id=letters[1:4],
                    ctype = c("Ca","Ca","Ca","Nu"),
                    visualize = c(NA,TRUE,FALSE,TRUE))
  f3 <- fringe(f3,dic)
  dic3 <- selectDicCtypes(f3,ctypes = c("Ca","Nu"), filter = "visualize")
  expect_equal(dic3$id, c("b","d"))
  setCnames(f1,c("a","f1_b","f1_c"))
  f1$name <- "F1"
  f2$name <- "F2"
  fjoin <- joinFringes(f1,f2)
  expect_true(all(fjoin$dic_$d$id %in% union(getCnames(f1),getCnames(f2))))

})

