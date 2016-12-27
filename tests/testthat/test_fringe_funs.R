context("fringe funs")


test_that("Fringe funs", {
  f1 <- sampleData("Ca-Nu-Ye", asFringe = TRUE)
  ctypes <- c("Ca","Ye")
  dic2 <- selectDicCtypes(f1,ctypes)
  expect_equal(dic2$ctype,c("Ca","Ye"))
  f2 <- selectFringeCtypes(f1,ctypes)
  expect_equal(getFtype(f2),"Ca-Ye")
  f3 <- sampleData("Ca-Ca-Ca-Nu")
  f3nms <- names(f3)
  dic <- data_frame(id=f3nms,
                    ctype = c("Ca","Ca","Ca","Nu"),
                    visualize = c(NA,TRUE,FALSE,TRUE))
  f3 <- fringe(f3,dic)
  dic3 <- selectDicCtypes(f3,ctypes = c("Ca","Nu"), filter = "visualize")
  expect_equal(dic3$id, f3nms[c(2,4)])
  setCnames(f1,c("a","f1_b","f1_c"))
  setCnames(f2,c("a","f2_b"))
  f1$name <- "F1"
  f2$name <- "F2"
  fjoin <- joinFringes(f1,f2)
  expect_true(all(fjoin$dic_$d$id %in% union(getCnames(f1),getCnames(f2))))
  f1$name
  fjoin$name <- "FJOIN"
  f4 <- sampleData("Ca-Ca-Da", asFringe = TRUE)
  setCnames(f4,c("a","f4_b","f4_c"))
  f4$name <- "F4"
  fjoin3 <- joinFringes(fjoin,f4)
  expect_equal(c(NA,"F1","F1","F2","F4","F4"),fjoin3$dic_$d$join_group)

})

