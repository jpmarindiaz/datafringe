context("fringe funs")


test_that("Fringe funs", {
  f1 <- sampleFringe("Cat-Num-Yea")
  ctypes <- c("Cat","Yea")
  dic2 <- selectDicCtypes(f1,ctypes)
  expect_equal(dic2$ctype,c("Cat","Yea"))
  f2 <- selectFringeCtypes(f1,ctypes)
  expect_equal(getFtype(f2),"Cat-Yea")
  f3 <- sampleData("Cat-Cat-Cat-Num")
  f3nms <- names(f3)
  dic <- data_frame(id=f3nms,
                    ctype = c("Cat","Cat","Cat","Num"),
                    visualize = c(NA,TRUE,FALSE,TRUE))
  f3 <- fringe(f3,dic)
  dic3 <- selectDicCtypes(f3,ctypes = c("Cat","Num"), filter = "visualize")
  expect_equal(dic3$id, f3nms[c(2,4)])
  setCnames(f1,c("a","f1_b","f1_c"))
  setCnames(f2,c("a","f2_b"))
  f1$name <- "F1"
  f2$name <- "F2"
  fjoin <- joinFringes(f1,f2)
  expect_true(all(fjoin$dic_$d$id %in% union(getCnames(f1),getCnames(f2))))
  f1$name
  fjoin$name <- "FJOIN"
  ftype <- "Cat-Cat-Dat"
  f4 <- sampleFringe(ftype)
  setCnames(f4,c("a","f4_b","f4_c"))
  f4$name <- "F4"
  fjoin3 <- joinFringes(fjoin,f4)
  expect_equal(c(NA,"F1","F1","F2","F4","F4"),fjoin3$dic_$d$join_group)

})

