# library(fringer)
# context("Fringe properties")
# test_that("properties", {
#   df <- data.frame(x = runif(5), y = runif(5))
#   c <- fringepkg(df)
#   expect_is(c,"Fringepkg")
#
#   nms <- c("N","N")
#   #names(nms) <- names(df)
#   expect_equal(guessCtypes(df),nms)
#
#   cols <- "dist"
#   expect_equal(guessCtypes(df),getCtypes(c))
#   expect_equal(letters[1:2],names(getDatafringe(c, withNames=FALSE)))
# })
