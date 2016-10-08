# library(fringer)
# context("validations")
#
# test_that("Fringe validations", {
#   t <- sampleData("CN", asFringe = TRUE)
#   expect_true(fringeValidate(t,"hasFtype","Ca-Nu"))
#   expect_false(fringeValidate(t,"hasCtypes",c("Ca","Nu","Nu")))
#   expect_true(fringeValidate(t,"hasColnames",c("a","number")))
#
# })
#
# test_that("Col validations", {
#
#   t <- sampleData("DXXNNNN", asFringe = FALSE)
#   fringe <- fringe(t)
#   cols <- c("pagePathLevel1","fullReferrer","pageviews")
#   ctype <- "Tx"
#   expect_false(fringeColValidate(fringe,cols,"hasCtype",ctype))
#   expect_true(fringeColValidate(fringe,c("pageviews","avgTimeOnPage"),"hasCtype","Nu"))
#
#   availableSampleData()
#   t <- sampleData("CN",asFringe = TRUE)
#   expect_true(fringeColValidate(t,2,"unique"))
#   expect_false(fringeColValidate(t,"a","unique"))
#
#   # customs validator fun
#   #   f <- function(datas,cols, val) {as.logical(val)}
#   #   a <- colValidate(t,type = "custom",1,f, 0)
#   #   b <- colValidate(t,"custom",1,f, val=1)
#   #   expect_false(a)
#   #   expect_true(b)
# })
#
