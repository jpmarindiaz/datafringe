context("Fringe pkgs")

test_that("Fpkg loading", {

  path <- sysfile("fringes/objetivos")
  files <- list.files(path)
  frsNames <- list_fringe(path)$id
  frsFilenames <- c(paste0(frsNames,"_data.csv"),paste0(frsNames,"_dic_.csv"))
  expect_true(all(frsFilenames %in% files))
  fringe_idx <- sysfile("fringes/objetivos/fringe_idx.csv")
  fringes_path <- sysfile("fringes/objetivos")
  sqlite_path <- tempfile(fileext = ".sqlite3")
  sqlite_out <- write_fpkg_sqlite(fringes_path, sqlite_path, fringe_idx = fringe_idx)
  expect_equal(sqlite_path,sqlite_out)

  db <- src_sqlite(sqlite_out)
  tableNames <- src_tbls(db)
  expect_true(all(file_path_sans_ext(frsFilenames) %in% tableNames))
  unlink(sqlite_out)

})
