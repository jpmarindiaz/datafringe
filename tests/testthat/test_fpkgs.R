context("Fringe pkgs")

test_that("Fpkg loading", {

  fd1 <- sampleData("Ca-Da-Ye-Nu",asFringe = TRUE)
  fd2 <- sampleData("Ca-Ca-Nu",asFringe = TRUE)
  frs <- list(fd1,fd2)
  names(frs) <- c("d1","d2")
  tmpfile <- tempfile(fileext = ".sqlite3")
  write_fpkg_sqlite(frs,tmpfile)
  frsnames <- list_fringes_sqlite(tmpfile) %>% .$id
  expect_equal(frsnames, names(frs))
  db <- src_sqlite(tmpfile)
  d1sqlite <- readFringeSqlite("d1",db)
  expect_equal(getCnames(fd1),getCnames(d1sqlite))
  expect_equal(fd1$dic_$d,d1sqlite$dic_$d)
  unlink(tmpfile)

  path <- sysfile("fringes/objetivos")
  files <- list.files(path)
  frsNames <- list_fringes(path)$id
  frsFilenames <- c(paste0(frsNames,"_data.csv"),paste0(frsNames,"_dic_.csv"))
  expect_true(all(frsFilenames %in% files))
  fringe_idx <- sysfile("fringes/objetivos/fringe_idx.csv")
  fringes_path <- sysfile("fringes/objetivos")
  sqlite_path <- tempfile(fileext = ".sqlite3")
  sqlite_out <- write_fpkg_sqlite(fringes_path, sqlite_path, fringe_idx = fringe_idx)
  expect_equal(sqlite_path,sqlite_out)

  sqlite_path <- sqlite_out
  fringe_idx_sqlite <- read_fringe_idx_sqlite(sqlite_path)
  expect_equal(nrow(list_fringes_sqlite(sqlite_path)),8)
  fringe_idx <- read_csv(fringe_idx)
  expect_equal(fringe_idx_sqlite %>% .$id, fringe_idx %>% .$id)

  frs_idx <- fringe_idx %>% filter(!exclude) %>% .$id
  frs_sqlite <- list_fringes_sqlite(sqlite_path) %>% .$id
  expect_equal(intersect(frs_idx,frs_sqlite),union(frs_idx,frs_sqlite))

  db <- src_sqlite(sqlite_out)
  tableNames <- src_tbls(db)
  expect_true(all(file_path_sans_ext(frsFilenames) %in% tableNames))

  fr_csv <- readFringe(file.path(sysfile("fringes/objetivos"),"objetivos_comparada"))
  fr_sqlite <- readFringeSqlite("objetivos_comparada",db)
  expect_equal(fr_csv$name,fr_sqlite$name)
  expect_equal(getCnames(fr_csv),getCnames(fr_sqlite))
  expect_true(identical(fr_csv$data,fr_sqlite$data))
  expect_true(identical(fr_csv$dic_$d,fr_sqlite$dic_$d))
  # expect_equal(fr_csv$data,fr_sqlite$data)
  # expect_equal(fr_csv$dic_$d,fr_sqlite$dic_$d)

  excludeCols <- c("v1_d_001","v1_d_003","v1_d_004")
  fr_sqlite_exclude <- readFringeSqlite("objetivos_comparada",db, excludeCols = excludeCols)
  expect_true(all(!excludeCols %in% getCnames(fr_sqlite_exclude)))

  unlink(sqlite_out)
})
