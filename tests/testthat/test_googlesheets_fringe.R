
test_that("Fringe google sheets",{

  # Handling private googlesheets
  #gsheet <- "https://docs.google.com/spreadsheets/d/1qC7rk1iAQj8SFPlMyHOeDM0-gwttjhdQEhFIxtq06DE/edit#gid=0"
  #f <- fringe(gsheet)

  gsheet <- "https://docs.google.com/spreadsheets/d/18nPwnT7il3-tvLBEsqBoiOJCPq1ECEUWC9EJdB6KRCs/edit#gid=0"
  # Creates fringe from google sheet
  s <- gs_url(gsheet)
  tabs <- gs_ws_ls(s)
  gsheet_dic <- gs_read_csv(s, ws = tabs[grepl("_dic", tabs)])
  f1 <- fringe(gsheet)
  expect_equal(f1$dic_$d %>% select(id,label,ctype), gsheet_dic)

  # TODO add tests for error when gsheet is not public

})
