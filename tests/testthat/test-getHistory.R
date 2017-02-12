library(boerseDuesseldorf)
context("getHistory")

test_that("check if History exists", {

  destdir_history <- "resources/download-history"
  unlink(file.path(destdir_history, "*"))
  testfile_history <- "resources/example-history.html"
  export_file_history <- "resources/example-history.rda"
  export_file_fonds <- "resources/example-fonds.tsv"

  fonds_df_sorted <- read.delim(export_file_fonds)

  getHistory(wkns = fonds_df_sorted$url[1:2], destdir = destdir_history) %>%
    expect_equal(c("http://boerse-duesseldorf.de/fonds/wkn/A1JRDC/historische_kurse",
                   "http://boerse-duesseldorf.de/fonds/wkn/A0X97R/historische_kurse"))

  extractHistory_expect <-
    data.frame(
      wkn = c("A0HMTV", "A0HMTV"),
      date = as.Date(c("2017-02-10", "2017-02-09")),
      var = c("open", "open"),
      ## open = c(121.67, 120.35),
      ## high = c(121.67, 120.35),
      ## low = c(121.67, 120.3),
      ## close = c(121.67, 120.3),
      ## units = c(NA, NA),
      value = c(121.67, 120.35),
      stringsAsFactors = FALSE
    )

  extractHistory_res <-
    extractHistory(htmlfile = testfile_history)

  ## str(extractHistory_expect)
  ## str(extractHistory_res[1:2, ])
  extractHistory_res[1:2, ] %>%
    expect_equal(extractHistory_expect)

})
