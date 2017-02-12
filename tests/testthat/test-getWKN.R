library(boerseDuesseldorf)
context("getWKN")

test_that("check if WKNs exist", {

  destdir_fonds <- "resources/download-fonds"
  unlink(file.path(destdir_fonds, "*"))

  export_file_provider <- "resources/example-provider.tsv"
  export_file_fonds <- "resources/example-fonds.tsv"
  testfile_fonds_empty <- "resources/example-fonds-empty.html"
  testfile_fonds_nonempty <- "resources/example-fonds-nonempty.html"

  provider_df_import <-
    read.delim(export_file_provider)
  fgs <-
    provider_df_import$url[1:2]

  getWKN(fgs = fgs, destdir = destdir_fonds) %>%
    expect_equal(c("http://boerse-duesseldorf.de/fonds/fondgesellschaften/689",
                   "http://boerse-duesseldorf.de/fonds/fondgesellschaften/3391"))

  extractWKN_expect <-
    data.frame(label = c("UBS-ETF Barclays Capital US 7-10 Year Treasury Bond",
                         "UBS-ETF EMU Values"),
               url = c("/fonds/wkn/A1JRDC",
                       "/fonds/wkn/A0X97R"),
               stringsAsFactors = FALSE)

  extractWKN_res <-
    extractWKN(htmlfile = testfile_fonds_nonempty)

  extractWKN_res[1:2, ] %>%
    expect_equal(extractWKN_expect)

  extractWKN(htmlfile = testfile_fonds_empty) %>%
    expect_equal(NULL)

  write.table(extractWKN_res,
              file = export_file_fonds,
              sep = "\t",
              row.names = FALSE,
              quote = FALSE)

})
