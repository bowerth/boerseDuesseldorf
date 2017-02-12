library(boerseDuesseldorf)
context("getSummary")

test_that("check if Summary exist", {

  destdir_summary <- "resources/download-summary"
  unlink(file.path(destdir_summary, "*"))
  testfile_summary <- "resources/example-summary.html"
  export_file_summary <- "resources/example-summary.rda"
  export_file_fonds <- "resources/example-fonds.tsv"

  fonds_df_sorted <- read.delim(export_file_fonds)

  ## getWKN(wkns = wkns, destdir = destdir_summary, suffix="/kurskennzahlen") %>%
  getSummary(wkns = fonds_df_sorted$url[1:2], destdir = destdir_summary) %>%
    expect_equal(c("http://boerse-duesseldorf.de/fonds/wkn/A1JRDC/kurskennzahlen",
                   "http://boerse-duesseldorf.de/fonds/wkn/A0X97R/kurskennzahlen"))

  divideSummary(36.40) %>%
    expect_equal(0.3640)

  c("20.92%", "20.92") %>%
    sapply(percentSummary) %>%
    unname() %>%
    expect_equal(c(0.2092, 20.92))

  c("+20,92%", "-20,92") %>%
    cleanSummaryNum() %>%
    expect_equal(c(0.2092, -20.92))

  c("Sharpe-Ratio", "Volatilität", "Abstand vom Hoch", "Max. Verlust") %>%
    cleanSummaryChar() %>%
    expect_equal(c("sharpe_ratio", "volatilitaet", "abstand_vom_hoch", "max_verlust"))

  extractSummary_expect <-
    data.frame(wkn = c("A0X97R", "A0X97R"),
               var = c("hoch", "tief"),
               period = c("14d", "14d"),
               value = c(37.5500, 36.8200),
               stringsAsFactors = FALSE)

  extractSummary_res <-
    extractSummary(htmlfile = testfile_summary)

  extractSummary_res[1:2, ] %>%
    expect_equal(extractSummary_expect)

  ## write.table(extractSummary_res,
  ##             file = export_file_summary,
  ##             sep = "\t",
  ##             row.names = FALSE,
  ##             quote = FALSE)

  save(extractSummary_res, file = export_file_summary)

})
