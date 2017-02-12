library(boerseDuesseldorf)
context("getFondsgesellschaften")

test_that("check if correct url is produced", {

  ## destdir_provider <- "../../inst/extdata/provider/"
  destdir_provider <- "resources/download-provider"
  unlink(file.path(destdir_provider, "*"))

  testfile_provider <- "resources/example-provider.html"
  export_file_provider <- "resources/example-provider.tsv"
  ## clean directory

  checkMode("a") %>%
    expect_equal(TRUE)

  checkMode("") %>%
    expect_equal(FALSE)

  urlFG(mode="a") %>%
    expect_equal(
      "http://boerse-duesseldorf.de/fonds/fondgesellschaften?mode=a"
    )

  modes <- c("a", "b")
  getFG(modes = modes, destdir = destdir_provider) %>%
    expect_equal(
      c("http://boerse-duesseldorf.de/fonds/fondgesellschaften?mode=a",
        "http://boerse-duesseldorf.de/fonds/fondgesellschaften?mode=b")
    )

  list.files(destdir_provider) %>% length() %>%
    expect_equal(length(modes))

  ## failure(msg = "text") %>%
  ##   expect_equal(cat("Oh noes! Request failed!", "text", "\n"))

  extractFG_expect <-
    data.frame(label = c("Aachener Grundvermögen",
                         "AACHENER GRUNDVERMÖGEN Kapitalanlagegesellschaft mbH"),
               url = c("/fonds/fondgesellschaften/689",
                       "/fonds/fondgesellschaften/3391"),
               stringsAsFactors = FALSE)

  extractFG_res <-
    extractFG(htmlfile = testfile_provider)

  extractFG_res[1:2,] %>%
    expect_equal(extractFG_expect)

  write.table(extractFG_res,
              file = export_file_provider,
              sep = "\t",
              row.names = FALSE,
              quote=FALSE)

})
