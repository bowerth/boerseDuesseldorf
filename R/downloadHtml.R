#' @importFrom magrittr %>%
#' @export

downloadHtml <- function(urls, destdir="inst/extdata/provider") {

  stopifnot(dir.exists(destdir))

  ## different behavior if downloading funds instead of provider
  ## 'modes' not required
  ## urls <- sapply(modes, urlFG)
  curlpool <- createUrlPool(urls)

  destdir <<- destdir
  curl::multi_run(pool = curlpool)
  msg <- paste("files downloaded to", destdir)
  return(msg)
}

createUrlPool <- function(urls) {
  curlpool <-
    curl::new_pool(total_con = 20, host_con = 4, multiplex = TRUE)
  sapply(urls,
         curl::curl_fetch_multi,
         success,
         failure,
         curlpool)
  return(curlpool)
}

failure <- function(msg){
  cat("Oh noes! Request failed!", msg, "\n")
}

success <- function(res){
  writeHtmlFile(listitem = res, listfield = "content", destdir = destdir)
}

writeHtmlFile <- function(listitem, listfield = "content", destdir) {
  binfile <- tempfile(tmpdir = destdir, fileext = ".html")
  filecon <- file(binfile, "wb")
  writeBin(object = listitem[[listfield]], con = filecon)
  close(filecon)
}
