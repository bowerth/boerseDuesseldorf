#' getFG
#'
#' Get Fondsgesellschaften
#'
#' Function to return list of Fondsgesellschaften
#'
#' @param modes charachter initial letters, e.g. \code{c("a", "b")}
#' @param destdir path to download HTML files
#' @importFrom magrittr %>%
#' @export

getFG <- function(modes, destdir) {
  stopifnot(length(modes) > 0)
  urls <-
    sapply(modes, urlFG)
  downloadHtml(urls, destdir)
  return(unname(urls))
}

#' @export

extractFG <- function(htmlfile, ntable=1) {
  doc <-
    htmlfile %>%
    xml2::read_html() # %>%
  ##   rvest::html_nodes("table") %>%
  ##   .[ntable] %>%
  ##   rvest::html_table(fill = TRUE)
  rows <- xml2::xml_find_all(doc, ".//tr")[-1] # remove header
  ## xml2::xml_find_all(rows, ".//td")
  anodes <- xml2::xml_find_all(rows, ".//a")

  ## link_list <-
  ##   lapply(anodes,
  ##          function(x) {
  ##            list(url = xml2::xml_attr(x, attr = "href"),
  ##                 label = xml2::xml_text(x))})

  link_df <-
    data.frame(
      label = xml2::xml_text(anodes),
      url = xml2::xml_attr(anodes, attr = "href"),
      stringsAsFactors = FALSE
    )

  return(link_df)
}

urlFG <- function(mode) {
  stopifnot(checkMode(mode))
  theurl <-
    file.path(
      providerConf$boerseDuesseldorf$baseurl,
      paste0(
        providerConf$boerseDuesseldorf$fgurl,
        paste0("?mode=", mode)
      ))
  return(theurl)
}

checkMode <- function(mode) {
  mode_valid <- c("1", letters)
  check <- ifelse(tolower(mode) %in% mode_valid, TRUE, FALSE)
  return(check)
}
