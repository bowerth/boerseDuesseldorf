#' getWKN
#'
#' Get WKN
#'
#' Function to return table of WKNs
#'
#' @param fgs partial URL terminating with FG ID, e.g. \code{"/fonds/fondgesellschaften/3507"}
#' @param destdir path to download HTML files
#' @importFrom magrittr %>%
#' @export

getWKN <- function(fgs, destdir) {
  stopifnot(length(fgs) > 0)
  urls <-
    paste0(providerConf$boerseDuesseldorf$baseurl,
           fgs)
  downloadHtml(urls = urls,
               destdir = destdir)
  return(unname(urls))
}

#' @export

extractWKN <- function(htmlfile, ntable=1) {
  doc <-
    htmlfile %>%
    xml2::read_html()
  rows <-
    xml2::xml_find_all(doc, ".//tr")[-1] # remove header

  if(length(rows) == 0) return()

  anodes <-
    xml2::xml_find_all(rows, ".//a")

    link_df <-
      data.frame(
        label = xml2::xml_text(anodes),
        url = xml2::xml_attr(anodes, attr = "href"),
        stringsAsFactors = FALSE
      ) %>%
      .[stringr::str_detect(.$url, "wkn"), ]

  rownames(link_df) <- NULL

  return(link_df)
}
