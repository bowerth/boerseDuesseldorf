#' getSummary
#'
#' Get Summary
#'
#' Function to parse wkn summary
#'
#' @param wkns partial URL terminating with WKN e.g. \code{"/fonds/wkn/A1JRDC"}
#' @param destdir path to download HTML files
#' @importFrom magrittr %>%

getItem <- function(wkns, destdir, suffix="/kurskennzahlen") {
  stopifnot(length(wkns) > 0)
  urls <-
    paste0(
      providerConf$boerseDuesseldorf$baseurl,
      wkns,
      suffix)
  downloadHtml(urls = urls,
               destdir = destdir)
  return(unname(urls))
}

#' @export

getSummary <- function(wkns, destdir) {
  getItem(wkns, destdir, suffix="/kurskennzahlen")
}

#' @export

extractSummary <- function(htmlfile) {

  ## htmlfile <- "../../inst/extdata/summary/file14ca14bd5c2f.html"
  doc <-
    htmlfile %>%
    xml2::read_html()

  wkn <- titleWkn(doc)

  summary_tables <-
    doc %>%
    rvest::html_table()
  summary_table <-
    do.call(rbind, summary_tables)
  ## if(nrows(summary_table) == 0) return()

  summary_table_numeric <-
    apply(summary_table[,-1], 2, cleanSummaryNum)

  colnames(summary_table_numeric) <-
    colnames(summary_table_numeric) %>%
    sub(" ", "", .) %>%
    sub("Tage", "d", .) %>%
    sub("Monat", "m", .) %>%
    sub("Jahr|Jahre", "y", .)

  summary_variables <-
    summary_table[[1]] %>%
    cleanSummaryChar()

  summary_df <-
    cbind.data.frame(wkn = wkn,
                     var = summary_variables,
                     summary_table_numeric,
                     stringsAsFactors = FALSE)

  summary_long <-
    summary_df %>%
    reshape2::melt(variable.name = "period",
                   value.name = "value",
                   id.vars = c("wkn", "var")) # %>%
    ## reshape2::dcast(wkn ~ var + period)

  summary_long[["period"]] <-
    as.character(summary_long[["period"]])

  return(summary_long)
}

cleanSummaryChar <- function(x) {
  x_out <-
    x %>%
    tolower() %>%
    gsub("[ |-]", "_", .) %>%
    gsub("ü", "ue", .) %>%
    gsub("ä", "ae", .) %>%
    sub("[.]", "", .)
  return(x_out)
}
## cleanSummaryChar(summary_variables)

cleanSummaryNum <- function(x) {
  x_out <-
    x %>%
    sub(",", ".", .) %>%                # comma to period
    ## sub("[+|-]", "", .) %>%             # remove initial symbol
    sub("[+]", "", .) %>%             # remove initial symbol
    sapply(percentSummary) %>%
    unname()
  return(x_out)
}

percentSummary <- function(x) {
  if (stringr::str_detect(x, "%"))
    x_out <-
      x %>%
      sub("%", "", .) %>%
      as.numeric() %>%
      divideSummary()
  else
    x_out <-
      x %>%
      as.numeric()
  return(x_out)
}

divideSummary <- function(x) {
  x / 100
}

titleWkn <- function(doc) {
  wkn <-
    doc %>%
    xml2::xml_find_first(".//title") %>%
    xml2::xml_text() %>%
    stringr::str_split(" [|] ") %>%
    .[[1]] %>%
    .[2] %>%
    sub("WKN: ", "", .)
  return(wkn)
}
