#' getHistory
#'
#' Get History
#'
#' Function to parse wkn history
#'
#' @param wkns partial URL terminating with WKN e.g. \code{"/fonds/wkn/A1JRDC"}
#' @param destdir path to download HTML files
#' @importFrom magrittr %>%
#' @export

getHistory <- function(wkns, destdir) {
  getItem(wkns, destdir, suffix="/historische_kurse")
}

#' @export

extractHistory <- function(htmlfile) {

  doc <-
    htmlfile %>%
    xml2::read_html()

  wkn <- titleWkn(doc)

  html_table_raw <-
    doc %>%
    xml2::xml_find_all(".//table") %>%
    xml2::xml_children()

  theader <-
    html_table_raw[[1]] %>%
    xml2::xml_contents() %>%
    xml2::xml_children() %>%
    xml2::xml_text()

  tbody <-
    html_table_raw[[2]] %>%
    xml2::xml_contents()

  row_list <-
    lapply(tbody, getRow)

  history_table <-
    do.call(rbind.data.frame, row_list)

  names(history_table) <-
    ## c("Datum", "Eröffnung", "Tageshoch", "Tagestief", "Schluss", "Stücke")
    theader

  history_table_numeric <-
    suppressWarnings(apply(history_table[,-1], 2, cleanHistoryNum))

  colnames(history_table_numeric) <-
    colnames(history_table_numeric) %>%
    sub("Eröffnung", "open", .) %>%
    sub("Tageshoch", "high", .) %>%
    sub("Tagestief", "low", .) %>%
    sub("Schluss", "close", .) %>%
    sub("Stücke", "units", .)

  history_date <-
    history_table[[1]] %>%
    cleanHistoryDate()

  history_df <-
    cbind.data.frame(wkn = wkn,
                     date = history_date,
                     history_table_numeric,
                     stringsAsFactors = FALSE)

  history_long <-
    history_df %>%
    reshape2::melt(variable.name = "var",
                   value.name = "value",
                   id.vars = c("wkn", "date"))

  history_long[["var"]] <-
    as.character(history_long[["var"]])

  history_long_filter <-
    history_long %>%
    subset(!is.na(history_long[["value"]]))

  return(history_long_filter)
}

#' @export

xtsHistory <- function(data) {
  data_wide <-
    data %>%
    reshape2::dcast(date ~ wkn + var, value.var = "value")
  rownames(data_wide) <- data_wide[["date"]]
  data_xts <-
    data_wide[, -1] %>%
    xts::as.xts()
  return(data_xts)
}
## xtsHistory(extractHistory_res) %>% head()
## xtsHistory(extractHistory_res) %>% class()

cleanHistoryNum <- function(x) {
  x_out <-
    x %>%
    sub(",", ".", .) %>%
    as.numeric()
  return(x_out)
}

cleanHistoryDate <- function(x) {
  x_out <-
    x %>%
    as.Date(format = "%d.%m.%Y")
  return(x_out)
}
## cleanHistoryDate("10.02.2017")

getRow <- function(tbody) {
  tbody %>%
    xml2::xml_children() %>%
    xml2::xml_text()
}

