## ----init, echo=TRUE-----------------------------------------------------
library(boerseDuesseldorf)
library(dplyr)
library(tidyr)
## destdir_provider <- "../inst/extdata/provider"
## destdir_fonds <- "../inst/extdata/fonds"
## destdir_summary <- "../inst/extdata/summary"
destdir_history <- "../inst/extdata/history"
## export_file_provider <- "../inst/extdata/provider.tsv"
## export_file_fonds <- "../inst/extdata/fonds.tsv"
## export_file_summary <- "../inst/extdata/summary.rda"
export_file_history <- "../inst/extdata/history.rda"
## for pkgdown
## destdir_provider <- system.file("extdata", "provider", package = "boerseDuesseldorf")
export_file_provider <-
  system.file("extdata", "provider.tsv", package = "boerseDuesseldorf")
export_file_fonds <-
  system.file("extdata", "fonds.tsv", package = "boerseDuesseldorf")
export_file_summary <-
  system.file("extdata", "summary.rda", package = "boerseDuesseldorf")

## ----download_fg, eval=FALSE---------------------------------------------
#  ## modes <- c("a", "b")
#  modes <- c("1", letters)
#  unlink(file.path(destdir_provider, "*"))
#  getFG(modes = modes, destdir = destdir_provider)

## ----extract_fg, eval=FALSE----------------------------------------------
#  htmlfiles_provider <- list.files(destdir_provider)
#  htmlfiles_provider_relpath <- file.path(destdir_provider, htmlfiles_provider)
#  provider_list <-
#    lapply(htmlfiles_provider_relpath, extractFG)
#  provider_df <- do.call("rbind", provider_list)
#  provider_df_sorted <-
#    provider_df %>%
#    arrange(label)
#  write.table(provider_df_sorted,
#              file = export_file_provider,
#              sep = "\t",
#              row.names = FALSE,
#              quote = FALSE)

## ----show_fg, eval=TRUE, results="as.is"---------------------------------
provider_df_sorted <-
  read.delim(export_file_provider)
knitr::kable(provider_df_sorted[1:10,])

## ----download_wkn, eval=FALSE--------------------------------------------
#  fgs <-
#    provider_df_import$url
#  getWKN(fgs = fgs, destdir = destdir_fonds)

## ----extract_wkn, eval=FALSE---------------------------------------------
#  htmlfiles_fonds <- list.files(destdir_fonds)
#  htmlfiles_fonds_relpath <- file.path(destdir_fonds, htmlfiles_fonds)
#  fonds_list <-
#    lapply(htmlfiles_fonds_relpath, extractWKN)
#  fonds_df <- do.call("rbind", fonds_list)
#  fonds_df_sorted <-
#    fonds_df %>%
#    arrange(label)
#  write.table(fonds_df_sorted,
#              file = export_file_fonds,
#              sep = "\t",
#              row.names = FALSE,
#              quote = FALSE)

## ----show_wkn, eval=TRUE, results="as.is"--------------------------------
fonds_df_sorted <-
  read.delim(export_file_fonds)
knitr::kable(fonds_df_sorted[1:10,])

## ----download_summary, eval=FALSE----------------------------------------
#  getSummary(wkns = fonds_df_sorted$url, destdir = destdir_summary)

## ----extract_summary, eval=FALSE, warning=FALSE--------------------------
#  htmlfiles_summary <- list.files(destdir_summary)
#  htmlfiles_summary_relpath <- file.path(destdir_summary, htmlfiles_summary)
#  summary_list <-
#    lapply(htmlfiles_summary_relpath, extractSummary)
#  summary_df <- do.call("rbind", summary_list)
#  summary_df_sorted <-
#    summary_df %>%
#    arrange(wkn)
#  save(summary_df_sorted,
#       file = export_file_summary)

## ----show_summary, eval=TRUE, results="as.is"----------------------------
load(file = export_file_summary)
summary_df_sorted %>%
  tbl_df %>%
  filter(period %in% c("1y", "3y", "5y")) %>%
  filter(var %in% c("performance")) %>%
  select(wkn, var, period, value) %>%
  spread(key = period, value = "value") %>%
  arrange(desc(`5y`)) %>%
  top_n(10) %>%
  knitr::kable()

## ----download_history, eval=FALSE----------------------------------------
#  getHistory(wkns = fonds_df_sorted$url, destdir = destdir_history)

## ----extract_history, eval=FALSE, warning=FALSE--------------------------
#  htmlfiles_history <- list.files(destdir_history)
#  htmlfiles_history_relpath <- file.path(destdir_history, htmlfiles_history)
#  history_list <-
#    lapply(htmlfiles_history_relpath, extractHistory)
#  history_df <- do.call("rbind", history_list)
#  history_df_sorted <-
#    history_df %>%
#    arrange(wkn, desc(date))
#  save(history_df_sorted,
#       file = export_file_history)

