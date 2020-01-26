## Libraries and cURL setup ====================================================
library("tidyverse")
library("magrittr")
library("lubridate")
library("stringi")
library("jsonlite")
library("httr")
library("RCurl")
library("data.table")

curlSetOpt(timeout = 200)
has_internet <- function() {
  !is.null(curl::nslookup("r-project.org", error = F))
}

## Cycles and months: shifting from 2018 to weekly =============================
### cycles <- seq(1978, 2018, 2)
### mons   <- seq(1, 24, 1)
cycles <- seq(2016, 2020, 2)
weeks <- seq(lubridate::week(as.Date("2020-12-31")) * 2 - 1)

## Create directories if they do not exist =====================================
for (cycle in cycles) {
  path <- paste0("year", cycle)
  if (!dir.exists(path)) {
    dir.create(path)
  }
  for (week in weeks) {
    subpath <- file.path(path, paste0("week", str_pad(week, 3, pad = "0")))
    if (
      !dir.exists(subpath) &
      !dir.exists(paste0(subpath, " (fin)"))
    ) {
      dir.create(subpath)
    }
  }
}

## API keys ====================================================================
key0 <- key1 <- key2 <- key3 <- key4 <- key5 <-
  key6 <- key7 <- key8 <- key9 <- key10 <- key11 <- "redacted"

## Function for updating when the process is interrupted =======================
find_last_index <- function(x) {
  output <- (x %>% str_match("last_index\":\"([[:digit:]]+)"))[[2]]
  return(output)
}

find_last_date <- function(x) {
  return(x$pagination$last_indexes$last_contribution_receipt_date)
}
