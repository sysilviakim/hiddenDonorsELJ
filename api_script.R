## Record ======================================================================
zz <- file(
  file.path(paste0("output", cycle, "_", week, ".Rout")),
  open = "wt"
)
sink(zz, type = c("output", "message"), split = T)

## Download the first file. ====================================================
source("prep_download.R")
download.file(baseURL, file)

## Read the file. Update the index and last date. ==============================
baseRead <- paste(readLines(file, warn = F), collapse = "")
baseJSON <- fromJSON(baseRead, flatten = T)

## Additional pages, updating pagination, starts from 2 ========================
if (baseJSON$pagination$pages > 1) {
  if (startNumber == 1) {
    lastIndex[i] <- find_last_index(baseRead)
    lastDate[i] <- find_last_date(baseJSON)
    startNumber <- startNumber + 1
  }

  ### This is for precaution (updates, etc.)
  for (i in startNumber:((baseJSON$pagination$pages) * 100)) {
    file <- paste0(path, "/", "contrib-redo-", cycle, "-", i, ".txt")
    while (has_internet() == FALSE) {
      Sys.sleep(3)
      print(paste0("Internet is disconnected. Number is ", i))
    }
    addURL <- paste0(
      baseURL,
      "&last_index=", lastIndex[i - 1],
      "&last_contribution_receipt_date=", lastDate[i - 1]
    )

    ### Try downloading. Rest and try again if it fails
    tryCatch({
      download.file(addURL, file)
    }, error = function(e) {
      print(paste0("Error in ", i, " because ", e))
      Sys.sleep(90)
      download.file(addURL, file)
    })

    ### Read the JSON. We are not going to put it as a dataframe yet.
    addRead <- paste(readLines(file, warn = F), collapse = "")
    addJSON <- fromJSON(addRead, flatten = T)

    ### Is the dataframe empty? Or else, continue
    if (length(addJSON$results) == 0) {
      save(
        list = c("lastDate", "lastIndex", "i"),
        file = paste0(
          path, "/", "contribTemp", cycle, "-",
          str_pad(week, 3, pad = "0"), ".Rda"
        )
      )
      break
    } else {
      lastIndex[i] <- find_last_index(addRead)
      lastDate[i] <- find_last_date(addJSON)
      print(paste(i, lastIndex[i], lastDate[i]))
    }

    ### Save the lastIndex and lastDate
    if ((i %% 50) == 0) {
      save(
        list = c("lastDate", "lastIndex", "i"),
        file = paste0(
          path, "/", "contribTemp", cycle, "-",
          str_pad(week, 3, pad = "0"), ".Rda"
        )
      )
    }
    Sys.sleep(runif(1, 3, 10))
  }
}

gc(reset = T)
sink()