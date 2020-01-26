## Setup path ==================================================================
path <- file.path(
  paste0("year", cycle), paste0("week", str_pad(week, 3, pad = "0"))
)

## Where should I start? =======================================================
temp_file <- paste0(
  path, "/", "contribTemp", cycle, "-", str_pad(week, 3, pad = "0"), ".Rda"
)
if (!file.exists(temp_file)) {
  load("start.Rda")
  i <- startNumber
} else {
  load(temp_file)
  startNumber <- i
}

## Setup file and url ==========================================================
### lastdates  <- seq(as.Date(paste0(cycle - 1, "-02-01")),
###   length = 24, by = "1 month"
### ) - 1
### firstdates <- seq(as.Date(paste0(cycle - 1, "-01-01")),
###   length = 24, by = "1 month"
### )
first_dates <- seq(
  as.Date(paste0(cycle - 1, "-01-01")), 
  as.Date(paste0(cycle + 1, "-01-01")), 
  by = 7
)
last_dates <- seq(
  as.Date(paste0(cycle - 1, "-01-01")), 
  as.Date(paste0(cycle + 1, "-01-01")), 
  by = 7
) + 6
last_dates[length(last_dates)] <- as.Date(paste0(cycle + 1, "-01-01")) - 1

baseURL <-
  paste0(
    "https://api.open.fec.gov/v1/schedules/schedule_a/",
    "?contributor_type=individual&contributor_type=committee&",
    "&api_key=", eval(parse(text = paste0("key", week %% 12))),
    "&per_page=100&", "sort=contribution_receipt_date",
    "&min_date=", first_dates[week],
    "&max_date=", last_dates[week],
    "&two_year_transaction_period=", cycle
  )
file <- paste0(path, "/", "contrib-redo-", cycle, "-", i, ".txt")