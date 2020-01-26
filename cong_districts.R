source("utility.R")

# Congressional Districts =====================================================
suppressWarnings(
  cd_sliced <- import_DB(
    pw = pw, db = db, tbl = "indiv_cont",
    varlist = c(
      "identifier", "contributor_zip", "county", "censusTract", "censusBlock",
      "latitude", "longitude"
    )
  ) %>%
    select(zip = contributor_zip, everything()) %>%
    mutate(
      stabb = substr(identifier, 1, 2),
      zip = ifelse(
        nchar(zip) < 5, str_pad(zip, width = 5, pad = "0", side = "left"), zip
      ),
      zip = ifelse(
        nchar(zip) < 6, str_pad(zip, width = 9, pad = "0", side = "right"), zip
      ),
      zip = str_pad(zip, width = 9, pad = "0", side = "left"),
      zip5 = as.integer(substr(zip, 1, 5)),
      county = as.integer(county),
      censusTract = as.integer(censusTract),
      censusBlock = as.integer(censusBlock)
    ) %>%
    ## This assumes a single district in this data/cycle, but this may not be so
    group_by(identifier, zip, county, censusTract, censusBlock) %>%
    slice(1) %>%
    ## If censusBlock is empty among entries for a single individual, drop
    group_by(identifier) %>%
    filter(
      !(max(ifelse(censusBlock != "", 1, 0), na.rm = TRUE) == 1 &
        censusBlock == "")
    ) %>%
    ## Mode
    filter(n() == max(n())) %>%
    ## If addresses with equal frequencies, take the latest address
    slice(n()) %>%
    left_join(., Kmisc::fips %>% select(stabb, stfips))
)

## Correct with block - CD crosswalk ===========================================
rel_block_cd <- read.table("data/National_CD114.txt", sep = ",") %>%
  filter(row_number() != 1) %>%
  set_colnames(c("blockid", "cd")) %>%
  mutate(
    stfips = as.integer(substr(blockid, 1, 2)),
    county = as.integer(substr(blockid, 3, 5)),
    censusTract = as.integer(substr(blockid, 6, 11)),
    censusBlock = as.integer(substr(blockid, 12, 15)),
    cd = as.integer(as.character(cd))
  )

## Correct with zip code - CD crosswalk (Census) but only when there's
## a clear correspondence
rel_zcta_cd <- read.table(
  "https://www2.census.gov/geo/relfiles/cdsld13/natl/natl_zccd_delim.txt",
  skip = 2, sep = ","
) %>%
  set_colnames(c("stfips", "zip5", "cd_census")) %>%
  mutate(
    stfips = as.integer(stfips),
    zip5 = as.integer(zip5),
    cd_census = as.integer(cd_census)
  ) %>%
  group_by(stfips, zip5) %>%
  filter(n() == 1)

## Correct with HUD USPS zip code crosswalk files ==============================
rel_hud_zip_cd <-
  readxl::read_xlsx(
    "data/ZIP_CD_122015.xlsx",
    col_types = rep("text", 6)
  ) %>%
  set_colnames(c("zip5", "stcd", "res", "bus", "oth", "tot")) %>%
  mutate(
    zip5 = as.integer(zip5),
    stfips = as.integer(substr(stcd, 1, 2)),
    cd_hud = as.integer(substr(stcd, 3, 4))
  ) %>%
  select(zip5, stfips, cd_hud) %>%
  group_by(stfips, zip5) %>%
  filter(n() == 1)

## Block to CD crosswalk takes priority if something differs ===================
cd_sliced %<>%
  left_join(., rel_block_cd) %>%
  mutate(
    cd = ifelse(grepl("^AK|^DE|^MT|^ND|^SD|^VT|^WY|^DC", identifier), 0, cd)
  ) %>%
  select(-blockid) %>%
  left_join(., rel_zcta_cd) %>%
  mutate(cd = ifelse(is.na(cd) & !is.na(cd_census), cd_census, cd)) %>%
  left_join(., rel_hud_zip_cd) %>%
  mutate(cd = ifelse(is.na(cd) & !is.na(cd_hud), cd_hud, cd)) %>%
  select(-cd_census, -cd_hud)

write.fst(cd_sliced, "output/temp/cd_sliced.fst")