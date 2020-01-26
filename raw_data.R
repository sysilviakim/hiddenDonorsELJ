source("utility.R")

# Raw Data Import (pw and db name redacted) ====================================
raw_df <- import_DB(
  pw = pw, db = db, tbl = "indiv_cont",
  varlist = c(
    "identifier", "committee_final", "committee_intermediary",
    "contribution_receipt_date", "contribution_receipt_amount",
    "contributor_aggregate_ytd"
  )
)
raw_df$cycle <- 2016

raw_df <- cmte_track(
  raw_df, cmte_var = "committee_final", cmte_ID = "C00577130",
  indiv_ID = "identifier", label = "bernie", filter = TRUE
) %>%
  date_vars(df = ., indiv_ID = "identifier") %>%
  mutate(stabb = substr(identifier, 1, 2))

# Committee and Candidate Information ==========================================
cmte_master <- import_DB(pw = pw, db = db, tbl = "committee_master")
cand_master <- import_DB(pw = pw, db = db, tbl = "candidate_master")
data("battle_house_2016")
data("battle_senate_2016")

cmte_master <- cmte_subset(
  df = cmte_master, cmte_var = "committeeID", year_input = 2016
) %>%
  select(committee_final = committeeID, committee_design = cmteDesign)

## Some duplicates (54 instances in 2016)
## Checked each one: same people filing twice even when cand ID is different
## e.g. H6IL18153 and H4IL18109, Robert Mellon
## Exceptions are C00576603 and C00512343 (both presidential and ignorable)
cand_master <- cand_subset(
  df = cand_master %>% filter(year > 2010 & year < 2024),
  cand_var = "candID", cmte_var = "candPCC", year_var = "year",
  year_input = 2016, recent = TRUE, office_var = "candOffice"
) %>%
  select(
    committee_final = candPCC, candidate_id = candID, stabb, cd,
    office = candOffice, incumbent = candIncumbent,
    party, party_dem, party_rep, special = specialElection
  ) %>%
  filter(
    ## Exceptions to the arrange -> slice (checked manually)
    !(committee_final == "C00624239" & office == "H") &
      !(committee_final == "C00515601" & office == "S") &
      !(committee_final == "C00457960" & office == "S") &
      !(committee_final == "C00451146" & office == "S") &
      !(committee_final == "C00542068" & office == "S") &
      !(committee_final == "C00613497" & office == "H" & cd == 3)
  ) %>%
  slice(1) %>%
  arrange(office, stabb, cd, party, committee_final) %>%
  ## Since this is only one cycle
  mutate(
    battle_house = ifelse(candidate_id %in% battle_house_2016, 1, 0),
    battle_senate = ifelse(candidate_id %in% battle_senate_2016, 1, 0)
  )

write.fst(raw_df,      "output/temp/raw_df.fst")
write.fst(cmte_master, "output/cmte_master.fst")
write.fst(cand_master, "output/cand_master.fst")
