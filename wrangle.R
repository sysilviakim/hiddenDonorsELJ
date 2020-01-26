source("utility.R")
raw_df <- fst::read.fst("output/temp/raw_df.fst")
raw_df$bernie_cont <- ifelse(raw_df$committee_final == "C00577130", 1, 0)
raw_df$contribution_receipt_date <- as.Date(raw_df$contribution_receipt_date)
cmte_master <- fst::read.fst("output/cmte_master.fst")
cand_master <- fst::read.fst("output/cand_master.fst")

# Demographics, Unrelated to Contribution Records ==============================
demo_sliced <- import_DB(
  pw = pw, db = db, tbl = "indiv_cont",
  varlist = c(
    "identifier", "gender", "race", "contributor_occupation",
    "contribution_receipt_date"
  ) 
) %>%
  mutate(
    gender = ifelse(!(gender %in% c("male", "female")), "unknown", gender),
    race = ifelse(
      !(race %in% c("white", "black", "asian", "hispanic")), "unknown", race
    )
  ) %>%
  select(identifier, occ = contributor_occupation, everything())

demo_sliced <- left_join(
  x = demo_sliced %>% select(-contribution_receipt_date, -occ),
  y = occ_classify(demo_sliced, filter = TRUE)
) %>%
  group_by(identifier, cycle) %>%
  slice(1)

fst::write.fst(demo_sliced, "output/temp/demo.fst")

# Sliced Datasets with Wrangled Data ===========================================
vis_sliced <- amount_total(
  df = raw_df, cmte_df = cmte_master, cmte_labels = "bernie"
)
fst::write.fst(vis_sliced, "output/temp/vis.fst")

## Averages and Sums ===========================================================
avg_sliced <- avg_vars(df = raw_df, cmte_labels = "bernie", filter = TRUE)
fst::write.fst(avg_sliced, "output/temp/avg_sliced.fst")

# Entry and Exit Dates =========================================================
date_sliced <- date_vars(df = raw_df, filter = TRUE) %>%
  mutate(
    presumptive = ifelse(
      exit_date > as.Date("2016-06-06") & cycle == 2016, 1, 0
    ),
    clinton_endorse = ifelse(
      exit_date > as.Date("2016-07-12") & cycle == 2016, 1, 0
    ),
    end_convention = ifelse(
      exit_date > as.Date("2016-07-28") & cycle == 2016, 1, 0
    )
  )
fst::write.fst(date_sliced, "output/temp/date.fst")

## Threshold-Crossing Dates ====================================================
## cumulative_pre_subset <- return = "else"
threshdate_sliced <- cumsum_vars(df = raw_df) %>%
  ungroup() %>%
  filter(committee_final == "C00577130") %>%
  select(identifier, cycle, cross_date_200_bernie = cross_date_200)
fst::write.fst(threshdate_sliced, "output/temp/threshdate_sliced.fst")

## First Contribution to Whom? =================================================
bernie_first <- first_cmtes(raw_df, varlist = "bernie", filter = TRUE)

## Party Contributions =========================================================
party_sliced <- party_track(df = raw_df, filter = TRUE)

## Congressional Contributions =================================================
congress_sliced <- congress_track(
  df = raw_df, cand_df = cand_master, filter = TRUE
)

## Out-District Contributions ==================================================
cd_sliced <- fst::read.fst("output/temp/cd_sliced.fst")
out_sliced <- out_close_track(
  df = left_join(raw_df, cd_sliced),
  cand_df = cand_master %>%
    select(contributor_state = stabb, committee_cd = cd, everything()),
  cmte_var = "committee_final", indiv_ID = "identifier", filter = TRUE
)

## Presidential Contributions
pres2016_sliced <- pres2016_track(df = raw_df, filter = TRUE)

## Amounts to Specific Targets
amount_sliced <- amount_target(
  df = cumsum_vars(df = raw_df, return = "else") %>%
    select(-bernie_cont),
  cand_df = cand_master %>%
    select(contributor_state = stabb, committee_cd = cd, everything()),
  cd_df = cd_sliced, filter = TRUE
)

## Altogether, individual-level
main <- left_join(date_sliced, cd_sliced) %>%
  left_join(., avg_sliced) %>%
  left_join(., party_sliced) %>%
  left_join(., pres2016_sliced) %>%
  left_join(., congress_sliced) %>%
  left_join(., out_sliced) %>%
  left_join(., demo_sliced) %>%
  left_join(., vis_sliced) %>%
  left_join(., amount_sliced) %>%
  left_join(., threshdate_sliced) 

main$time_cross <- main$cross_date_200_bernie - main$bernie_entry_date
main$vis_group_3 <- 
  if_else(main$time_cross == 0, main$bernie_vis_200 + 1, main$bernie_vis_200)
main$vis_group_3 <- if_else(is.na(main$vis_group_3), 0, main$vis_group_3)
main$vis_group_3 <- factor(
    main$vis_group_3, levels = c(2, 1, 0),
    labels = c("Immediately Visible", "Eventually Visible", "Hidden")
  )
fst::write.fst(main, "output/all_sliced.fst")

main <- main[main$bernie_contributor == 1, ]
fst::write.fst(main, "output/main_sliced.fst")
