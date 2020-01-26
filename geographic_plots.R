source("utility.R")

# Geographic Plots
## Zip-code level data from the IRS ============================================
zipcode_stats <- 
  read_csv(
    "data/15zpallnoagi.csv", 
    col_names = TRUE, quote = "", comment = "", trim_ws = TRUE
  ) %>%
  select(
    state_fips = STATEFIPS,
    stabb      = STATE,
    zip5 = ZIPCODE,
    n1 = N1,              # Number of returns
    agi = A00100,         # Adjus gross income (AGI)
    total_income = A02650 # Total income amount
  ) %>%
  mutate(
    agi_avg = agi / n1,
    inc_avg = total_income / n1,
    zip5 = as.integer(zip5)
  ) %>%
  select(-state_fips)

library(choroplethr)
## githubinstall::gh_install_packages("choroplethrZip", ref = github_pull("8"))
library(choroplethrZip) 

## 2012-2016 American Community Survey 5-Year Estimates, Total Population
df_pop_zip <- 
  read_csv(
    "./data/ACS_16_5YR_S0101_zcta_pop.csv",
    col_names = TRUE, quote = "", comment = "", trim_ws = TRUE, skip = 1
  ) %>%
  select(
    region = Id2, value = `Total; Estimate; Total population`
  )

## Zip-code and state-level summary ============================================
main %<>%
  left_join(., zipcode_stats) %>%
  left_join(
    x = ({.} %>%
      select(region = zip5, everything()) %>%
      mutate(
        region = stringr::str_pad(
          region,
          width = 5, side = "left", pad = "0"
        )
      )
    ),
    y = df_pop_zip,
    by = NULL
  )
save(main, file = "output/reg.Rda")

sanders_by_zip <- temp %>% 
  filter(!is.na(n1)) %>%
  group_by(region) %>%
  geo_summ(.)

sanders_by_state <- temp %>%
  select(-region) %>%
  left_join(
    x = ({.} %>% select(region = stabb, everything())), 
    y = (Kmisc::fips %>% select(region = stabb, everything())),
    by = NULL
  ) %>% 
  filter(!is.na(n1)) %>%
  group_by(region) %>%
  geo_summ(.)

save(
  list = c("sanders_by_zip", "sanders_by_state"),
  file = "./output/geo_summary.Rda"
)

## How does the donor population itself overall compare to the population? =====
df_pop_all_compare <- 
  left_join(
    df_pop_zip, sanders_by_zip %>% select(region, all_donors, pop, n1)
  ) %>%
  rowwise() %>%
  mutate(value = all_donors / max(pop, n1) * 100) %>%
  select(region, value) %>%
  mutate(original = ifelse(is.na(value), 0, value))
df_pop_all_compare$value <- Hmisc::cut2(
  df_pop_all_compare$original, 
  cuts = c(seq(0, 1.8, 0.3), 100)
)

## Indebted to https://github.com/arilamstein/choroplethrZip/pull/8
pal <- 
  c('#ffffff','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
us <- map_data('state')
choro1 <- ZipChoropleth$new(df_pop_all_compare)
## choro1$title <- 
##   "Percentage of Sanders Donors Against Population, Zip Code Level"
choro1$inset_outline <- "gray"
choro1$ggplot_scale <- scale_fill_manual(name = "(%)", values = pal)
p <- choro1$render_nationwide() +
  scale_fill_manual(name = "(%)", values = pal) +
  geom_polygon(
    data = us, aes(x = long, y = lat, group = group), 
    color = "gray", fill = NA, alpha = .35
  )

pdf("figures/pop_all_donors_comparison.pdf", width = 7, height = 3.8)
Kmisc::plot_nogrid(Kmisc::pdf_default(p))
dev.off()

## How does visible donors compare to invisibles? ==============================
df_pop_visible <- 
  left_join(
    df_pop_zip,
    sanders_by_zip %>% 
      mutate(
        ## Because this has lot more variance than the first graph,
        ## mute zip codes where there are five or less donors to begin with
        vis_mean = ifelse(all_donors < 6, 0, vis_mean)
      ) %>%
      select(region, vis_mean)
  ) %>%
  mutate(vis_mean = vis_mean * 100) %>%
  select(region, value = vis_mean) %>%
  mutate(original = ifelse(is.na(value), 0, value))
df_pop_visible$value <- Hmisc::cut2(
  df_pop_visible$original, 
  cuts = c(seq(0, 30, 5), 100))

choro2 <- ZipChoropleth$new(df_pop_visible)
## choro2$title <- 
##   "Percentage of Visible Donors Within Donors, Zip Code Level"
choro2$inset_outline <- "gray"
choro2$ggplot_scale <- scale_fill_manual(name = "(%)", values = pal)
p2 <- choro2$render_nationwide() +
  scale_fill_manual(name = "(%)", values = pal) +
  geom_polygon(
    data = us, aes(x = long, y = lat, group = group), 
    color = "gray", fill = NA, alpha = .35
  )

pdf("figures/pop_visible_comparison.pdf", width = 7, height = 3.8)
Kmisc::plot_nogrid(Kmisc::pdf_default(p2))
dev.off()

zip_state <- 
  readxl::read_xlsx(
    "data/ZIP_COUNTY_092016.xlsx", col_names = TRUE, trim_ws = TRUE
  ) %>%
  mutate(stfips = as.integer(substr(COUNTY, 1, 2))) %>%
  select(region = ZIP, stfips) %>%
  left_join(., Kmisc::fips)

top_donor_zip <- 
  left_join(
    df_pop_all_compare %>% filter(original >= 5),
    zip_state
  ) %>%
  arrange(desc(original))

sort(
  round(prop.table(table(top_donor_zip[1:100, ]$stabb)) * 100, digits = 1), 
  decreasing = TRUE
)