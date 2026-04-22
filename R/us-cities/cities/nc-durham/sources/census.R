library(tidyverse)
library(tidycensus)
setwd(dirname(.rs.api.getSourceEditorContext()$path))

acs_vars <- load_variables(2023, "acs5", cache = TRUE)
table(acs_vars$geography)

use_vars <-
  c(
    income = "B02001_003", 
    pop = "B02001_001",
    poverty_denominator = "C17002_001",
    poverty_0.00_0.50 = "C17002_002",
    poverty_0.50_0.99 = "C17002_003",
    poverty_1.00_1.24 = "C17002_004",
    poverty_1.25_1.49 = "C17002_005",
    poverty_1.50_1.84 = "C17002_006",
    poverty_1.85_1.99 = "C17002_007",
    poverty_2.00_3.00 = "C17002_008" # really 2.00+
  
    # white = "B02001_002",
    # poverty_denom = "B06011_001",
    # poverty_000_099 = "B06012_002",
    # poverty_100_149 = "B06012_003",
    # poverty_150_above = "B06012_004",
    # income_avg = "B19013_001",
    # income_b = "B19013B_001",
    # income_a = "B19013C_001",
    # income_p = "B19013D_001",
    # income_o = "B19013E_001",
    # income_s = "B19013F_001",
    # income_t = "B19013G_001",
    # income_w = "B19013H_001",
    # income_h = "B19013I_001","B19049_001", #
  )  

raw_acs <-
  get_acs(
    geography = "block group", 
    variables = use_vars,
    state = 37, 
    county = 063,
    cache_table = TRUE
  ) |> 
  rename_all(tolower)

saveRDS(raw_acs, "cache-data/census-poverty-raw.Rds")
raw_acs <- readRDS("cache-data/census-poverty-raw.Rds")
  
poverty_avg <-
  use_vars |> 
  as_tibble(rownames = "variable") |> 
  filter(
    str_detect(variable, "pov.*\\d")
  ) |> 
  mutate(
    min = str_extract(variable, "\\d\\.\\d+") |> as.numeric(),
    max = str_extract(variable, "\\d\\.\\d+$") |> as.numeric(),
    mid = (min + max) / 2
  )


census_poverty <-
  raw_acs |> 
  inner_join(poverty_avg |> select(variable, mid)) |> 
  summarise(
    .by = geoid,
    total_avg = sum(mid * estimate),
    total = sum(estimate),
    estimate = total_avg / total
  )

write_csv(census_poverty, "output/census-poverty.csv")

# race
acs_demo_vars <-
  list(
    race_total =  "B03002_001",
    race_white =  "B03002_003",
    race_black =  "B03002_004",
    race_asian =  "B03002_006",
    race_latino = "B03002_012",
    edu_09 = "B29002_002",
    edu_11 = "B29002_003",
    edu_12 = "B29002_004",
    edu_13 = "B29002_005",
    edu_14 = "B29002_006",
    edu_16 = "B29002_007",
    edu_18 = "B29002_008"
  )


census_prior <-
  get_acs(
    geography = "block group", 
    variables = acs_demo_vars,
    year = 2020,
    state = 37, 
    county = 063,
    cache_table = TRUE
  ) |> 
  rename_all(tolower)

census_current <-
  get_acs(
    geography = "block group", 
    variables = acs_demo_vars,
    year = 2024,
    state = 37, 
    county = 063,
    cache_table = TRUE
  ) |> 
  rename_all(tolower)

census_together <-
  bind_rows(
    "prior" = census_prior,
    "current" = census_current,
    .id = "year"
  ) |>
  select(-c(name, moe)) |> 
  separate_wider_delim(
    variable,
    delim = "_",
    names = c("demo", "variable")
  )

census_race_prep <-
  census_together |>
  filter(demo == "race") |> 
  #filter(geoid == "370630013042") |> 
  select(-demo) |> 
  filter(variable %in% c("white", "black")) |> 
  pivot_wider(
    names_from = year,
    values_from = estimate
  ) |>
  mutate(
    shift = current - prior
  ) |> 
  select(-c(prior)) |> 
  pivot_wider(
    names_from = variable,
    values_from = c(shift, current)
  )
  
census_edu_prep <-
  census_together |>
  filter(demo == "edu") |> 
  select(-demo) |>
  #filter(geoid == "370630014001") |> 
  mutate(
    year = ifelse(year == max(year), "current", "prior"),
    max_edu = as.integer(variable),
    #max_edu = ifelse(max_edu <= 12, 12, 18),
    gt_hs_ind = as.integer(max_edu > 12)
  ) |> 
  count(year, geoid, gt_hs_ind, wt = estimate) |> 
  mutate(
    .by = c(year, geoid),
    pct = round(n / sum(n) * 100)
  ) |> 
  filter(gt_hs_ind == 1) |> 
  select(-c(gt_hs_ind, n)) |> 
  pivot_wider(
    names_from = year,
    values_from = pct
  ) |> 
  mutate(
    .keep = "unused",
    shift_pct_degree = current - prior,
    current_pct_degree = current,
    prior_pct_degree = prior
  )

fill_scale <-
  list(
    c = RColorBrewer::brewer.pal(4, "PuBu"),
    d = RColorBrewer::brewer.pal(4, "RdBu")
  )

library(mapview)

block_groups |> 
  inner_join(census_edu_prep) |> 
  inner_join(census_race_prep) |> 
  mutate(
    white_pct = round(current_white / (current_black + current_white) * 100),
    black_pct = round(current_black / (current_black + current_white) * 100),
    gentrifying = as.integer(shift_white > 20 & shift_black < -50),
    gentrification_shift = ifelse(gentrifying == 1, shift_white + abs(shift_black), 0)
  ) |> 
  relocate(geometry, .after = everything()) |> 
  # mapview(zcol = "shift_pct_degree", layer.name = "> hs degree", col.regions= RColorBrewer::brewer.pal(4, "RdBu"), alpha.regions = 0.25)
  #mapview(zcol = "gentrification_shift", layer.name = "g") +#, col.regions= RColorBrewer::brewer.pal(4, "RdBu"), alpha.regions = 0.25)
  mapview(zcol = "black_pct", layer.name = "pct", alpha.regions = 0.8, col.regions = fill_scale$c) +
  #mapview(zcol = "white_pct", layer.name = "pct", alpha.regions = 0.8, col.regions = fill_scale$c) +
  mapview(
    points_of_interest,
    color = "black", 
    alpha.regions = 1,
    col.regions = "orange"
  )

census_demo <-
  census_poverty |> 
  left_join(census_edu_prep) |> 
  left_join(census_race_prep) 

write_csv(census_demo, "output/census-demo.csv")
