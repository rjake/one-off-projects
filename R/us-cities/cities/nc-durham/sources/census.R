# workspace ----
library(tidyverse)
library(tidycensus)
library(sf)

setwd(dirname(.rs.api.getSourceEditorContext()$path))
options(tigris_use_cache = TRUE)

acs_vars <- load_variables(2023, "acs5", cache = TRUE)
decenial_vars <- load_variables(2020, "pl", cache = TRUE)
decenial_vars_sf <- load_variables(2020, "sf1", cache = TRUE)
tidycensus::acs5_geography |> view()

use_counties <- c(63)#, 135, 183) # 63 durham, 135 orange, 183 raleigh

## block - race ----
census_blocks <- 
  get_decennial(
    geography = "block",
    year = 2020,
    variables = 
      c(
        total_pop = "P1_001N",
        race_white = "P1_003N",
        race_black = "P1_004N"
      ), 
    state = 37,
    county = use_counties,
    cache_table = TRUE,
    geometry = TRUE
  ) |> 
  rename_all(tolower)

# define variables to fetch
race_vars <- c(
  latino = "P2_002N",
  white  = "P2_005N",
  black  = "P2_006N",
  asian  = "P2_008N",
  total  = "P2_001N"
)

# fetch data for each county
## * raw_race ----
raw_race <- local({
  if ("raw_race" %in% ls(envir = globalenv())) return(raw_race)
  
  counties <- 
    census_blocks$name |> 
    str_extract("(?<=, )[^,]+(?= County)") |> 
    unique()
  
  res <- map_dfr(
    counties,
    ~get_decennial(
      geography = "block",
      variables = race_vars,
      year = 2020,
      state = "NC",
      county = .x,
      summary_var = "P2_001N",
      cache_table = TRUE
    )
  )
  
  res |>  
    janitor::clean_names()
})

## * prep_race ----
prep_race <-
  raw_race |>
  select(-name) |> 
  # filter(geoid == "370630001011024") |>
  rename(total_pop = summary_value) |> 
  filter(total_pop > 0) |> 
  mutate(
    block_geoid = geoid,
    geoid = substr(geoid, 1, 12),
    block_id = substr(block_geoid, 13, 15) |> as.integer(),
    # convert "total" to "other"
    value = ifelse(variable == "total", 0, value),
    variable = ifelse(variable == "total", "other", variable),
  ) |> 
  mutate(
    .by = block_geoid,
    value = ifelse(variable == "other", total_pop - sum(value), value),
    biggest_group = ifelse(value == max(value), variable, NA) |> max(na.rm = TRUE),
    biggest_pct = ifelse(variable == biggest_group, round(value/total_pop * 100), NA) |> max(na.rm = TRUE)
  ) |> 
  pivot_wider(names_from = variable, values_from = value)|>
  mutate(
    across(c(latino, white, black, asian, other), ~ round(.x / total_pop * 100), .names = "pct_{.col}")
  ) |> 
  relocate(block_geoid, geoid, block_id)

prep_race |>  
  write_csv("output/census-race-blocks.csv")

## census_blocks ----
census_blocks |> 
  st_transform(4326) |> 
  transmute(
    block_geoid = geoid,
    geoid = substr(block_geoid, 1, 12),
    block_id = substr(block_geoid, 13, 15) |> as.integer(),
    geometry
  ) |> 
  distinct() |> 
  saveRDS("output/sf-blocks.Rds")



poverty_vars <-
  c(
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
    variables = poverty_vars,
    state = 37, 
    county = use_counties,
    cache_table = TRUE
  ) |> 
  rename_all(tolower)

saveRDS(raw_acs, "output/census-poverty-raw.Rds")
raw_acs <- readRDS("output/census-poverty-raw.Rds")
  
poverty_avg <-
  poverty_vars |> 
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
    est_poverty_ratio = total_avg / total
  ) |> 
  select(geoid, est_poverty_ratio)

write_csv(census_poverty, "output/census-poverty.csv")

## block_group - income ----
income_vars <- 
  c(
    #avg_income = "B06011_001",
    med_hh_income = "B19013_001",
    med_nf_income = "B19202_001",
    med_earing = ""
  )

raw_income <-
  get_acs(
    geography = "block group", 
    variables = income_vars,
    state = 37, 
    county = use_counties,
    cache_table = TRUE,
    geometry = TRUE
  ) |> 
  rename_all(tolower) |> 
  select(geoid, variable, estimate)

census_income <-
  raw_income |> 
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) |> 
  relocate(geometry, .after = everything())

census_income |> view()

census_income |> skimr::skim()

census_income |> 
  ggplot(aes(med_hh_income, med_nf_income)) + 
  geom_point() + 
  geom_smooth(method =  "lm") + 
  geom_abline()

write_csv(census_income, "output/census-income.csv")

## block_group - gentrifcation & education ----
acs_demo_vars <-
  list(
    race_total = "B03002_001",
    race_white = "B03002_003",
    race_black = "B03002_004",
    race_asian = "B03002_006",
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
    county = use_counties,
    cache_table = TRUE
  ) |> 
  rename_all(tolower)

census_current <-
  get_acs(
    geography = "block group", 
    variables = acs_demo_vars,
    year = 2024,
    state = 37, 
    county = use_counties,
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

census_demo <-
  census_poverty |> 
  left_join(census_income) |> 
  left_join(census_edu_prep) |> 
  left_join(census_race_prep) |> 
  relocate(geometry, .after = everything())


census_demo |> 
  st_as_sf() |> 
  mapview::mapview(zcol = "current_pct_degree", layer.name = "> hs degree", col.regions= RColorBrewer::brewer.pal(4, "RdBu"), alpha.regions = 0.25)


census_demo |> 
  st_drop_geometry() |> 
  write_csv("output/census-demo.csv")


library(mapview)

fill_scale <-
  list(
    c = RColorBrewer::brewer.pal(4, "PuBu"),
    d = RColorBrewer::brewer.pal(4, "RdBu")
  )

block_groups |> 
  inner_join(census_income) |>
  mutate(
    med_hh_income = med_hh_income > 70000#((med_hh_income %/% 60000) + 1) * 60000
  ) |> 
  ggplot() +
  geom_sf(
    aes(fill = as.factor(med_hh_income))
  )
+
  scale_fill_viridis_b(option = "E")
mapview(zcol = "med_hh_income", layer.name = "med_hh_income")
             
census_income |> 
  inner_join(census_edu_prep) |> 
  inner_join(census_race_prep) |>
  inner_join(census_poverty) |> 
  mutate(
    white_pct = round(current_white / (current_black + current_white) * 100),
    black_pct = round(current_black / (current_black + current_white) * 100),
    gentrifying = as.integer(shift_white > 20 & shift_black < -50),
    gentrification_shift = ifelse(gentrifying == 1, shift_white + abs(shift_black), 0)
  ) |> 
  relocate(geometry, .after = everything()) |> 
  # mapview(zcol = "shift_pct_degree", layer.name = "> hs degree", col.regions= RColorBrewer::brewer.pal(4, "RdBu"), alpha.regions = 0.25)
  #mapview(zcol = "gentrification_shift", layer.name = "g") +#, col.regions= RColorBrewer::brewer.pal(4, "RdBu"), alpha.regions = 0.25)
  mapview(zcol = "black_pct", layer.name = "pct", alpha.regions = 0.8, col.regions = fill_scale$c)

