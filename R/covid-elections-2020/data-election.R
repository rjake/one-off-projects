# from project directory
setwd("covid-elections-2020/")

library(tidyverse)

raw_election <-
  "https://raw.githubusercontent.com/kjhealy/us_elections_2020_csv/master/results_current.csv" |>
  data.table::fread(drop = "id") |>
  as_tibble()


prep_elections <-
  raw_election|>
  filter(race == "President") |>
  # re-code DC
  mutate(fips5 = ifelse(fips_char == 11, 11001, fips5)) |>
  # select & transform
  transmute(
    state_fips = str_pad(fips_char, 2, "left", "0"),
    county_fips =
      str_pad(fips5, 5, "left", "0") |>
      replace_na("total"),
    county_name = place,
    party = ifelse(pab == "O", "other", tolower(party)),
    votes
  ) |>
  filter(votes > 0)


elections_wide <-
  prep_elections |>
  count(state_fips, county_fips, party, wt = votes) |>
  pivot_wider(
    names_from = party,
    values_from = n,
    values_fill = 0
  ) |>
  rename(gop = rep) |>
  mutate(
    votes = dem + gop + other,
    pct_dem = dem / votes * 100,
    pct_gop = gop / votes * 100,
    pct_other = other / votes * 100,
    pct_non_dem = 100 - pct_dem
  ) |>
  print()


county_election <-
  elections_wide |>
  # only keep total for Alaska, otherwise non-state total records
  filter(
    (state_fips == "02" & county_fips == "total")
    | (state_fips != "02" & county_fips != "total")
  )


state_election <-
  elections_wide |>
  filter(county_fips == "total" | state_fips == 11) |>
  select(-county_fips)


write_csv(county_election, "2020_county_elections.csv")
write_csv(state_election, "2020_state_elections.csv")
