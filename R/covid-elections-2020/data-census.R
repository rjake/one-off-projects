# from project directory
setwd("covid-elections-2020/")

library(tidyverse)
library(tidycensus)

# uses most recent year (2019)
raw_state_pop <- get_estimates(geography = "state", variables = "POP")
raw_county_pop <- get_estimates(geography = "county", variables = "POP")

# update variable names
state_pop <-
  raw_state_pop |>
  transmute(
    state_fips = GEOID,
    state_name = NAME,
    state_pop = value
  )

county_pop <-
  raw_county_pop |>
  transmute(
    county_fips = GEOID,
    county_name = NAME,
    county_pop = value,
    state_fips = str_sub(GEOID, 1, 2)
  ) |>
  left_join(state_pop |> select(-state_pop)) |>
  select(-state_fips)

# save to csv
write_csv(state_pop, "covid-elections-2020/2019_state_population.csv")
write_csv(county_pop, "covid-elections-2020/2019_county_population.csv")
