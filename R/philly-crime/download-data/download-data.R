setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)
library(jsonlite)
library(glue)
library(tidycensus)

# json example
{'
  "cartodb_id": 1,
  "the_geom": "0101000020E610000017429450F6D052C00C1997C3D1FC4340",
  "the_geom_webmercator": "0101000020110F0000A3F89A4F18F65FC17B0D2F015F8C5241",
  "objectid": 113,
  "dc_dist": "19",
  "psa": "1",
  "dispatch_date_time": "2011-09-27T10:41:00Z",
  "dispatch_date": "2011-09-27",
  "dispatch_time": "06:41:00",
  "hour": 6,
  "dc_key": 201119085906,
  "location_block": "7500 BLOCK WOODCREST AV",
  "ucr_general": "600",
  "text_general_code": "Theft from Vehicle",
  "point_x": -75.26503386,
  "point_y": 39.97515149
'}


# crime data -----
query_fomatted <- {
  glue(
    "https://phl.carto.com/api/v2/sql?q=select
       --*
      dc_key, 
      dispatch_date, 
      hour, 
      dc_dist as district, 
      psa as service_area, 
      text_general_code, 
      point_x, 
      point_y
    from incidents_part1_part2
    where 
      dispatch_date > '2013-01-01'
      and psa not in ('A', 'R', 'S')"
  )
}

query_prep <-
  query_fomatted |> 
  str_split("\n") |> 
  flatten_chr() |> 
  str_remove_all("--.*") |> 
  trimws() |>
  discard(~.x == "") |> 
  glue_collapse(" ") |> 
  print()

system.time({
  raw_data <- # ~12 min
    query_prep |> 
    URLencode() |>
    fromJSON()
})

prep_data <-
  raw_data$rows |> 
  # head() |>
  as_tibble() |> 
  mutate(
    district = paste0("D-", district),
    service_area = paste0("PSA-", service_area),
    dispatch_date = as.Date(dispatch_date)
  ) |> 
  print()

saveRDS(prep_data, "crime-data.Rds")
write_csv(prep_data, "crime-data.csv")

# census data -----
tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"))

census_data <-
  tidycensus::get_acs(
    geography = "tract",
    variables = "P1_001N",
    state = "PA",
    county = "Philadelphia",
    year = 2012
  )

vars13 <- tidycensus::load_variables(year = 2013, "acs5", cache = TRUE)
vars20 <- tidycensus::load_variables(year = 2020, "acs5", cache = TRUE)

all_vars <-
  bind_rows(
    `2013` = vars13, 
    `2023` = vars20, 
    .id = "year"
  ) |> 
  filter(geography == "tract") |> 
  select(-geography) |> 
  group_by(name) |> 
  filter(n() == 2) |> # ensure it exists in both periods
  slice_max(year) |> # keep 2023
  ungroup()


vars_list <- 
  list(
    B19013_001 = "median income"
  )
use_vars <-
  all_vars |> 
  filter(
    name == "BO1"
    
  )



# 39.952047, -75.210691

query_fomatted <- {
  glue(
    "https://phl.carto.com/api/v2/sql?q=select
       --*
      dc_key, 
      dispatch_date, 
      hour, 
      dc_dist as district, 
      psa as service_area, 
      text_general_code, 
      point_x, 
      point_y
    from incidents_part1_part2
    where 
      dispatch_date = '2013-08-11'
      and point_x between -75.214 and -75.209
      and point_y between 39.950 and 39.953"
  )
}
