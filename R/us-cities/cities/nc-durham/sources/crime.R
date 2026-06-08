# Workspace ----
library(tidyverse)
library(sf)
library(tmap)
setwd(dirname(.rs.api.getSourceEditorContext()$path))

relevant_areas <- 
  read_sf("input/census-boundaries/relevant-areas.shp") |> 
  st_set_crs(4326)

crime_lookup <- read_csv("input/crime/crime_category_lookup.csv")

d_crime <- 
  readxl::read_excel("input/crime/durham.xlsx") |> 
  janitor::clean_names()


r_crime <-
  read_sf("input/crime/raleigh.shp") |> 
  janitor::clean_names()

prep_durham <-
  d_crime |>
  st_as_sf(coords = c("x", "y"), crs = 2264) |> 
  st_transform(4326) |> 
  transmute(
    date = mdy(report_date),
    year = year(date),
    month = month(date),
    dow = wday(date),
    hour = 
      as.POSIXct(report_time, format = "%I:%M:%S%p") |> 
      format(format = "%H") |> 
      as.integer(),
    original_category = offense
  ) |> 
  filter(year >= 2025)


prep_raleigh <-
  r_crime |> 
  st_transform(2264) |> 
  st_transform(4326) |> 
  transmute(
    date = ymd(reported_d),
    year = year(date),
    month = reported_m,
    dow = wday(date),
    hour = reported_h,
    original_category = crime_cate
  )
  
all_crime <-
  bind_rows(
    durham = prep_durham,
    raleigh = prep_raleigh,
    .id = "county"
  ) |> 
  left_join(crime_lookup) |> 
  relocate(geometry, .after = everything())

relevant_crime <-
  all_crime |> 
  st_intersection(relevant_areas |> select(geometry))

saveRDS(relevant_crime, "cache-data/crime.Rds")

relevant_crime %>%
  mutate(
    x = st_coordinates(.)[,1],
    y  = st_coordinates(.)[,2]
  ) |> 
  st_drop_geometry() |> 
  write_csv("cache-data/crime.csv")

  
