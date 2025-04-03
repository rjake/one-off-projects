# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(sf)
library(geosphere)

# Raw data ----
# census
census_county <- read_csv("sources/output/census-county-2024.csv")
census_subcounty <- read_csv("sources/output/census-subcounty-2024.csv")

# geo spatial
geo_climbing_gyms <- 
  read_csv("sources/output/climbing-gyms.csv") |> 
  st_as_sf(coords = c("x", "y"), crs = 4326, remove  = FALSE) |> 
  rename(
    gym_x = x,
    gym_y = y
  )

geo_routes <- 
  read_csv("sources/output/mountain-project-popular-routes.csv") |> 
  st_as_sf(coords = c("x", "y"), crs = 4326, remove  = FALSE)

geo_subcounty <-
  read_csv("sources/output/subcounty-geo.csv") |> 
  st_as_sf(coords = c("x", "y"), crs = 4326, remove  = FALSE)

# subcounty_buffer <- 
#   geo_subcounty |> 
#   st_buffer(
#     dist = units::set_units(200, "miles")
#   )
  
# other
info_elections <- 
  read_csv("sources/output/elections-2024.csv") |> 
  select(
    county_fips, 
    pct_dem = per_dem
  )

info_weather <- 
  read_csv("sources/output/weather-2023.csv") |> 
  mutate(
    county_fips = str_pad(county_fips, 5, "left", "0")
  ) |> 
  select(county_fips:avg_daytime_temp)


# Prepare data ----
county_info <- 
  census_county |> 
  select(
    county_fips, 
    county_name,
    n_population,
    pct_poverty,
    n_lgbt,
    pct_lgbt,
    pct_price_150_299K
  ) |> 
  mutate(
    .before = everything(),
    county_fips, 
    state_name = str_extract(county_name, "(?<=, ).*"), 
    county_name = str_remove(county_name, " County,.*")
  ) |> 
  left_join(info_elections) |> 
  left_join(info_weather)

write_csv(county_info, "county-info.csv")


subcounty_info <- 
  census_subcounty |> 
  select(
    subcounty_fips, 
    n_population,
    pct_poverty,
    n_lgbt,
    pct_lgbt,
    pct_price_150_299K
  ) |> 
  left_join(
    geo_subcounty |> st_drop_geometry()
  ) |> 
  relocate(
    county_fips, 
    state_name, 
    county_name, 
    subcounty_name, 
    .after = subcounty_fips
  ) |> 
  left_join(info_elections) |> 
  left_join(info_weather)


# Geo joins
subcounty_gym <-
  geo_subcounty |>
  # filter(county_fips == "42101") |>
  st_join(geo_climbing_gyms, join = st_nearest_feature)

  

find_within_n_miles <- function(x, y, n) {
  st_is_within_distance(
    x = x, 
    y = y, 
    dist = units::set_units(n, "miles")
  ) |> 
    lengths()
}


subcounty_climbing <-
  geo_subcounty %>%
  #filter(county_fips == "42101") %>%
  mutate(
    n_gym_10mi = find_within_n_miles(., geo_climbing_gyms, 10),
    n_gym_20mi = find_within_n_miles(., geo_climbing_gyms, 20),
    n_boulder_60mi = find_within_n_miles(., geo_routes |> filter(route_type == "Boulder"), 60),
    n_route_60mi = find_within_n_miles(., geo_routes |> filter(route_type != "Boulder"), 60),
    n_route_200mi = find_within_n_miles(., geo_routes |> filter(route_type != "Boulder"), 200)
  )

# Final ----
subcounty_final <- 
  subcounty_info |> 
  left_join(
    subcounty_climbing |> 
      st_drop_geometry() |> 
      select(subcounty_fips, starts_with("n_"))
  ) |> 
  left_join(
    subcounty_gym |> 
      st_drop_geometry() |> 
      select(subcounty_fips, gym_x, gym_y, gym_name, rating, n_votes)
  )


write_csv(subcounty_final, "county-subdivision-info.csv")


# Mock analysis ----
subcounty_final <- read_csv("county-subdivision-info.csv")

subcounty_final |> 
  #filter(county_fips == 42101) 
  #filter(state_name == "North Carolina") |> 
  #filter(county_name == "Durham County") |> 
  filter(
      subcounty_fips %in% c( 
        "0803191007", # denver
        
        "1100150000", # dc
        "2951065000", # st. louis
        
        "3702190086", # asheville
        "3706393764", # durham (triangle)
        "3708192124", # greensboro (morehead)
        "3711993268", # charlotte
        
        "4210160000", # philly
        
        "4845390165"  # austin
        
      )
  )
|> 
  filter(
    n_population > 10000,
    n_lgbt > 50,
    n_gym_10mi > 0,
    pct_dem > 0.60,
    pct_poverty < 0.30,
    pct_temp_ideal >= 0.60,
    pct_temp_below_60 < 0.25,
    #n_route_60mi >= 5
  ) |> 
  mutate(
    n_lgbt = pmin(n_lgbt, 2000)
  ) |> 
  view()
