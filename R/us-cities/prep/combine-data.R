# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(sf)
library(geosphere)

find_within_n_miles <- function(x, y, n) {
  st_is_within_distance(
    x = x, 
    y = y, 
    dist = units::set_units(n, "miles")
  ) |> 
    lengths()
}

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

geo_county <-
  read_csv("sources/output/county-geo.csv") |> 
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

county_gym <-
  geo_county |>
  # filter(county_fips == "42101") |>
  st_join(geo_climbing_gyms, join = st_nearest_feature)

county_climbing <-
  geo_county %>%
  #filter(county_fips == "42101") %>%
  mutate(
    n_gym_10mi = find_within_n_miles(., geo_climbing_gyms, 10),
    n_gym_20mi = find_within_n_miles(., geo_climbing_gyms, 20),
    n_boulder_60mi = find_within_n_miles(., geo_routes |> filter(route_type == "Boulder"), 60),
    n_route_60mi = find_within_n_miles(., geo_routes |> filter(route_type != "Boulder"), 60),
    n_route_200mi = find_within_n_miles(., geo_routes |> filter(route_type != "Boulder"), 200)
  )

# Prepare data ----
county_final <- 
  census_county |> 
  transmute(
    county_fips, 
    county_name,
    n_population,
    pct_poverty,
    n_lgbt = n_lgbt * 2,
    pct_lgbt = pct_lgbt * 2,
    pct_cost_higher_2400 = pct_cost_higher,
    pct_price_higher_400k = pct_price_higher
  ) |> 
  mutate(
    .before = everything(),
    county_fips, 
    state_name = str_extract(county_name, "(?<=, ).*"), 
    county_name = str_remove(county_name, " County,.*")
  ) |> 
  left_join(
    geo_county |> st_drop_geometry()
  ) |> 
  left_join(info_elections) |> 
  left_join(info_weather) |> 
  left_join(
    county_climbing |> 
      st_drop_geometry() |> 
      select(county_fips, starts_with("n_"))
  ) |> 
  left_join(
    county_gym |> 
      st_drop_geometry() |> 
      select(county_fips, gym_x, gym_y, gym_name, rating, n_votes)
  )

county_final |> 
  write_csv("county-info.csv")


subcounty_info <- 
  census_subcounty |> 
  transmute(
    subcounty_fips, 
    n_population,
    pct_poverty,
    n_lgbt = n_lgbt * 2,
    pct_lgbt = pct_lgbt * 2,
    pct_cost_higher_2400 = pct_cost_higher,
    pct_price_higher_400k = pct_price_higher
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


subcounty_final |> 
  #rename_all(tolower) |> 
  write_csv("county-subdivision-info.csv")


# Mock analysis ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(simplecolors)

county_final <- read_csv("county-info.csv")
crag_stats <- read_csv("sources/output/crag-stats.csv")


# county map ----
states <- map_data("state") 
  
min_y <- 41
min_x <- -92

county_final |> 
  filter(
    x > min_x, 
    y < min_y,
    n_population > 10000,
    n_lgbt > 50,
    pct_dem > 0.60,
    pct_poverty < 0.30
  ) |> 
  mutate(
    temp =
      case_when(
        pct_temp_below_60 > 0.35 ~ "cold",
        pct_temp_below_60 > 0.25 ~ "kind of cold",
        pct_temp_below_60 > 0.15 ~ "kind of warm",
        .default =  "warm",
      )
  ) |> 
  ggplot() +
  geom_point( # crags
    data = 
      crag_stats |> filter(x > min_x, y < min_y),
    aes(x, y, shape = "crag/boulder"), 
    alpha = 0.75,
    color = "grey85",
    fill = sc("dullorange1"),
    size = 3
  )  +
  geom_polygon( # states
    data = states |> filter(long > min_x - 2, lat < min_y),
    aes(long, lat, group = group),
    fill = NA, color = "grey70", linewidth = 0.25
  ) +
  geom_point( # population
    aes(x, y, size = n_lgbt, color = temp, fill = temp),
    shape = 21, 
    #fill = "white", 
    #alpha = 0.5
  ) +
  geom_point( # gyms
    aes(gym_x, gym_y, shape = "climbing gym"), color = "black", size = 1
  ) +
  scale_color_manual(
    values = simplecolors::sc("blue5", "blue3", "red3", "red5")
  ) +
  scale_fill_manual(
    values = simplecolors::sc("blue3", "blue1", "red1", "red3")
  ) +
  scale_shape_manual(
    values = c(16, 24)
  ) +
  guides(
    color = guide_none(),
    fill = guide_legend(override.aes = list(alpha = 1, size = 3, color = "grey")),
    shape = guide_legend(override.aes = list(color = "black")),
    size = guide_legend(override.aes = list(color = "black"))
  ) +
  coord_quickmap(
    xlim = c(min_x + 2, min_x + 18),
    ylim = c(min_y - 15.5, min_y - 1)
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(20, 20, 20, 20),
    #panel.background = element_rect(fill = "grey75")
  ) +
  labs(
    x = NULL,
    y = NULL,
    shape = "Points of \nInterest",
    fill = "Temperature",
    size = "# LGBT"
  )
#

ggsave("../img/map-se.png", width = 6, height = 5.2)



#subcounty_final <- read_csv("county-subdivision-info.csv")



subcounty_filter <-
  subcounty_final |>   
  filter(
    #n_population > 10000,
    n_lgbt > 20,
    n_gym_10mi > 0,
    pct_dem > 0.60,
    pct_poverty < 0.30
  ) |> 
  select(
    #-matches("^.$|_[xy]$"),
    -matches("rating|sqmi|votes|below_[45]"),
    -matches("popul|20mi|route_60mi")
  ) |> 
  mutate(
    .keep = "unused",
    pct_temp_above_60 = 1 - pct_temp_below_60,
    pct_not_poverty = 1 - pct_poverty,
    pct_price_affordable = 1 - pct_price_higher_400k,
    pct_cost_affordable = 1 - pct_cost_higher_2400
  ) 

subcounty_filter |> 
  write_csv("potential-subcounty.csv")

potential <- 
  subcounty_final |> 
  #filter(county_fips == 42101) 
  #filter(state_name == "North Carolina") |> 
  #filter(county_name == "Durham County") |> 
  filter(
    subcounty_fips %in% c( 
      "0803191007", # denver
      
      #"1100150000", # dc
      
      "2951065000", # st. louis
      
      "0601390270", # briones, CA
      
      "3702190086", # asheville
      "3706393764", # durham (triangle)
      "3708192124", # greensboro (morehead)
      "3711993268", # charlotte
      
      "4100392142", # oregon
      
      "4210160000", # philly
      
      "4845390165"  # austin
    )
  ) |>
  select(
    state_name, subcounty_name,
    where(~is.numeric(.x))
  )

potential |> 
  select(
    -matches("^.$|_[xy]$"),
    -matches("rating|sqmi|votes")
  ) |> 
  select(where(is.numeric)) |> 
  drop_na() |> 
  #select(pct_cost_higher_2400, pct_price_higher_400k) |> 
  cor() |> 
  corrplot::corrplot(
    type = "upper",
    method = "ellipse",
    order = "AOE"
  )

potential_long <-
  potential |> 
  select(
    -matches("^.$|_[xy]$"),
    -matches("rating|sqmi|votes|below_[45]"),
    -matches("popul|20mi|route_60mi")
  ) |> 
  mutate(
    .keep = "unused",
    pct_temp_above_60 = 1 - pct_temp_below_60,
    pct_not_poverty = 1 - pct_poverty,
    pct_price_affordable = 1 - pct_price_higher_400k,
    pct_cost_affordable = 1 - pct_cost_higher_2400
  ) |> 
  pivot_longer(
    cols = -c(state_name, subcounty_name)
  ) |> 
  #distinct(name) |> 
  mutate(
    cat = case_when(
      str_detect(name, "popul|pov|lgbt|dem") ~ "demo",
      str_detect(name, "temp") ~ "temp",
      str_detect(name, "gym|boulder|route") ~ "climb",
      str_detect(name, "cost|price") ~ "costs",
    )
  ) |> 
  mutate(
    .by = name,
    x = dense_rank(value)
  )

potential_long |> 
  summarise(
    .by = c(state_name, subcounty_name),
    mean = mean(x),
    sum = sum(x)
  ) |> 
  view()

potential_long |> 
  ggplot(aes(x, y = name, color = subcounty_name)) +
  geom_point() +
  facet_grid(
    rows = vars(cat),
    scales = "free_y"
  )




subcounty_final |> 
  filter(
    n_gym_10mi > 0,
    pct_dem > 0.65,
    pct_poverty < 0.25,
    n_lgbt > 50
  ) |> 
  select(
    -matches("^.$|_[xy]$"),
    -matches("rating|sqmi|votes")
  ) |> 
  filter(
    .by = state_name,
    pct_t
  )




subcounty_final |>   
  filter(
    n_population > 10000,
    n_lgbt > 50,
    n_gym_10mi > 0,
    pct_dem > 0.60,
    pct_poverty < 0.30,
    #pct_temp_ideal >= 0.60,
    #pct_temp_below_60 < 0.25,
    #n_route_60mi >= 5
  ) |> 
  mutate(
    n_lgbt = pmin(n_lgbt, 2000)
  ) #|> view()


subcounty_final |> 
  filter(
    state_name == "California",
    n_gym_10mi >= 1,
    pct_dem > 0.65,
    pct_poverty < 0.2
  ) |> 
  DT::datatable(
    filter = "top"
  )
reactable::reactable()
gt::gt() |> 
  gt::opt_interactive(
    use_search = TRUE,
    use_filters = TRUE,
    use_resizers = TRUE,
    use_highlight = TRUE,
    use_compact_mode = TRUE,
  )
