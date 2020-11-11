library(tidyverse)
library(cartogram)
library(urbnmapr) # devtools::install_github("UrbanInstitute/urbnmapr")
library(sf)
library(tmap)

# raw data ----
covid_raw <-
  read_csv(
    url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  )

county_csv <-
  read_csv(
    url("https://raw.githubusercontent.com/btskinner/spatial/master/data/county_centers.csv")
  ) %>%
  select(fips, x = pclon10, y = pclat10)

county_pop <-
  urbnmapr::countydata %>%
  select(fips = county_fips, pop = hhpop)

# data manipulation ----
covid_latest <-
  covid_raw %>%
  drop_na(fips) %>%
  #filter(fips == "08089") %>%
  group_by(fips) %>%
  mutate(
    prev_deaths = lag(deaths, default = 0, order_by = date),
    deaths = ifelse(deaths < prev_deaths, prev_deaths, deaths),
    new_deaths = deaths - prev_deaths
  ) %>%
  ungroup() %>%
  # filter(new_deaths < 0) %>%
  filter(date == max(date))


# combine data ----
county_info <-
  county_csv %>%
  left_join(county_pop) %>%
  left_join(covid_latest %>% select(fips, cases, deaths, new_deaths)) %>%
  mutate(
    case_rate = cases / pop,
    death_rate = deaths / pop,
    mortality_rate = deaths / cases
  ) %>%
  mutate(across(ends_with("rate"), ~(round(.x * 100, 3)))) %>%
  #filter(x > -85) %>%
  filter(x > -130) # remove alaska


# set_crs <- "+init=epsg:4326"

counties_sf <-
  get_urbn_map(map = "counties", sf = TRUE) %>%
  rename(fips = county_fips) %>%
  right_join(county_info) %>%
  drop_na(deaths)

# make cartogram ----
covid_dorling <-
  cartogram_dorling(counties_sf, "pop", k = 0.5, itermax = 100)

map_limits <-
  st_bbox(covid_dorling)

# tmap ----
tmap_mode("view")

tm_basemap() +
  tm_shape(covid_dorling, bbox = map_limits) +
  tm_polygons(
    "pop", style = "fisher",
    border.col = "grey60",
    legend.title = "2010 Population by county"
  )

# ggplot ----
ggplot(covid_dorling) +
  geom_sf(aes(fill = pop), color = "grey80") +
  scale_fill_viridis_b(direction = -1, option = "B", trans = "log") +
  theme_void() +
  labs(title = "US Population")

# save ----
covid_dorling %>%
  select(fips:state_name, pop_2010 = pop) %>%
  saveRDS("map_dorling_cartogram.Rds")

new_df <- readRDS("map_dorling_cartogram.Rds")
