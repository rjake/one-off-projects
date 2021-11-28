# from RProj
setwd("trump-tweets")

library(rtweet)
library(tidyverse)
library(sf)
library(urbnmapr) # devtools::install_github("UrbanInstitute/urbnmapr")
# library(tigris)
library(simplecolors)

# load token
rtweet::get_token() # see below

# read in data
county_demo <- local({
  county_land_coverage <- readRDS("county_land_coverage.Rds")

  county_election <-
    read_csv("../covid-elections-2020/election_by_county.csv") %>%
    transmute(
      county_fips,
      pct_gop = per_gop,
      voting_category = case_when(
        pct_gop < 0.45 ~ "blue",
        pct_gop < 0.55 ~ "purple",
        TRUE ~ "red"
      )
    )

  county_sf <-
    urbnmapr::get_urbn_map("counties", sf = TRUE) %>%
    sf::st_transform(4326)

  county_pop <- readRDS("census_pop.Rds")

  county_pop %>%
    select(county_fips, pop) %>%
    left_join(select(county_land_coverage, county_fips, state_fips, aland_sq_mi)) %>%
    filter(!state_fips %in% c("02", "15", "72")) %>%
    mutate(pop_density = pop / aland_sq_mi) %>%
    group_by(state_fips) %>%
    mutate(
      # sum_pop = sum(pop),
      # sum_area = sum(aland_sq_mi),
      mean_pop_density_state = sum(pop) / sum(aland_sq_mi)#,
      # pop_rank = percent_rank(pop),
      # pop_pct = coalesce(pop / sum(pop), 1),
      # density_rank = percent_rank(pop_density)
    ) %>%
    ungroup() %>%
    mutate(
      urban_rural = case_when(
        pop_density > mean_pop_density_state ~ "urban",
        # pop_pct > 0.5 ~ "urban",
        # pop >= 5e4 ~ "urban",
        # pop >= 10000 ~ "micropolitan", # supposed to be 10-49K & next to an uban center
        TRUE ~ "rural"
      )
    ) %>%
    left_join(county_election) %>%
    left_join(county_sf) #%>% View()
})


state_region_totals <-
  county_demo %>%
  #filter(state_fips == "42") %>%
  group_by(state_fips, state_abbv, urban_rural, voting_category) %>%
  # summarise(
  #   pop = sum(pop),
  #   mean_pop_density = mean(pop_density),
  #   geometry = st_union(geometry)
  # ) %>%
  ungroup() %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_cast("POLYGON")



coords <-
  state_region_totals %>%
  sf::st_point_on_surface() %>%
  # sf::st_centroid() %>%
  st_coordinates()

final_table <-
  state_region_totals %>%
  cbind(coords) %>%
  # mutate(
  #   unique_id = row_number(),
  #   search_radius = aland_sq_mi / 50
  # ) %>%
  # relocate(unique_id, .before = everything()) %>%
  relocate(X, Y, .before = geometry) %>%
  rename_all(tolower)

saveRDS(final_table, "county_areas.Rds")

# compare to this map:
# https://www.ers.usda.gov/topics/rural-economy-population/rural-classifications/what-is-rural.aspx#:~:text=Most%20counties%2C%20whether%20metro%20or,of%20urban%20and%20rural%20populations.&text=According%20to%20this%20system%2C%20rural,with%20fewer%20than%202%2C500%20people.


final_table %>%
  filter(state_abbv == "TX") %>%
  # filter(pop > 10000) %>%
#  arrange(pop) %>% slice(1:3)
  ggplot() +
  geom_sf(
   aes(fill = paste(voting_category, urban_rural), geometry = geometry),
   color = NA
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_point(aes(x, y)) +
  # facet_wrap(~paste(voting_category, urban_rural)) +
  scale_fill_manual(
    values = c(
      `blue urban`   = sc("blue"),
      `blue rural`   = sc("blue1"),
      `red urban`    = sc("red"),
      `red rural`    = sc("red1"),
      `purple urban` = sc("violet"),
      `purple rural` = sc("violet1"))
  )

#
county_demo %>%
  filter(state_abbv == "MD") %>%
  filter(urban_rural == "rural", voting_category == "red") %>%
  arrange((pop_density)) %>%
  slice_head(n = 10) %>%
  group_by()
  ggplot() +
  geom_sf(aes(fill = pop, geometry = geometry))

#




# create token named "twitter_token"
n_tweets <- 18000

non_blue_counties <-
  final_table %>%
  #filter(state_abbv == "PA") %>%
  #filter(pop > 10000) %>%
  filter(voting_category != "blue") %>%
  add_count(state_abbv, wt = pop, name = "pop_state") %>%
  mutate(
    pct_by_state = pop_state / sum(pop),
    pct_by_county = pop / pop_state,
    n_tweet_state = floor(n_tweets * pct_by_state),
    n_tweet_county = n_tweet_state * pct_by_county
  ) %>%
  # mutate(unique_id = rank(unique_id))
  # select(-c(geometry, fips_class), -matches("fips")) %>%
  as_tibble()

non_blue_counties %>%
  distinct(state_name, pop_state, pct_by_state, n_tweet_state) %>%
  arrange(desc(pop_state))

non_blue_counties %>%
  arrange(search_radius) %>%
  slice(1) %>%
  t()

tweets <- search_tweets(
  # q = "until:2021-01-07 since:2021-01-06 capital trump",
  q = "capitol",
  geocode = "37.78,-122.40,5mi",
  n = 5,
#  type = "popular",
  include_rts = FALSE,
  lang = "en"
)

tweets$text

#trend <- trends_available()

rt <- search_tweets(
  "lang:en",
  geocode = lookup_coords("usa"),
  n = 10
)

## create lat/lng variables using all available tweet and profile geo-location data
rt <- lat_lng(rt)

## plot state boundaries
# par(mar = c(0, 0, 0, 0))
# maps::map("state", lwd = .25)

## plot lat and lng points onto state map
plot(1,1)
with(rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))



theme_minimal(base_family = "Roboto")





