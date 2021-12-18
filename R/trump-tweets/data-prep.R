# workspace ---
setwd("trump-tweets")

library(tidyverse)
library(rtweet)
library(tigris)
options(tigris_use_cache = TRUE)


# Twiter ----
# Create Twitter token for rtweet
token <-
 rweet::create_token(
   app = "yake84",
   consumer_key    = Sys.getenv("twitter_api_key"),
   consumer_secret = Sys.getenv("twitter_api_secret_key"),
   access_token    = Sys.getenv("twitter_access_token"),
   access_secret   = Sys.getenv("twitter_access_token_secret")
 )

# land coverage ----
# County FIPS data - need ALAND field
# ALAND field, comes from tigris which came with tidycensusto
 raw_county_sf <- tigris::counties(resolution = "20m")

 raw_county_sf %>%
   # sf::st_set_geometry(NULL) %>%
   # as_tibble() %>%
   select(
     state_fips = STATEFP,
     county_fips = GEOID,
     ALAND
   ) %>%
   mutate(ALAND = ALAND / 1e6 / 2.58999) %>%       # m2 -> km2 => mi2
   rename(aland_sq_mi = ALAND) %>%
   saveRDS("county_land_coverage.Rds")

 # Land area measurements are originally recorded as whole square meters (to convert square meters to square kilometers, divide by 1,000,000; to convert square kilometers to square miles, divide by 2.58999; to convert square meters to square miles, divide by 2,589,988).
#
# Persons per square mile - population and housing unit density are computed by dividing the total population or number of housing units within a geographic entity by the land area of that entity measured in square miles or in square kilometers. Density is expressed as "population per square mile (kilometer)" or "housing units per square mile (kilometer)." To determine population per square kilometer, multiply the population per square mile by .3861.

# population ----
# Get pop data from census
# help from: https://dcl-wrangle.stanford.edu/census.html
 census_api_request <-
   "https://api.census.gov/data/2018/pep/population?get=GEONAME,DATE_CODE,DATE_DESC#' ,POP&for=county:*"

 raw_census <-
   census_api_request %>%
   jsonlite::fromJSON(simplifyVector = TRUE, flatten=TRUE) %>%
   as_tibble()

 raw_census %>%
   setNames(slice(., 1) %>% unlist(use.names = FALSE)) %>%
   slice(-1) %>%
   filter(str_detect(DATE_DESC, "^7.*2018")) %>%
   transmute(
     county_fips = paste0(state, county),
     pop = as.integer(POP)
   ) %>%
   saveRDS("census_pop.Rds")

# County classifications ----
# https://www.ers.usda.gov/data-products/
raw_urban_classification <-
  read_csv("census_county_classification/UrbanInfluenceCodes2013.csv") %>%
  transmute(
    county_fips = FIPS,
    pop = Population_2010,
    uic_id = UIC_2013,
    uic_desc = Description,
    class = case_when(
      uic_id %in% c(1, 2) ~ "metro",
      uic_id %in% c(3, 5, 8) ~ "micropolitan",
      uic_id %in% c(4, 5, 6, 9) ~ "exurbs",
      #uic_id ,
      TRUE ~ "rural"
    )
  )

# non-core mean no urban core in the county
count(raw_urban_classification, class, uic_id, uic_desc)

raw_typology <-
  read_csv("census_county_classification/2015CountyTypologyCodes.csv") %>%
  mutate(FIPStxt = str_pad(FIPStxt, 5, "left", "0")) %>%
  rename_all(tolower) %>%
  rename_all(str_replace_all, "[ -]", "_") %>%
  rename_all(str_remove_all, "[,_]\\d.*") %>%
  select(-c(state, county_name)) %>%
  rename(county_fips = fipstxt)
