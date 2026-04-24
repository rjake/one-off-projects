# Census ----
"-----------------------------------------------------------------


walkability
https://close.city/?x=-78.9032&y=35.9748&z=11.7502&r=0&l=1111111&tt_30=1&tt_43=3

city maps
https://maps.durhamnc.gov/?x=36.01084996101076&y=-78.79857862372533&z=577790.5542885&r=0&b=11&a=-1&u=0&pid=NA&s=custom&l=#
------------------------------------------------------------------"

# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)
library(tidycensus)
library(glue)
library(janitor)
library(sf)
library(geojsonsf)
library(simplecolors)

geo <- 
  list(
    state = "NC",
    county = "Durham"
  )


acs_vars <- load_variables(2022, "acs5", TRUE)


vars_of_interest <- {
  acs_vars |> 
    filter(
      str_detect(
        name, 
        c(
          "B01001_001",    # population
          #"B09019_001",   # households
          "B09019_01[13]", # gayness
          "B06012_00[23]", # poverty
          "B25087_0[01].", # housing costs
          "B25096.*"       # house price
        ) |> 
          glue_collapse("|")
      )
    ) |> 
    filter(!str_detect(name, "B25087_00[12]|B25096_0(01|02|12)")) |> 
    mutate(
      category = 
        case_when(
          name == "B01001_001" ~ "population",
          name == "B09019_001" ~ "household",
          str_detect(name, "B09019_01[13]") ~ "lgbt",
          str_detect(name, "B06012_00[23]") ~ "poverty",
          str_detect(name, "B25087_0[01]") ~ "housing costs",
          str_detect(name, "B25096.*") ~ "house price"
        )
    )
}

# Get Data ----
raw_geo <- 
  get_acs(
    variables = "B01001_001",
    geography = "tract",
    geometry = TRUE,
    cache_table = TRUE,
    keep_geo_vars = TRUE,
    state = geo$state,
    county = geo$county,
    year = 2023
  )

prep_geo <-
  raw_geo |> 
  select(
    STATEFP:TRACTCE,
    GEOID,
    tract_id = NAME.x,
    variable,
    pop = estimate,
    moe
  ) |> 
  janitor::clean_names()
  

geo_points <-
  prep_geo |> 
  st_centroid() %>%
  cbind(st_coordinates(.)) |> 
  st_set_geometry(NULL) |> 
  rename_all(~tolower(.x))


prep_geo |> 
  ggplot() +
  geom_sf() +
  geom_point(data = geo_points, aes(x, y, color = pop, size = pop))
  

raw_demo <- 
  get_acs(
    variables = vars_of_interest$name,
    geography = "tract",
    #cache_table = TRUE,
    keep_geo_vars = TRUE,
    state = geo$state,
    county = geo$county,
    year = 2023
  )


prep_demo <-
  raw_demo |> 
  select(-c(NAME, moe)) |> 
  janitor::clean_names() |> 
  filter(
    variable %in% c("B01001_001", "B06012_002", "B06012_003")
  ) |> 
  mutate(
    variable = 
      recode(
        variable,
        "B01001_001" = "pop", 
        "B06012_002" = "poverty_0_100", 
        "B06012_003" = "poverty_100_150"
      )
  ) |> 
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) |> 
  mutate(
    n_poverty_150 = poverty_0_100 + poverty_100_150,
    pct_poverty_150 = round(n_poverty_150 / pop * 100)
  ) |> 
  right_join(
    geo_points |> select(geoid, x, y)
    #prep_geo |> select(geoid)
  )

spots <-
  tribble(
    ~y     , ~x       , ~name,
    36.0352, -78.90003, "rock quarry dog park",
    35.9147, -78.92085, "pineywood dog park",
    35.9525, -78.92388, "gym"
  )

prep_geo |> 
  ggplot() +
  geom_sf(fill = "white", color = "grey80") +
  #geom_sf(data = prep_demo, aes(fill = pct_poverty_150, geometry = geometry)) +
  geom_point(data = prep_demo, aes(x, y, color = pct_poverty_150, size = pop)) +
  geom_point(data = spots, aes(x, y), shape = "\U2605", size = 7, color = "white") +
  geom_point(data = spots, aes(x, y), shape = "\U2605", size = 6) +
  scale_color_steps(
    n.breaks = 4,
    low = sc("blue2"),
    high = sc("brightred5")
  ) +
  coord_sf(
    xlim = c(-79.01, -78.78),
    ylim = c(35.9, 36.12)
  ) +
  theme_void()


