setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)
# library(ggstar)
library(sf)
library(mapview)


prep_acs <- read_rds("cache-data/prep_acs.Rds")
roads <- read_rds("cache-data/roads.Rds")
raw_neighborhoods <- read_sf("input/durham-hoods.geojson")

bg <- 
  sf::read_sf("input/census-block-group-nc/tl_2025_37_bg.shp") |> 
  rename_all(tolower) |> 
  filter(countyfp == "063")


map_limits <- 
  st_bbox(
    c(
      xmin = -78.85, xmax = -78.99, 
      ymin =  35.89, ymax =  36.05
    ), 
    crs = st_crs(4326)
  )

local_neigborhoods <-
  raw_neighborhoods |> 
  st_crop(map_limits) 

mapview(local_neigborhoods, zcol = "name", legend = FALSE)


local_roads <-
  roads$osm_lines |> 
  st_crop(map_limits) |> 
  mutate(
    lanes = as.integer(lanes) |> replace_na(1) |> pmin(3),
    maxspeed = str_extract(maxspeed, "\\d+") |> as.integer() |> replace_na(10)
  ) |> 
  select(
    where(~mean(is.na(.x)) <  0.3)
  ) |> 
  mutate(
    .by = c(name, maxspeed),
    lanes = max(lanes)
  )


local_roads |> 
  ggplot() +
  geom_sf(aes(color = highway, linewidth = lanes)) +
  scale_linewidth(
    breaks = 1:3,
    range = c(0.1, 1)
  )


points_of_interest <-
  data.table::fread(
    "location, y, x
    g ,  35.9507, -78.9245
    o, 35.9674, -78.90027
    o, 35.8992, -78.90466
    h,  35.9810, -78.9257
    "
    # opt_5, -78.92313612698618,  35.89766629278903,
    # opt_3, -78.89658808465747, 35.951150348266104,
    # opt_2, -78.89261107481181, 35.97466785511796,
    # opt_1, -78.89380118830732, 35.91915240473086
    # opt_4, -78.896033,         35.921157
  )

plot_census <-
  bg |> 
  select(geoid, aland) |> 
  st_transform(4326) |>
  st_crop(map_limits) |> 
  left_join(
    prep_acs
    #raw_acs #|> filter(variable == "income_avg")
  ) 

mapview(
  plot_census,
  zcol = "estimate",
  alpha.regions = 0.25,
  col.regions = colorRampPalette(c("red", "blue"))(170)
) +
  mapview::mapview(
    points_of_interest |>
      filter(location |> str_detect("home|gym")) |> 
      st_as_sf(
        coords = c("x", "y"),
        crs = 4326
      ), 
    color = "white", 
    col.regions = "black",
    legend = FALSE
  )

plot_census |> 
  ggplot() +
  geom_sf(aes(fill = estimate), color = NA) +
  geom_sf(
    data = local_roads |> filter(highway |> str_detect("motor|prim|sec")), 
    color = "grey80",
    aes(linewidth = lanes)
  ) +
  geom_label(
    data = points_of_interest,# |> filter(str_detect(location, "h|g")),
    aes(x, y, label = location),
    size = 3,
    label.r = unit(0.25, "lines"),
    color = "black",
    fill = "yellow"
  ) +
  scale_linewidth(
    breaks = 1:3,
    range = c(0.1, 1),
    guide = "none"
  ) +
  scale_fill_continuous(
    n.breaks = 5,
    palette = simplecolors::sc("red4", "red2", "blue2", "blue4", "blue5"),
    guide = "colourbar"
  ) +
  #theme_void() +
  theme(
    legend.position = "right"
  ) +
  labs(
    fill = "Mean Poverty Ratio\n(weighted)"
  )




# none
ct <- 
  sf::read_sf("input/census-tract-nc/tl_2025_37_tract.shp") |> 
  rename_all(tolower) |> 
  filter(countyfp == "063")

ct |> 
  st_transform(4326) |>
  st_crop(map_limits) |> 
  ggplot() +
  geom_sf(aes(fill = aland)) +
  geom_point(
    data = points_of_interest,
    aes(x, y),
    size = 4,
    color = "red"
  )


#
parcels <- 
  st_read("layers/Parcels/Parcels_NEW.shp") |> 
  rename(neighborhood = NEIGHBORHO) |> 
  rename_all(tolower)

prep_data <-
  parcels |> 
  st_set_crs(2264) |>
  # select(
  #   where(~mean(is.na(.x)) < 0.2)
  # ) |> 
  transmute(
    id = objectid_1,
    location_addr = location_a,
    property_descr = property_d,
    land_class = land_class,
    deed_date = ymd(deed_date),
    year = year(deed_date),
    month = month(deed_date),
    acreage_sqft = acreage * 43560,
    bldg_sqft = heated_are,
    total_prop_value = total_prop,
    cost_total_value = cost_total
  ) |> 
  mutate(
    n_slash = str_count(land_class, "/"),
    land_type = ifelse(
      n_slash == 0, 
      str_extract(land_class, "^\\w+"),
      str_extract(land_class, "^[^/]+")
    ),
    land_subtype = ifelse(
      n_slash == 0, 
      str_remove(land_class, "^\\w+ "),
      str_remove(land_class, "^[^/]+/ ?")
    )
  ) |> 
  relocate(geometry, .after = everything())


as_points <-
  prep_data |> 
  st_centroid() |> 
  filter(land_type |> str_detect("RES")) |> 
  st_transform(crs = 4326)  |>  
  st_crop(map_limits) %>%
  mutate(
    x = st_coordinates(.)[,1],
    y  = st_coordinates(.)[,2]
  )

as_points |> 
  #head(1000) |> 
  ggplot(aes(x, y, z = year)) +
  coord_map() +
  stat_summary_hex(fun = function(x) mean(x), bins = 20) +
  # Apply a color scale (viridis is colorblind-friendly and looks good)
  scale_fill_viridis_c(option = "magma", name = "Average Z Value") +
  theme_minimal()

p <- plotly::ggplotly()

mapview(p)
#



# Load data:
if (FALSE) {
  library(osmdata)
  library(tidycensus)

  roads <- 
    getbb("Durham, North Carolina") |> 
    opq() |> 
    add_osm_feature(
      key = "highway",
      value = c("motorway", "primary", "secondary", "tertiary")
    )  |> 
    osmdata_sf()
  
  saveRDS(roads, "cache-data/roads.Rds")
  

  acs_vars <- load_variables(2023, "acs5", cache = !TRUE)
  table(acs_vars$geography)
  
  
  use_vars <-
    c(
      income = "B02001_003", 
      pop = "B02001_001",
      poverty_denominator = "C17002_001",
      poverty_0.00_0.50 = "C17002_002",
      poverty_0.50_0.99 = "C17002_003",
      poverty_1.00_1.24 = "C17002_004",
      poverty_1.25_1.49 = "C17002_005",
      poverty_1.50_1.84 = "C17002_006",
      poverty_1.85_1.99 = "C17002_007",
      poverty_2.00_3.00 = "C17002_008" # really 2.00+
      
      # white = "B02001_002",
      # poverty_denom = "B06011_001",
      # poverty_000_099 = "B06012_002",
      # poverty_100_149 = "B06012_003",
      # poverty_150_above = "B06012_004",
      # income_avg = "B19013_001",
      # income_b = "B19013B_001",
      # income_a = "B19013C_001",
      # income_p = "B19013D_001",
      # income_o = "B19013E_001",
      # income_s = "B19013F_001",
      # income_t = "B19013G_001",
      # income_w = "B19013H_001",
      # income_h = "B19013I_001","B19049_001", #
    )  
  
  raw_acs <-
    get_acs(
      geography = "block group", 
      variables = use_vars,
      state = 37, 
      county = 063,
      cache_table = TRUE
    ) |> 
    rename_all(tolower)
  
  saveRDS(raw_acs, "cache-data/raw_acs.Rds")
  
  poverty_avg <-
    use_vars |> 
    as_tibble(rownames = "variable") |> 
    filter(
      str_detect(variable, "pov.*\\d")
    ) |> 
    mutate(
      min = str_extract(variable, "\\d\\.\\d+") |> as.numeric(),
      max = str_extract(variable, "\\d\\.\\d+$") |> as.numeric(),
      mid = (min + max) / 2
    )
  
  
  prep_acs <-
    raw_acs |> 
    inner_join(poverty_avg |> select(variable, mid)) |> 
    summarise(
      .by = geoid,
      total_avg = sum(mid * estimate),
      total = sum(estimate),
      estimate = total_avg / total
    )
  
  # race
  acs_race_vars <-
    list(
      total =  "B03002_001",
      white =  "B03002_003",
      black =  "B03002_004",
      asian =  "B03002_006",
      latino = "B03002_012"
    )

race_2013 <-
  get_acs(
      geography = "block group", 
      variables = acs_race_vars,
      year = 2013,
      state = 37, 
      county = 063,
      cache_table = TRUE
    ) |> 
    rename_all(tolower)

race_2023 <-
    get_acs(
      geography = "block group", 
      variables = acs_race_vars,
      year = 2023,
      state = 37, 
      county = 063,
      cache_table = TRUE
    ) |> 
    rename_all(tolower)

totals <-
  bind_rows(
    "2013" = race_2013,
    "2023" = race_2023,
    .id = "year"
  ) |>
  select(-c(name, moe)) |>
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) |>
  mutate(
    other = total - latino - white - black - asian
  ) |>
  pivot_longer(
    white:other,
    names_to = "race",
    values_to = "n"
  ) |>
  mutate(pct = n / total * 100)
  
  totals_compare <-
    totals |> 
    #filter(geoid == "370630001011", race == "white") |> 
    pivot_wider(
      names_from = year,
      values_from = c(total, n, pct)
    )

  
  
  
  
  saveRDS(prep_acs, "cache-data/prep_acs.Rds")
  
  
}
