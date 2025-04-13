library(tidyverse)

elections <- read_csv("data/elections-2024.csv")

possible <- 
  elections |> 
  filter(
    per_gop < 0.35,
    total_votes > 10000
  )

crag_info <- 
  read_csv("data/mountain-project-popular-routes.csv") 

crag_info |> 
  #filter(state == "Michigan") |> 
  filter(
    .by = crag,
    avg_stars > 2,
    n() > 5
  ) |> 
  mutate(
    x = round_any(x, 0.5),
    y = round_any(y, 0.3)
  ) |>
  # mutate(
  #   .by = crag,
  #   mid_x = mean(x),
  #   mid_y = mean(y)
  # ) |> 
  #count(crag, x, y, mid_x, mid_y) |> 
  count(crag, x, y) |> 
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(long, lat, group = group),
    color = "grey30",
    linewidth = 0.5
  ) +
  geom_point(
    aes(x, y, color = crag, size = n)
  ) +
  # geom_point(aes(mid_x, mid_y), shape = 21, fill = NA, color = "black", size = 5) +
  guides(color = "none") +
  #theme(legend.position = "none") +
  coord_quickmap() +
  theme_void()


crag_info |> 
  summarise(
    .by = c(crag),
    n = n(),
    mean_rating = mean(avg_stars),
    median_rating = median(avg_stars)
  ) |> 
  filter(n > 10) |> #view()
  ggplot(aes(mean_rating, median_rating, size = n)) +
  geom_point() +
  geom_abline() +
  coord_fixed()


crag_info |> 
  filter(
    .by = crag,
    #avg_stars > 2.e,
    n() > 10
  ) |> 
  mutate(
    x = round_any(x, 0.5),
    y = round_any(y, 0.3)
  ) |>
  summarise(
    .by = c(crag, x, y),
    n = n(),
    avg_stars = mean(avg_stars)
  ) |> 
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(long, lat, group = group),
    fill = "white",
    color = "grey80",
    linewidth = 0.5
  ) +
  geom_point(
    aes(x, y, color = avg_stars, size = n)
  ) +
  # geom_point(aes(mid_x, mid_y), shape = 21, fill = NA, color = "black", size = 5) +
  #guides(color = "none") +
  scale_color_binned(
    low = "grey90",
    high = "dodgerblue"
  ) +
  #theme(legend.position = "none") +
  coord_quickmap() +
  theme_void()


plotly::ggplotly()

crag_routes <- read_csv("data/mountain-project-popular-routes.csv")

apply(st_distance(points_sf, polygons_sf), 1, min)
https://stackoverflow.com/questions/56787646/r-how-to-use-apply-in-this-case-to-speed-up-the-function/56793183

crag_routes |> 
  #filter(state == "North Carolina") |>
  filter(x < -100) |> 
  # mutate(
  #   # round to 3 miles
  #   x2 = round_any(x, 1/54 * 5),
  #   y2 = round_any(y, 1/69 * 5)
  # ) |>
  #distinct(x2, y2)
  mutate(
    .by = c(state, crag),
    x2 = median(x),
    y2 = median(y)
  ) |> 
  add_count(x2, y2) |> 
  ggplot() +
  # geom_polygon(
  #   data = map_data("state", "Idaho"),
  #   aes(long, lat),
  #   fill = "white"
  # ) +
  # geom_segment(
  #   aes(
  #     x = x, y = y, 
  #     xend = x2, yend = y2,
  #     group = route
  #   ),
  #   color = "red"
  # ) +
  geom_point(aes(x2, y2, size = n), color = "red", alpha = 0.5) +
  geom_point(aes(x, y), size = 0.5)
+
  coord_quickmap(ylim = c(42, 43))

raw_gyms <- 
  jsonlite::read_json(
    "https://raw.githubusercontent.com/Georift/climbing-board-locations/HEAD/data/tensionboardapp2-locations.json"
  )

gyms <- 
  raw_gyms$gyms |> 
  map_dfr(~flatten_df(.x)) |> 
  rename(
    y = latitude,
    x = longitude
  )
gyms |> 
  filter(
    x < -50,
    between(y, 25, 50)
  ) |> 
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(long, lat, group = group),
    color = "grey30",
    linewidth = 0.5
  ) +
  geom_point(
    aes(x, y), color = "pink"
  )


ggplot() +
  geom_polygon(
    data = map_data("state", "idaho"),
    aes(long, lat, group = group),
    color = "grey30",
    linewidth = 0.5
  ) +
  geom_point(
    data = res,
    aes(x, y), color = "orange"
  ) +
  coord_quickmap()



subcounty_info |> 
  select(
    county_subdivision:state, 
    pop = n_population, 
    n_lgbt, pct_lgbt, 
    n_price_150_299K, 
    pct_price_150_299K, 
    pct_poverty,
    pct_dem,
    precip_in:temp_max,
    x, y
  ) |> 
  filter(state == "Vermont") |> view()
  filter(
    n_lgbt > 100,
    pct_poverty < 0.2
  ) |> 
  arrange(desc(pct_price_150_299K))  |> 
  view()
  
  apply(st_distance(points_sf, polygons_sf), 1, min)

  
  r Unit: seconds    expr       min        lq      mean    median        uq      max neval cld    
  loop     7.086761  8.287012  9.023580  8.825570  9.646178 11.59616    10  a    
  apply   40.170422 41.481090 42.398297 42.048745 42.562616 46.17528    10     b  
  native   6.795684  6.931117  7.840289  7.867703  8.146455 10.64996    10  a 
