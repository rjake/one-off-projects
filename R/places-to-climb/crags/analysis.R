# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))


library(tidyverse)
library(glue)

output_folder <- "areas/WV"
output_final_metadata <- glue("{output_folder}/final_route_metadata.csv")

route_info <- read_csv(output_final_metadata)


route_info |> 
  mutate(
    jake_ind = 
      str_detect(feature_tags, "dihedral|chimney|flake|hueco|mantle") &
      !str_detect(feature_tags, "slab")
  ) |> 
  filter(
    # bad_ind == 0,
    good_ind == 1,
    #avg_stars >= 2,
    #jake_ind == 1,
    sport_ind == 1,
    str_detect(rating_simple, "v[0-3]$|5.[6-9]")
    # str_detect(region_1, "Miller")
  ) |> 
  mutate(
    region_2 = fct_infreq(region_2) |> fct_rev()
  ) |> 
  ggplot(aes(y = region_2)) +
  geom_bar(aes(fill = rating_simple, group = rating_simple))

#
route_metrics <-
  route_info |> 
  filter(
    region_2 == "Linville Gorge"
  ) |>
  # filter(
  #   bad_ind == 0
  # ) |>
  # select(-bad_ind) |> 
  summarise(
    .by = region_3,
    x = mean(x),
    y = mean(y),
    n = n(),
    across(ends_with("_ind"), ~sum(.x, na.rm = TRUE))
  )

route_metrics |>
  filter(rope_ind > 0) |> 
  select(
    -c(rope_ind, boulder_ind)
  ) |> 
  pivot_longer(
    ends_with("_ind")
  ) |> 
  ggplot(
    aes(
      value, 
      name, 
      fill = case_when(
        str_detect(name, "^(sport|tr)") ~ "type",
        name == "bad_ind" ~ "bad",
        .default = "good"
      )
    )
  ) +
  facet_wrap(~region_3) +
  geom_col() +
  theme(
    plot.title.position = "plot",
    panel.background = element_rect("white", "grey80"),
    legend.position = "none"
  ) +
  labs(
    title = "Routes at Linville Gorge, NC",
    y = "Metric",
    x = "# Routes"
  )

route_info |> 
  filter(route_id == 107163191) |> 
  select(
    -c(rating_int, rating_simple, x_diff, y_diff)
  ) |> 
  as.list() |> 
  yaml::as.yaml() |> 
  cat()
