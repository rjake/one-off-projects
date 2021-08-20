library(tidycensus)
library(tidyverse)
library(simplecolors)

vars_2010 <- load_variables(2010, "acs5", TRUE)


filter(vars_2010, str_detect(concept, "HISPANIC OR LATINO ORIGIN BY RACE")) |>
  print(n = Inf)


acs_race_vars <-
  list(
    total =  "B03002_001",
    white =  "B03002_003",
    black =  "B03002_004",
    asian =  "B03002_006",
    latino = "B03002_012"
  )

race_2010 <-
  get_acs("state", year = 2010, unlist(acs_race_vars)) |>
  mutate(year = 2010)

race_2018 <-
  get_acs("state", year = 2018, unlist(acs_race_vars)) |>
  mutate(year = 2018)

totals <-
  bind_rows(
    race_2010,
    race_2018
  ) |>
  select(-moe) |>
  rename_all(tolower) |>
  rename(state = name) |>
  filter(geoid != "72") |>
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
  mutate(pct = n / total * 100) |>
  print()

# filter(geoid == "35") |>
biggest_change <-
  totals |>
  filter(!geoid %in% c("72")) |>
  filter(race == "white") |>
  select(-c(total, n)) |>
  pivot_wider(
    names_from = year,
    values_from = pct,
    names_glue = "y_{year}"
  ) |>
  mutate(
    change = y_2018 - y_2010
  ) |>
  arrange(change) |>
  # filter(y_2015 > 50, y_2018 < 50) |>
  print()

totals |>
  # filter(state %in% biggest_change$state[1:20]) |>
  saveRDS("census-demographic-shift/totals.Rds")


plot_prep <-
  totals |>
  mutate(
    fifty_pct = total / 2,
    var = n
  ) |>
  group_by(geoid, race) |>
  arrange(year) |>
  mutate(
    diff = max(var - lag(var), na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    fct_race =
      ifelse(race == "white", race, paste(race, geoid)) |>
      fct_reorder(diff, mean, na.rm = TRUE) |>
      fct_rev() |>
      fct_relevel("white", after = Inf),
    fct = as.numeric(fct_race)
  ) |>
  group_by(geoid) |>
  mutate(
    rank = rank(diff),
    alpha = as.integer(race == "white" | fct == min(fct))
  ) |>
  ungroup() |>
  print()


color_race <- list(
  white = "white",
  asian =  sc("brightteal4"),
  black =  sc("brightviolet4"),
  latino = sc("brightorange4"),
  other =  sc("brightblue4")
)




plot_prep |>
  filter(geoid != "72") |>
  filter(state %in% biggest_change$state[1:20]) |>
  mutate(state = factor(state, levels = biggest_change$state[order(biggest_change$y_2018)])) |>
  ggplot(aes(year, var)) +
  facet_wrap(~state, scales = "free") +
  geom_area(aes(group = fct_race, fill = race, alpha = alpha)) +
  geom_linerange(
    #data = ~filter(.x, race == "white"),
    aes(ymin = 0, ymax = total),
    color = "grey20"
  ) +
  geom_line(aes(y = fifty_pct), color = "black", linetype = "dotted") +
  scale_alpha(range = c(0.2, 1), guide = "none") +
  scale_fill_manual(values = color_race) +
  scale_y_continuous(
    labels = scales::number_format(scale = 1/1e6, accuracy = 1, suffix = "M")
  ) +
  scale_x_continuous(breaks = range(plot_prep$year)) +
  #theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Population shifts by race, 2015-2018"
  )

ggsave("census-demographic-shift/top-20-white-reduced.png")

plotly::ggplotly()

# v_2010  <- load_variables(2010, "sf1", cache = TRUE)
# v_2000 <-  load_variables(2000, "sf1", cache = TRUE)
# decennial_race_vars <-
#   list(
#     total =  "P005001",
#     latino = "P005010",
#     white =  "P005003",
#     black =  "P005004",
#     asian =  "P005006"
#   )
# race_2000 <-
#   get_decennial("state", year = 2000, unlist(race_vars)) |>
#   mutate(year = 2000)
#
# race_2010 <-
#   get_decennial("state", year = 2010, unlist(race_vars)) |>
#   mutate(year = 2010)
