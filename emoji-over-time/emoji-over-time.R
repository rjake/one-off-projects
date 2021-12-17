# https://home.unicode.org/emoji/emoji-frequency/
library(googlesheets4)
library(tidyverse)

theme_set(
  theme_bw() +
    theme(panel.grid = element_blank())
)

raw_data <- # requires OAuth token to be set up
  map_dfr(
    c(2019, 2021),
    ~ googlesheets4::read_sheet(
        ss = "1Zs13WJYdZL1pNZP0dCIXkWau_tZOjK3mmJz0KNq4I30",
        sheet = paste0(.x, "_ranked")
      ) |>
      mutate(source = .x)
  )

prep_data <-
  raw_data|>
  rename_all(tolower) |>
  rename(introduced = year) |>
  relocate(source, rank, emoji) |>
  relocate(introduced, .after = everything()) |>
  arrange(name) |>
  add_count(emoji) |>
  group_by(emoji) |>
  summarise(
    introduced = min(introduced),
    rank_diff = rank[source == 2019] - rank[source == 2021],
    age = max(source) - introduced
  ) |>
  ungroup() |>
  mutate(
    id = fct_reorder(emoji, rank_diff) |>
      as.integer(),
    color = case_when(
      introduced < 2010 ~ "< 2010",
      introduced < 2015 ~ "< 2015",
      TRUE ~ ">= 2015"
    )
  ) |>
  print()


prep_data |>
  ggplot(aes(id, rank_diff, fill = color)) +
  geom_col() +
  scale_fill_manual(values = c("black", "grey", "red"))


prep_data |>
  ggplot(aes(introduced, rank_diff, color = rank_diff)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_gradient2(mid = "grey95")
