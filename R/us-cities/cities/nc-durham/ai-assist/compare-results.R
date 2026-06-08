library(tidyverse)

# --- load data ---
# weights.csv: columns = criterion, weight
# scores.csv:  columns = criterion, <neighborhood columns...>
# both csvs should have the same criterion values
# meta criteria (ease/patience) should be included in scores.csv but NOT in weights.csv

weights <- read_csv("weights.csv", show_col_types = FALSE)
scores  <- read_csv("scores.csv",  show_col_types = FALSE)

meta_criteria <- c("ease_of_finding_now", "ideal_patience_310k", "ideal_patience_350k")

neighborhoods <- names(scores) |> discard(\(x) x == "criterion")

scores_core <- scores |> filter(!criterion %in% meta_criteria)
scores_meta <- scores |> filter(criterion %in% meta_criteria)

# --- validate ---
stopifnot(
  "weight criteria and score criteria don't match" =
    setequal(weights$criterion, scores_core$criterion)
)

# --- weighted score per scenario ---
# weights.csv can have multiple weight columns (one per scenario)
# e.g.: criterion, base, ease_focused, ideal_310, ideal_350
# if only one weight column, it is used for all scenarios

weight_scenarios <- names(weights) |> discard(\(x) x == "criterion")

compute_weighted <- function(scenario_weights) {
  scores_core |>
    left_join(scenario_weights, by = "criterion") |>
    mutate(across(all_of(neighborhoods), \(x) x * weight)) |>
    summarise(across(all_of(neighborhoods), sum)) |>
    pivot_longer(everything(), names_to = "neighborhood", values_to = "score")
}

normalize <- \(w) w / sum(w) * 100

weighted_scores <- weight_scenarios |>
  set_names() |>
  map(\(s) {
    w <- weights |>
      select(criterion, weight = all_of(s)) |>
      mutate(weight = normalize(weight))
    compute_weighted(w)
  }) |>
  imap(\(df, nm) mutate(df, scenario = nm)) |>
  list_rbind()

# --- extract meta scores ---
meta_wide <- scores_meta |>
  pivot_longer(-criterion, names_to = "neighborhood", values_to = "value") |>
  pivot_wider(names_from = criterion, values_from = value)

# --- apply multiplier logic ---
# multiplier = meta_score / 5 (scales 1-5 -> 0.2-1.0)
# adjusted_score = weighted_score * multiplier
# patience_gap   = ideal_adj_350 - ease_adj (how much lifestyle you gain by waiting)

results <- weighted_scores |>
  pivot_wider(names_from = scenario, values_from = score) |>
  left_join(meta_wide, by = "neighborhood") |>
  mutate(
    across(
      all_of(weight_scenarios),
      \(x) round(x, 1),
      .names = "{.col}"
    ),
    ease_multiplier    = ease_of_finding_now / 5,
    pat310_multiplier  = ideal_patience_310k / 5,
    pat350_multiplier  = ideal_patience_350k / 5,
    # adjusted scores — uses whichever scenario column you nominate below
    # edit `base` to match your primary scenario column name if different
    ease_adj           = round(base * ease_multiplier,   1),
    ideal_adj_310      = round(base * pat310_multiplier, 1),
    ideal_adj_350      = round(base * pat350_multiplier, 1),
    patience_gap       = round(ideal_adj_350 - ease_adj, 1)
  ) |>
  arrange(desc(ideal_adj_350))

print(results)

# --- plot: adjusted scores + patience gap ---
plot_df <- results |>
  select(neighborhood, ease_adj, ideal_adj_310, ideal_adj_350, patience_gap) |>
  pivot_longer(
    c(ease_adj, ideal_adj_310, ideal_adj_350),
    names_to  = "scenario",
    values_to = "score"
  ) |>
  mutate(
    scenario = factor(scenario,
      levels = c("ease_adj", "ideal_adj_310", "ideal_adj_350"),
      labels = c("Buy now (ease-adjusted)", "Ideal at $310K", "Ideal at $350K")
    ),
    neighborhood = neighborhood |>
      str_replace_all("_", " ") |>
      str_to_title() |>
      fct_reorder(score, max)
  )

gap_df <- results |>
  select(neighborhood, patience_gap) |>
  mutate(neighborhood = neighborhood |> str_replace_all("_", " ") |> str_to_title())

ggplot(plot_df, aes(score, neighborhood, fill = scenario)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = score),
    position = position_dodge(width = 0.9),
    hjust = -0.15, size = 2.8
  ) +
  geom_text(
    data = gap_df,
    aes(x = 102, y = neighborhood, label = paste0("+", patience_gap)),
    inherit.aes = FALSE,
    hjust = 0, size = 2.8, color = "grey40", fontface = "italic"
  ) +
  scale_fill_manual(values = c("#FAC775", "#9FE1CB", "#7F77DD")) +
  scale_x_continuous(limits = c(0, 115), expand = c(0, 0)) +
  labs(
    title    = "Neighborhood scores — ease-adjusted vs. patience scenarios",
    subtitle = "Italic values at right show patience gap (ideal $350K − buy now)",
    x = "Adjusted score", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position        = "top",
    panel.grid.major.y     = element_blank(),
    panel.grid.minor       = element_blank()
  )
