library(tidyverse)

scores_raw <- "
criterion,weight,lakewood,tuscaloosa_lakewood,duke_park,watts_hillandale,old_west_durham,trinity_park,northgate,colonial_village,forest_hills,morehead_hills
safety_community_feel,22,4,3,3,5,4,5,4,4,4,4
price_fit,16,5,5,3,3,2,1,5,5,3,3
lot_size_gt4k,12,4,5,4,4,2,1,4,4,5,5
tree_lined_quiet,10,4,4,3,5,4,5,5,4,5,5
story_1_or_1pt5,9,3,4,3,4,3,2,4,4,3,3
liberal_community,8,4,4,4,5,5,5,4,4,4,4
street_walking_culture,5,4,3,3,5,5,5,4,4,3,3
relaxing_greenspace_5min,4,5,5,3,4,4,5,4,4,5,5
upgrade_potential,6,4,5,3,3,3,3,4,4,4,3
vegan_access_15min,4,4,4,3,5,5,5,3,3,4,4
climbing_access_15min,4,3,3,3,4,4,4,4,4,3,3
siren_frequency_noise,3,4,4,2,4,4,4,3,3,4,4
bikeable_downtown,2,3,2,3,4,5,5,2,2,3,3
"

# --- parse ---
scores <- read_csv(scores_raw, show_col_types = FALSE)
write_csv(scores, "scores.csv")

neighborhoods <- names(scores) |> discard(\(x) x %in% c("criterion", "weight"))

# --- helper: compute weighted scores given a weight vector ---
compute_scores <- function(df, weights) {
  df |>
    mutate(across(all_of(neighborhoods), \(x) x * weights)) |>
    summarise(across(all_of(neighborhoods), sum)) |>
    pivot_longer(everything(), names_to = "neighborhood", values_to = "score") |>
    arrange(desc(score))
}

# --- scenario weight sets ---
# edit these vectors to adjust weights; must align with criterion row order
base_weights      <- scores$weight  # your proposed weights (sums to 103, adjust as needed)

# "ease of finding now": boost price_fit, lot_size, story stock; downweight lifestyle criteria
ease_weights <- c(
  safety_community_feel   = 15,
  price_fit               = 25,
  lot_size_gt4k           = 20,
  tree_lined_quiet        =  5,
  story_1_or_1pt5         = 15,
  liberal_community        =  3,
  street_walking_culture  =  2,
  relaxing_greenspace_5min =  2,
  upgrade_potential       =  5,
  vegan_access_15min      =  2,
  climbing_access_15min   =  2,
  siren_frequency_noise   =  2,
  bikeable_downtown       =  2
)

# "ideal at $310K": lifestyle-first, price_fit penalizes anything above $310K range
ideal_310_weights <- c(
  safety_community_feel   = 22,
  price_fit               = 16,  # hard-ish cap: >$310K median gets lower score
  lot_size_gt4k           = 12,
  tree_lined_quiet        = 10,
  story_1_or_1pt5         =  9,
  liberal_community        =  8,
  street_walking_culture  =  5,
  relaxing_greenspace_5min =  4,
  upgrade_potential       =  5,
  vegan_access_15min      =  3,
  climbing_access_15min   =  3,
  siren_frequency_noise   =  3,
  bikeable_downtown       =  0
)

tibble::tribble(
                          ~Criterion, ~Weight,
           "Safety / community feel",     22L,
                         "Price fit",     16L,
                 "Lot size >4K sqft",     12L,
          "Tree-lined quiet streets",     10L,
                 "1/1.5-story stock",      9L,
                 "Liberal community",      8L,
            "Street-walking culture",      5L,
  "Relaxing greenspace ≤5 min drive",      4L,
                 "Upgrade potential",      6L,
        "Vegan access ≤15 min drive",      4L,
        "Climbing gym ≤15 min drive",      4L,
           "Siren frequency / noise",      3L,
              "Bikeable to downtown",      2L
  )



# "ideal at $350K": same as above but soften price_fit penalty, reallocate weight to lifestyle
ideal_350_weights <- ideal_310_weights |>
  (\(w) { w["price_fit"] <- 10; w["safety_community_feel"] <- 24; w["tree_lined_quiet"] <- 12; w })()

# normalise a weight vector to sum to 100
normalise <- \(w) w / sum(w) * 100

# --- run all scenarios ---
scenarios <- list(
  base         = base_weights,
  ease         = ease_weights,
  ideal_310    = ideal_310_weights,
  ideal_350    = ideal_350_weights
) |>
  map(\(w) compute_scores(scores, normalise(w))) |>
  imap(\(df, nm) mutate(df, scenario = nm)) |>
  list_rbind()

# --- wide view for easy comparison ---
scenarios_wide <- scenarios |>
  pivot_wider(names_from = scenario, values_from = score) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  arrange(desc(ideal_310))

print(scenarios_wide)

# --- visualise ---
scenario_labels <- c(
  base      = "Base weights",
  ease      = "Ease of finding now",
  ideal_310 = "Ideal at $310K",
  ideal_350 = "Ideal at $350K"
)

scenarios |>
  mutate(
    scenario = 
      factor(scenario, levels = names(scenario_labels), labels = scenario_labels),
    neighborhood = 
      neighborhood |>
      str_replace_all("_", " ") |>
      str_to_title() |>
      fct_reorder2(scenario == "Ideal at $310K", score)
  ) |>
  ggplot(aes(scenario, neighborhood, fill = score)) +
  geom_tile() +
  scale_fill_gradient(low = "grey80", high = "navyblue")
+
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(score, 0)),
    position = position_dodge(width = 0.9),
    hjust = -0.15, size = 2.8
  ) +
  #scale_fill_manual(values = c("#9FE1CB", "#FAC775", "#7F77DD", "#F5C4B3")) +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  labs(
    title = "Neighborhood decision matrix",
    subtitle = "Weighted scores by scenario (all normalised to 100-point scale)",
    x = "Weighted score", y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
