library(tidyverse)

df <-
  tibble::tribble(
    ~skill, ~interest, ~proficiency, ~category,
    "Python",     85L,          10L, "opportunities",
    "JS",         70L,          20L, "opportunities",
    "Stats",      65L,          40L, "opportunities",

    #"Excel",      30L,          90L, "teach",
    "SQL",        60L,          90L, "strengths",
    "R",         100L,          95L, "strengths",
    "Qlik",       75L,          80L, "strengths",
    "Tableau",    90L,          85L, "strengths",
  ) |>
  mutate(size = proficiency / interest) |>
  print()





df |>
  ggplot(aes(interest, proficiency, color = category)) +
  ggforce::geom_mark_rect(
    aes(group = category, label = category, fill = category),
#    data = . %>% filter(skill == "JS"),
    con.colour = NA,
    expand = 0.04,
    color = NA,
    label.fill = NA,
    label.buffer = unit(-3, "mm")
  ) +
  geom_point(aes(size = (proficiency / interest))) +
  ggrepel::geom_text_repel(aes(label = skill), size = 3, color = "black") +
  xlim(0, 100) +
  ylim(0, 100) +
  scale_size(range = c(2, 6)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(
    title = "",
    x = "Interest",
    y = "Proficiency"
  )
#


df |>
  pivot_longer(c(interest, proficiency)) |>
  mutate(name = fct_rev(name)) |>
  ggplot(aes(x = name, y = value, group = skill)) +
  geom_line() +
  geom_point(aes(size = value)) +
  ggtext::geom_richtext(
    data = ~filter(.x, name == "interest"),
    aes(label = skill, hjust = -0.1),
    fill = NA,
    label.color = NA
  )
#
