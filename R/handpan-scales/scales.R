# https://chord.rocks/scales/f-sharp-gypsy-minor
# https://www.fretjam.com/borrowed-chords.html

setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(glue)

notes <- read_csv("input/notes-available.csv")

scales <-
  notes |> 
  mutate(order = row_number()) |> 
  pivot_longer(
    cols = -c(scale_note, order),
    names_to = "base_note",
    values_to = "chord_degree"
  ) |> 
  arrange(base_note) |> 
  relocate(base_note) |> 
  print()


point_coords <- local({
  d <- 0.7
  tibble(
    tone = c(5, 6, 7, 1, 2, 3, 4, 5),
    scale_note = names(notes)[tone + 1],
    scale_degree = notes[tone, 2, drop = TRUE],
    x = c(0, -d, d, -1, 1, -d, d, 0),
    y = c(-1, -d, -d, 0, 0, d, d, 1)
  )
})

plot(point_coords[,4:5], pch = 16)

find_notes <- function(...) {
  scale_degrees <- c(...)
  scale_name = names(match.call()[-1])
  
  scales |> 
    filter(chord_degree %in% scale_degrees) |> 
    group_by(base_note) |> 
    filter(
      n() > 1,
      max(str_detect(chord_degree, "3")) > 0
    ) |> 
    ungroup() |> 
    mutate(
      scale = scale_name,
      color = case_when(
        chord_degree == "1" ~ "black",
        chord_degree == "5" ~ "grey",
        TRUE ~ "red" 
      )
    ) #|> print()
}

  
union_scales <-
  bind_rows(
    #find_notes(five = c("1", "5")),
    # find_notes(sus2 = c("1", "2")),
    # find_notes(sus4 = c("1", "4")),
    find_notes(major = c("3", "5", "1")),
    find_notes(minor = c("b3", "5", "1"))
  ) |> 
  print()


plot_points <-
  union_scales |> 
  distinct(base_note, scale) |> 
  full_join(
    point_coords, 
    by = character(),
    copy = TRUE
  ) |> 
  left_join(union_scales) |>
  arrange(order) |> 
  mutate(
    base_note = 
      as_factor(base_note) |> 
      fct_relevel(notes$scale_note)
  ) |> 
  print()

# major & minor ----
plot_points |> 
  ggplot(aes(x, y)) +
  facet_grid(
    rows = vars(scale), 
    cols = vars(base_note)
  ) +
  geom_text(
    aes(label = scale_degree),
    color = "grey80",
    size = 3
  ) +# theme_void()
  geom_polygon(
    data = 
      ~drop_na(.x, color) |> 
      filter(y < 1),
    fill = NA,
    color = "grey40"
  ) +
  geom_label(
    data = ~drop_na(.x, color),
    aes(fill = color, label = scale_note),
    label.padding = unit(0.15, "lines"),
    size = 3,
    color = "white"
  ) +
  coord_fixed() +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(0.1)) +
  scale_y_continuous(expand = expansion(0.1)) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect("white", "grey80")
  ) +
  labs(
    title = "F# Double Harmonic Minor - 1, 2, b3, b5, 5, b6, 7",
    subtitle = "black = root, red = 3rd, grey = other",
    caption = "Hungarian Minor, Gypsy Minor, Hijaz"
  )

ggsave("output/drum-diagram.png", width = 7, height = 4)

 
# available chords ----
chord_structure <-
  read_csv("input/chord-patterns.csv") |> 
  arrange(priority) |> 
  filter(chord != "I7") |> 
  mutate(
    chord = fct_inorder(chord, ordered = TRUE),
    type = fct_inorder(type, ordered = TRUE)
  )

scale_degrees <- read_csv("input/scale-degrees.csv")

scales_list <-
  scales |> 
  mutate(chord_degree = factor(chord_degree, levels = scale_degrees$degree)) |> 
  group_by(base_note) |>
  arrange(chord_degree) |>
  summarise(scale_list = paste(chord_degree, collapse = " ")) |>
  ungroup() |>
  print()


reorder_degrees <- function(x = c("7", "2", "4")) {
  f <-
    factor(x, levels = scale_degrees$degree) |> 
    sort()
  
  glue("\\b{f}\\b") |> 
    paste(collapse = ".*")
}

chord_search <-
  chord_structure |> 
  mutate(
    #across(where(is.character), replace_na, ""),
    label = 
      glue("{chord} ({alternative}) - {follows} __ {suggests}")#, #|> str_remove_all(" \\(\\)| __ $")
  ) |> 
  rowwise() |> 
  mutate(
    req = glue("{scale_1} .* {scale_3}"),
    search = glue("\\b(({scale_1}|{scale_5}) .* {scale_3}|{scale_3} .* ({scale_5}|{scale_1}))\\b")
    #search = paste(scale_1, scale_3, scale_5, collapse = " -- ")
  ) |> 
  ungroup() |> 
  print()

chord_match <-
  chord_search |> 
  select(chord, label, priority, scale_1, scale_3, scale_5, type) |> 
  full_join(
    scales_list,
    by = character(),
    copy = TRUE
  ) |> 
  filter(
    str_detect(scale_list, glue("\\b{scale_3}\\b")),
    str_detect(scale_list, glue("\\b{scale_1}\\b|\\b{scale_5}\\b"))
  ) |> 
  arrange(base_note, priority) |> 
  select(-priority) |> 
  print()
  
id_notes <- function(base, degree) {
  res <-
    scales[scales$base_note == base & scales$chord_degree == degree,]$scale_note
  
  if (!length(res)) {
    return("")
  }
  
  res
}

scales_found <-
  chord_match |>
  #  slice(29)
  mutate(
    implied = case_when(
      !str_detect(scale_list, glue("\\b{scale_1}\\b")) ~ "(3/5)",
      !str_detect(scale_list, glue("\\b{scale_5}\\b")) ~ "(1/3)",
      TRUE ~ ""
    ),
    notes = paste(
      map2_chr(base_note, scale_1, id_notes),
      map2_chr(base_note, scale_3, id_notes),
      map2_chr(base_note, scale_5, id_notes)
    ) |> 
      trimws()
  ) |> 
  select(base_note, chord, notes, implied, type) |> 
  group_by(base_note) |> 
  filter(max(chord == "I") > 0) |> 
  ungroup() |> 
  print()

library(simplecolors)
scales_found |> 
  mutate(across(c(base_note, notes), toupper)) |> 
  mutate(
    color = case_when(
      implied == "" ~ "full",
      implied == "(3/5)" ~ "implied",
      TRUE ~ "1 & 3"
    ),
    weight = ifelse(implied != "(3/5)", "bold", "plain")
  ) |> #print()
  ggplot(aes(x = base_note, y = fct_rev(chord))) +
  facet_grid(rows = vars(type), scales = "free", space = "free") +
  geom_tile(fill = "white", color = "grey40") +
  geom_text(
    aes(
      label = paste(notes, implied),
      color = color,
      fontface = weight
    ), 
    show.legend = FALSE,
    size = 2
  ) +
  scale_color_manual(values = sc("brightblue3", "grey5", "grey4")) +
  #scale_alpha(range = c(0.4, 1)) +
  theme_gray(base_size = 9) +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "key",
    y = NULL
  )

ggsave("output/available-scales.png", width = 3, height = 4)
