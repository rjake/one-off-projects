# https://chord.rocks/scales/f-sharp-gypsy-minor
# https://www.fretjam.com/borrowed-chords.html

setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(glue)
library(simplecolors)

# all_notes <- 
#   "ab	a	bb	b	c	db	d	eb	e	f	gb	g" |> 
#   str_extract_all("\\w+") |> 
#   pluck(1)

input <- list(
  name = "F# Double Harmonic Minor - 1, 2, b3, b5, 5, b6, 7",
  notes = c("f# g# a c c# d f"),
  caption = "Hungarian Minor, Gypsy Minor, Romanian Hijaz"
)

chord_structure <-
  read_csv("input/chord-patterns.csv") |> 
  arrange(degree, priority) |> 
  filter(chord != "I7") |> 
  mutate(
    chord = fct_reorder(chord, degree),
    type = fct_inorder(type, ordered = TRUE)
  ) |> 
  print()

scale_degrees <- read_csv("input/scale-degrees.csv")

notes_used <- str_split(input$notes, " ")[[1]]
uses_sharps <- any(str_detect(notes_used, "#"))

replace_enharmonics <- function(x) {
  if (!uses_sharps) {
    return(x)
  }
  
  enharmonics <- c(
    "ab" = "g#",
    "bb" = "a#",
    "db" = "c#",
    "eb" = "d#",
    "gb" = "f#"
  )
  
  str_replace_all(x, enharmonics)
}


all_notes <-
  tibble(
    #scale_note = all_notes,
    ab = c(0:11),
    a  = (ab + 11) %% 12,
    bb = (ab + 10) %% 12,
    b  = (ab + 9) %% 12,
    c  = (ab + 8) %% 12,
    db = (ab + 7) %% 12,
    d  = (ab + 6) %% 12,
    eb = (ab + 5) %% 12,
    e  = (ab + 4) %% 12,
    f  = (ab + 3) %% 12,
    gb = (ab + 2) %% 12,
    g  = (ab + 1) %% 12
  ) |> 
  rename_all(replace_enharmonics) %>%
  mutate(
    scale_note = names(.),
    .before = everything()
  ) |> 
  print()

semitone_degree <-
  set_names(
    scale_degrees$degree,
    scale_degrees$tone
  )

notes <- #read_csv("input/notes-available.csv")
  all_notes |> 
  select(scale_note, (notes_used)) |> 
  filter(scale_note %in% (notes_used)) %>% 
  mutate(scale_note = factor(scale_note, levels = names(.), ordered = TRUE)) |> 
  arrange(scale_note) |> 
  print()

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

find_notes <- function(scale_name, scale_degrees) {
  scales |> 
    filter(chord_degree %in% scale_degrees) |> 
    group_by(base_note) |> 
    filter(
      n() > 1,
      min(chord_degree) == 0,
      max(chord_degree %in% 3:4) > 0
    ) |> 
    ungroup() |> 
    mutate(
      scale = scale_name,
      color = case_when(
        chord_degree == "0" ~ "black",
        chord_degree == "7" ~ "grey",
        TRUE ~ "red" 
      )
    ) #|> print()
}

  
union_scales <-
  bind_rows(
    find_notes(scale_name = "major", scale_degrees = c(0, 4, 7)),
    find_notes(scale_name = "minor", scale_degrees = c(0, 3, 7))
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
    scale_degree = recode(scale_degree, !!!semitone_degree),
    base_note = factor(base_note, levels = notes$scale_note, ordered = TRUE)
  ) |> 
  print()


# major & minor ----
plot_points |> 
  ggplot(aes(x, y)) +
  facet_grid(rows = vars(scale), cols = vars(base_note)) +
  geom_text(
    aes(label = scale_degree),
    color = "grey60", size = 2
  ) +# theme_void()
  geom_polygon(
    data = ~drop_na(.x, color) |>  filter(y < 1),
    fill = NA, color = "grey70"
  ) +
  geom_label(
    data = ~drop_na(.x, color),
    aes(fill = color, label = scale_note),
    label.padding = unit(0.15, "lines"),
    size = 2,
    color = "white"
  ) +
  coord_fixed() +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(0.2)) +
  scale_y_continuous(expand = expansion(0.2)) +
  theme_gray(base_size = 6) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect("white", "grey80")
  ) +
  labs(
    title = input$name,
    subtitle = "black = root, red = 3rd, grey = other",
    caption = input$caption
  )

ggsave("output/drum-diagram.png", width = 5, height = 2.3)

 
# available chords ----

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
      glue("[{note}] {chord}") |> 
      str_remove_all(" \\(\\)|^\\[NA\\]") |> 
      trimws() |> 
      fct_inorder(ordered = TRUE)
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
  arrange(priority, label) |> 
  mutate(label = fct_inorder(label, ordered = TRUE)) |> 
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
  select(base_note, chord, notes, implied, type, label) |> 
  group_by(base_note) |> 
  filter(max(chord %in% c("I", "i")) > 0) |> 
  ungroup() |> 
  print()

scales_found |> 
  mutate(type = fct_relevel(type, "major", "minor", "secondary dominant", "h. minor")) |> 
  mutate(across(c(base_note, notes), toupper)) |> 
  mutate(
    #facet = ifelse(str_detect(type, "minor"), "minor", "major"),
    base_note = 
      as.factor(base_note) |> 
      fct_relevel(!!!toupper(notes$scale_note)),
    color = case_when(
      implied == "" ~ "full",
      implied == "(3/5)" ~ "implied",
      TRUE ~ "1 & 3"
    ),
    weight = ifelse(implied == "", "bold", "plain")
  ) |> #print()
  #pull(base_note) |> str()
  ggplot(aes(x = base_note, y = fct_rev(chord))) +
  facet_wrap(~type, scales = "free_y") +
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
    strip.text = element_text(size = 6),
    panel.grid = element_blank()
  ) +
  labs(
    x = "key",
    y = NULL
  )

ggsave("output/available-scales.png", width = 6, height = 3)

# scales on pan
scale_points <-
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

# scale on pan ----
scale_degree_long <-
  chord_structure |>
  select(-c(alternative, follows:note, priority, chord)) |> 
  mutate(
    group = ifelse(type == "secondary dominant", "major", as.character(type)),
    color = ifelse(type != "secondary dominant", "base", "alt")
  ) |> 
  pivot_longer(
    cols = starts_with("scale_"),
    values_to = "chord_degree"
  ) |> 
  #filter(chord_degree == "6") |> 
  select(-name) |> 
  distinct() |> 
  group_by(group, chord_degree) |> 
  arrange(desc(color)) |> 
  slice(1) |> 
  ungroup() |> 
  print()
  

scales |>
  #filter(base_note == "c#") |> 
  inner_join(scale_degree_long) |> 
  inner_join(point_coords) |>
  #filter(base_note == "c#", group == "minor") |> arrange() |>  print() 
  arrange(base_note, group) |> 
  mutate(
    group = fct_infreq(group),
    base_note = 
      as_factor(base_note) |> 
      fct_relevel(notes$scale_note),
    fill = ifelse(chord_degree == 1, "root", color),
    text_color = (color == "alt")
  ) |> 
  ggplot(aes(x, y)) +
  facet_grid(rows = vars(group), cols = vars(base_note)) +
  geom_label(
    aes(fill = fill, label = scale_note, color = text_color),
    label.padding = unit(0.15, "lines"),
    size = 2
  ) +
  coord_fixed() +
  scale_fill_manual(values = c("white", sc("blue2", "blue4"))) +
  scale_color_manual(values = c("white", sc("blue2"))) +
  scale_x_continuous(expand = expansion(0.2)) +
  scale_y_continuous(expand = expansion(0.2)) +
  theme_gray(base_size = 6) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.background = element_rect("white", "grey80")
  ) +
  labs(
    title = "F# Double Harmonic Minor - 1, 2, b3, b5, 5, b6, 7",
    subtitle = "blue = base notes, orange = borrowed",
    caption = "Hungarian Minor, Gypsy Minor, Hijaz"
  )

ggsave("output/scale-note-availability.png", width = 5, height = 2.3)
