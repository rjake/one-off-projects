library(tidyverse)
library(vroom)
library(glue)
library(rlang)

url <- "https://raw.githubusercontent.com/zeisler/scrabble/master/db/dictionary.csv"

# 8:38

all_words <-
  tibble(
    word = read_lines(url),
    nchar = nchar(word)
  )



base_words <-
  all_words |>
  filter(nchar <= 15)

under_9 <-
  base_words |>
  filter(nchar <= 9)


return_words <- function(n, ref) {
  #n = 3;ref = n_2;
  words <- filter(base_words, nchar == n)  |>  pull(word)
  reference <- glue_collapse(ref, "|")
  matches <- str_detect(words, reference)

  words[matches]
}


n_2 <- filter(base_words, nchar == 2) |> pull(word)
n_3 <- return_words(3, n_2)
n_4 <- return_words(4, n_3)
n_5 <- return_words(5, n_4)
n_6 <- return_words(6, n_5)
n_7 <- return_words(7, n_6)
n_8 <- return_words(8, n_7)
n_9 <- return_words(9, n_8)


match_words <- function(x, y, n){
  # x = n_03; y = n_02; n = 2;
  word <- y[n]
  col_1 <- deparse(substitute(x))
  col_2 <- deparse(substitute(y))

  tibble(
    v1 = x[str_detect(x, word)],
    v2 = word
  ) |>
    rename(
      !!col_1 := v1,
      !!col_2 := v2
    )

}

res_n3 <- map_dfr(seq_along(n_2), match_words, x = n_3, y = n_2)
res_n4 <- map_dfr(seq_along(n_3), match_words, x = n_4, y = n_3)
res_n5 <- map_dfr(seq_along(n_4), match_words, x = n_5, y = n_4)
res_n6 <- map_dfr(seq_along(n_5), match_words, x = n_6, y = n_5)
res_n7 <- map_dfr(seq_along(n_6), match_words, x = n_7, y = n_6)
res_n8 <- map_dfr(seq_along(n_7), match_words, x = n_8, y = n_7)
res_n9 <- map_dfr(seq_along(n_8), match_words, x = n_9, y = n_8)


final_list <-
  res_n3 |>
  select(n_2, n_3) |>
  full_join(res_n4) |>
  full_join(res_n5) |>
  full_join(res_n6) |>
  full_join(res_n7) |>
  full_join(res_n8) |>
  full_join(res_n9) %>%
  mutate(max_length = 9 - rowSums(is.na(.)))


starting_summary <-
  final_list |>
  group_by(n_2) |>
  summarise(
    n_3 = sum(!is.na(n_3)),
    n_4 = sum(!is.na(n_4)),
    n_5 = sum(!is.na(n_5)),
    n_6 = sum(!is.na(n_6)),
    n_7 = sum(!is.na(n_7)),
    n_8 = sum(!is.na(n_8)),
    n_9 = sum(!is.na(n_9))
  ) |>
  arrange(desc(n_9), desc(n_8))


example_seq <-
  final_list |>
  drop_na(n_9) |>
  filter(str_detect(n_4, "laps|heat")) |>
  select(-max_length) |>
  pivot_longer(-n_2) |>
  group_by(n_2) |>
  summarise(
    seq = paste(value, collapse = " > ")
  ) |>
  ungroup() |>
  glue_data(
    "     {n_2} > {seq}"
  ) |>
  glue_collapse("\n")


starting_summary |>
  mutate(n_2 = fct_reorder(n_2, n_3)) |>
  gather(length, count, -n_2) |>
  #filter(!str_detect(length, "3|4")) |>
  filter(count > 0) |>
  mutate(
    length = as.factor(substr(length, 3, 3)),
    #n_2 = fct_reorder(n_2, count, sum),
    split = (100 - as.integer(n_2)) %/% 34
  ) |>
  # filter(as.integer(n_2) <= 40) |>
  group_by(length) |>
  mutate(pct = count / sum(count)) |>
  ungroup() |>
  ggplot(aes(length, n_2)) +
  geom_tile(aes(fill = log(pct * 1000)), color = NA) +
  geom_text(
    aes(label = count),
    color = "grey90", size = 3
  ) +
  facet_wrap(~split, scales = "free_y", nrow = 1) +
  scale_fill_viridis_c(end = 0.8, direction = -1) +
  theme_gray(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 8),
    #text = element_text(family = "sans"),
    plot.title.position = "plot"
  ) +
  labs(
    title = 'When playing Scrabble, "at" can take you far',
    subtitle = glue(
    'This chart shows how many paths will allow you to keep making words by adding one letter on each turn.\nHere are examples to go from a two letter word to a nine letter word:\n',
    example_seq, collapse = "\n"
    ),
    x = "word length",
    y = "two letter words"
  )


