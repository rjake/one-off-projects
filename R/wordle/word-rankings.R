library(tidyverse)

# https://en.lexipedia.org/
# filter [5 to 5] for only 5-letter words >= 5 times
local({
  if (FALSE) {# only run if needed
    file_location <- "~/../Downloads"
    download.file(
      url = "http://www.gwicks.net/textlists/engmix.zip",
      destfile = file.path(file_location, "words.zip")
    )
    unzip(
      file.path(file_location, "words.zip"),
      exdir = file_location
    )

    file.copy(
      from = file.path(file_location, "engmix.txt"),
      to = "R/wordle/dictionary.txt"
    )
  }
})


raw_words <-
  read_table(file = "~/../Downloads/engmix.txt") |>
  set_names("word") |>
  as.data.frame() # remove attrs


prep_word_list <-
  raw_words |>
  as_tibble() |>
  filter(str_detect(word, "^[a-z]{5}$")) |>
  print()


word_letters <-
  prep_word_list |>
  separate(
    col = word,
    sep = "",
    into = paste0("x", 0:5),
    remove = FALSE
  ) |>
  select(-x0) |>
  print()


word_letters_long <- 
  word_letters |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "position",
    values_to = "letter"
  ) |> 
  print()

letter_position_stats <- 
  word_letters_long |> 
  group_by(letter) |> 
  mutate(letter_freq = n()) |> 
  group_by(letter, position) |> 
  mutate(position_freq = n()) |> 
  ungroup() |> 
  print()


word_letter_stats <- 
  word_letters |> 
  left_join(
    letter_position_stats |> 
      select(word, starts_with("position")) |> 
      pivot_wider(
        names_from = position,
        values_from = position_freq,
        names_prefix = "freq_"
      )
  ) |> 
  print()


letter_stats <- 
  letter_position_stats |> 
  distinct(letter, letter_freq, position, position_freq) |> 
  pivot_wider(
    names_from = position, 
    values_from = position_freq
  ) |> 
  arrange(desc(letter_freq)) |> 
  print(n = 15)
  

word_stats <- 
  letter_position_stats |> 
  mutate(
    letter = factor(letter, levels = letter_stats$letter, ordered = TRUE),
    letter_ord = as.integer(letter)
  ) |> 
  group_by(word) |>
  arrange(letter) |> 
  summarise(
    n_1_5 = sum(letter %in% letter_stats$letter[1:5]),
    n_6_15 = sum(letter %in% letter_stats$letter[6:15]),
    letters_rank = paste(letter, collapse = ""),
    letter_alpha = paste(sort(as.character(letter)), collapse = ""),
    n_letter = n_distinct(letter),
    mean_letter_rank = mean(letter_ord),
    mean_letter_freq = mean(letter_freq),
    sum_letter_freq = sum(letter_freq),
    mean_pos_freq = mean(position_freq),
    median_pos_freq = median(position_freq)
  ) |> 
  ungroup() |> 
  left_join(word_letter_stats) |> 
  arrange(desc(mean_letter_freq)) |> 
  print()


# best starting words: raise, arise
word_stats |> 
  filter(n_letter == 5) |> 
  filter(n_1_5 == 5) |> 
  #filter(mean_letter == 3) |> 
  arrange(mean_letter_rank, desc(mean_pos_freq)) 

# best 2nd words: mount, donut, moult, month, count, mound, clout, hound
word_stats |> 
  filter(n_letter == 5) |> 
  filter(n_6_15 >= 5) |> 
  arrange(mean_letter_rank, desc(mean_pos_freq))

# example: floor - has [lor] but not [ais...], [l] is the 2nd letter
word_stats |> 
  filter(str_detect(word, "[lor]")) |> 
  filter(!str_detect(word, "[aisemntudgcv]")) |> 
  filter(str_detect(word, ".l..."))
