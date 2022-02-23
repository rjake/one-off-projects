library(tidyverse)
library(httr)

# NYT data -----
library(httr)

wordle_script_text <- 
  GET("https://www.nytimes.com/games/wordle/main.18637ca1.js") |> 
  content(as = "text", encoding = "UTF-8")

# code inside 'var Ma=[...]'
js_list <- 
  str_extract(
    string = wordle_script_text, 
    pattern = "(?<=var Ma\\=\\[)[^]]+(?=])"
    #           --------------- ----- --- 
  ) |> 
  str_split(",") |> 
  unlist() |> 
  str_remove_all('\\"')


raw_words <- tibble(word = js_list)

write_csv(raw_words, "R/wordle/input/nyt_word_list.csv")




# Dictionary data ---- 
# Past iterations used this data instead
# https://en.lexipedia.org/ - filter [5 to 5] for 5-letter words & >= 5 times
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
  to = "R/wordle/input/dictionary.txt"
)

# raw_words <-
#   read_table(file = "~/../Downloads/engmix.txt") |>
#   set_names("word") |>
#   as.data.frame() |> # remove attrs
#   filter(str_detect(word, "^[a-z]{5}$")) # make sure non-UTF8 entries dropped
