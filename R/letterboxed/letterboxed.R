a <- "qoi"
b <- "ems"
c <- "uct"
d <- "nhv"

library(tidyverse)
library(glue)
#url <- base::url("http://raw.githubusercontent.com/zeisler/scrabble/master/db/dictionary.csv")
all_words <- read_csv("~/Downloads/dictionary.csv", col_names = "word")

unique_letters <- function(x) {
  x |> 
    str_split_1("") |> 
    unique() |> 
    sort() |> 
    glue_collapse()
}


find_only_letters <- function(x, p) {
  pattern <- 
    glue(
      "\\b{p}{p}{{1,}}\\b"
    )
  
  grep(x = x, pattern, value = TRUE)
}


all_letters <- 
  c(a, b, c, d) |> 
  glue_collapse() |> 
  unique_letters()
  
any_letters <- glue("[{all_letters}]")

megamatch <- local({
  match_a <- glue("[{a}][^{a}]")
  match_b <- glue("[{b}][^{b}]")
  match_c <- glue("[{c}][^{c}]")
  match_d <- glue("[{d}][^{d}]")
  
  glue(
    "(({match_a}|{match_b}|{match_c}|{match_d})[^\\1]){{2,}}"
  )
})

megaexclude <- local({
  match_a <- glue("[{a}][{a}]")
  match_b <- glue("[{b}][{b}]")
  match_c <- glue("[{c}][{c}]")
  match_d <- glue("[{d}][{d}]")
  
  glue(
    "({match_a}|{match_b}|{match_c}|{match_d})"
  )
})

matching_words <- # 172,820 -> 1,691
  all_words$word |> 
  #head(100) |> 
  discard(~str_detect(.x, megaexclude)) |> 
  keep(
    ~str_detect(
      .x, 
      glue(
        .open = "<", .close = ">",
        x = any_letters,
        "\\b<x><x>{1,}\\b"
      )
    )
  ) |> 
  keep(
    ~str_detect(.x, megamatch)
  )

df <- 
  tibble(
    word = matching_words,
    word_letters = map_chr(word, ~unique_letters(.x)),
    last_letter = str_extract(word, ".$"),
    remainder = str_remove_all(all_letters, glue("[{word_letters}]")),
    search =
      str_remove_all(all_letters, glue("[{word}]")) |>
      glue(x = _, "^{last_letter}"),
    n_word = nchar(word),
    n_remainder = nchar(all_letters) - nchar(word_letters)
  )

results <- 
  df |> 
  arrange(n_remainder) |> 
  mutate(
    match = map(.x = search,  ~grep(x = df$word, .x, value = TRUE))
  )

results_long <- 
  results |> 
  unnest_longer(
    col = match
  ) |> 
  mutate(
    new_remainder = str_remove_all(remainder, glue("[{match}]"))
  ) |>
  arrange(
    nchar(new_remainder),
    word, 
    match
  )

results_long |> 
  #select(word, remainder, match, new_remainder) |> 
  view()

'
   word           word_letters last_letter remainder search n_word n_remainder match      new_remainder
   <chr>          <chr>        <chr>       <chr>     <glue>  <int>       <int> <chr>      <chr>        
 1 mischievous    cehimosuv    s           nqt       ^s         11           3 squint     ""           
 2 mischievous    cehimosuv    s           nqt       ^s         11           3 squints    ""           
 3 centimos       ceimnost     s           hquv      ^s          8           4 squinch    "v"          
 4 centimos       ceimnost     s           hquv      ^s          8           4 squish     "v"          
 5 cheque         cehqu        e           imnostv   ^e          6           7 economist  "v"          
 6 cheque         cehqu        e           imnostv   ^e          6           7 economists "v"          
 7 cinquecentist  ceinqstu     t           hmov      ^t         13           4 theonomous "v"          
 8 cinquecentisti ceinqstu     i           hmov      ^i         14           4 isocheim   "v"          
 9 cinquecentists ceinqstu     s           hmov      ^s         14           4 schmoe     "v"          
10 cinquecentists ceinqstu     s           hmov      ^s         14           4 schmos     "v"
'
