setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)

#unzip("data/kaggle.zip", exdir = "data/")

raw_data <-
  map_df(
    .x = list.files("data", ".csv", full.names = TRUE),
    .f = ~read_csv(.x)
  ) |> 
  rename_all(tolower)


prep_df <-
  raw_data |> 
  #filter(category == "Picture") |> 
  select(one_of(names(raw_data) |> sort())) |> 
  relocate(
    film, year, winner, release_date, mpaa_rating,
    starts_with("rating")
  ) |> 
  select(
    -matches(
      "^age|bafta|birth|category|female|goldenglobe|^nom|nonom_|oscarstat|previous|q\\d|win_"
    ),
    -c(mpaa_g:mpaa_r)
  ) |> 
  summarise(
    .by = c(film, year, contains("rating")),
    across(everything(), ~max(.x))
  )

prep_df |> 
  #filter(str_detect(mpaa_rating, "PG")) |> 
  filter(
    (genre_crime + genre_filmnoir) == 0,
    (genre_horror + genre_thriller + genre_war + genre_western) == 0
  ) |> 
  select(film:rating_rtcritic, where(~any(.x == 1, na.rm = TRUE))) |> 
  reactable::reactable(
    filterable = TRUE,
    pagination = FALSE
  )


raw_data <-
  read_tsv("https://raw.githubusercontent.com/DLu/oscar_data/main/oscars.csv")

# https://github.com/AminFadaee/Academy-Awards-Data/blob/master/data/complete/2022.json
library(jsonlite)
get_data <- function(file) {
  file <- "~/github/Academy-Awards-Data/data/complete/2022.json"
  x <- fromJSON(file)
  df <-
    tibble(
      category = x$category,
      nominations = x$nominations
    ) |> 
    unnest(nominations) |> 
    select(-c(notes, secondary)) |> 
    unnest(primary)
}



library(rvest)
library(tidyverse)

raw_html <- 
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Animated_Feature" |> 
  rvest::read_html()
  
all_tables <- 
  raw_html |> 
  html_nodes("table") |> 
  #rvest::html_element("table") |> 
  html_table()


selecetd_tables <- 
  all_tables |> 
  keep( ~names(.x)[1] == "Year") |> 
  map_dfr(c)



df <-
  read_csv("https://raw.githubusercontent.com/1234567shubu/analysis-of-movies-database/main/movie_meta_data.csv")
