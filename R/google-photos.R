library(tidyverse)
library(glue)

setwd("~/My Pictures/google/")

files <-
  list.files(
    path = "inputs/photos/",
    full.names = TRUE,
    recursive = TRUE
  )

files_df <-
  tibble(
    file = files,
    base_name = basename(file),
    ext = tools::file_ext(files) |> tolower()
  ) |> 
  filter(
    !str_detect(ext, "json|zip")
  )

files_df |>  
  count(ext)

# get info (slow)
file_sizes <-
  files_df$file  |>  
  #head() |> 
  file.size() 

file_info <-
  files_df |> 
  mutate(
    size = file_sizes
  )


file_info |> 
  filter(
    str_detect(
      file,
      "\\d/Takeout/Google Photos/Archive"
    )
  ) |> 

# build data frame
df <-
  file_info %>% 
  rownames_to_column(var = "path") %>% 
  as_tibble() %>% 
  rename(
    create_date = ctime,
    modify_date = mtime,
    last_access_date = atime
  ) %>% 
  mutate(
    file_name = basename(path),
    file_type = str_extract(file_name, "[^\\.]+$"),
    size_mb = size / 2^20
  ) %>% 
  select(
    starts_with("file"), 
    ends_with("date"),
    size_mb
  ) %>% 
  arrange(create_date) %>% 
  print()



new_names <-
  df %>%
  group_by(date = as.Date(create_date)) %>%
  mutate(ord = row_number()) %>%
  ungroup() %>%
  transmute(
    old = file_name,
    new = glue("{date}_{ord}.{file_type}")
  ) %>% 
  print()


walk2(new_names$old[4:324], new_names$new[4:324], file.rename)
file.rename()


SUNSHINE75
#



library(httr)

r <- GET("http://httpbin.org/get")

key <- Sys.getenv("google_api_2.0")
  # https://console.cloud.google.com/apis/credentials?authuser=1&folder=&organizationId=&project=fit-data-286302

GET(
  "https://photoslibrary.googleapis.com/v1/albums",
  add_headers(Authorization = paste("Bearer", key))
)
