setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(glue)
library(magick)

show_image <- function(file) {
  img <- image_read(file)
  plot(img) # or print(img)
}


if (FALSE) {
  takeout_files <-
    list.files(path = "inputs/photos/",
      pattern = "^takeout-20250730",
      full.names = TRUE
    )
  
  walk(
    .x = takeout_files[-1],
    .f = 
      ~unzip(
        zipfile = .x,
        exdir = tools::file_path_sans_ext(.x)
      )
  )
  unzip(
    takeout_files
  )
}

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

files_df |> count(ext)

# get file size info
file_sizes <-
  files_df$file  |>  
  #head() |> 
  file.size() 

file_info <-
  files_df |> 
  mutate(
    size_mb = file_sizes / 1e6
  )

relevant_files <-
  file_info |> 
  filter(
    !str_detect(
      file,
      "\\d/Takeout/Google Photos/Archive"
    )
  ) |> 
  filter(
    # drop files that are duplicated in albums
    .by = c(base_name, size_mb),
    # only exists once
    n() == 1
    # or a copy is in 'Photos from 20__'
    | str_detect(file, "Photos from \\d{4}")
  ) |> 
  mutate(
    dir = 
      str_remove(file, "inputs.*/Takeout/Google Photos/") |> 
      dirname()
  )
  
table(relevant_files$dir)

relevant_files |> 
  filter()

  
