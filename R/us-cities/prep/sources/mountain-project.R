# Mountain Project ----
"-----------------------------------------------------------------
Download popular routes from mountain project:
1) Get xml of states IDs. 
  Go to: https://www.mountainproject.com/route-finder#settings 
    > Location Change > inspect source > copy element & save to xml file

2) Loop through state IDs to find the top 1000 routes in that state: 
  Top routes = Sport 5.9, 1 pitch, 2+ stars

3) Union all states into 1 file

------------------------------------------------------------------"


# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)

mp_html <- read_lines("https://www.mountainproject.com/area/classics")

mp_state_ids <-
  mp_html |> 
  grep(pattern = "/classics/", value = TRUE) |> 
  grep(pattern = "(International|Alaska|Hawaii)", value = TRUE, invert = TRUE) |> 
  as_tibble() |> 
  transmute(
    id = str_extract(value, '(?<=classics/)\\d+'),
    title = str_extract(value, '[^">]+(?=</a>)') 
  )


# Functions ----
mp_download_state <- function(id, overwrite = FALSE) {
  # id <- mp_state_ids$id[1]
  state_name <- mp_state_ids$title[mp_state_ids$id == id]
  file_name <- 
    glue(
      x = state_name |> janitor::make_clean_names(),
      "input/mountain-project/{x}.csv"
    )
  
  if (file.exists(file_name) & !overwrite) {
    return(invisible())
  }
  
  file_location <- 
    glue(
      "https://www.mountainproject.com/\\
      route-finder-export?\\
      selectedIds={id}\\
      &type=rock\\
      &diffMinrock=2300&diffMinboulder=20000&diffMinaid=70000&diffMinice=30000&diffMinmixed=50000\\
      &diffMaxrock=2500&diffMaxboulder=20050&diffMaxaid=75260&diffMaxice=38500&diffMaxmixed=65050\\
      &is_sport_climb=1\\
      &stars=2.8\\
      &pitches=0\\
      &sort1=popularity+desc\\
      &sort2=rating"
    )
  
  df <- read_csv(file_location)
  
  df |> 
    janitor::clean_names() |> 
    select(-your_stars) |> 
    rename(
      x = area_longitude,
      y = area_latitude
    ) |> 
    mutate(
      .before = everything(),
      state = state_name,
      state_id = id
    ) |> 
    write_csv(file_name)
}


round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

location_rev <- function(x) {
  # x <- "Mule Hollow Wall > Big Cottonwood Canyon > Central Wasatch > Wasatch Range > Utah"
  str_split_1(x, " > ") |> 
    rev() |> 
    head(3) |> 
    tail(2) |> 
    paste0(collapse = " > ")
}


# Download all ----
if (FALSE) {
  walk(
    mp_state_ids$id,
    possibly(~mp_download_state(.x)),
    .progress = TRUE
  )
  
}

# Aggregate data ----
all_routes <- 
  list.files(
    "input/mountain-project/",
    full.names = TRUE
  ) |> 
  read_csv()


final_routes <- 
  all_routes |> 
  mutate(
    .before = everything(),
    state = str_to_title(state),
    crag = map_chr(location, ~location_rev(.x))
  )

# Save ----
write_csv(final_routes, "output/mountain-project-popular-routes.csv")




# Not using:

crag_stats <- 
  final_routes |> 
  #filter(state == "North Carolina") |> 
  mutate(
    region_1 = str_extract(crag, "^[^>]+") |> trimws(),
    region_2 = str_extract(crag, "[^>]+$") |> trimws()
  ) |> 
  add_count(
    state, region_1,
    name = "n_region_1"
  ) |> 
  add_count(
    state, region_1, region_2,
    name = "n_region_2"
  ) |> 
  arrange(crag) |> 
  summarise(
    .by = c(state, crag, region_1, region_2, n_region_1, n_region_2),
    x = mean(x),
    y = mean(y),
    n_routes = n(),
    mean_rating = mean(avg_stars),
    median_rating = median(avg_stars)
  )


separate_wider_delim(
  location,
  names = c("state_name", "region", "location", "sub_location_1", "sub_loction_2"),
  delim = " > ",
  too_few = "align_start",
  too_many = "drop"
)
