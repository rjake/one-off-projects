"-----------------------------------------------------------------

------------------------------------------------------------------"

# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)
library(googleway)

googleway::set_key(Sys.getenv("google_maps"))

# Functions ----
get_gym_info <- function(state_name, overwrite = FALSE) {
  # state_name <- "Tennessee"
  file_name <- 
    glue(
      x = state_name |> janitor::make_clean_names(),
      "input/gyms/{x}.csv"
    )
  
  if (file.exists(file_name) & !overwrite) {
    return(invisible())
  }
  
  gym_search <- glue("Rock climbing gym in {state_name}")
  
  res <- google_places(search_string = gym_search)
  all_results <- res$results
  
  next_page_token <- res[["next_page_token"]]
  
  while( !is.null(next_page_token) ) {
    res <- google_places(search_string = gym_search, page_token = next_page_token)
    new_df <- res$results
    
    all_results <- bind_rows(all_results, new_df)
    
    next_page_token <- res[["next_page_token"]]
  }

  df <- 
    all_results |> 
    transmute(
      gym_name = name,
      rating = rating,
      n_votes = user_ratings_total,
      formatted_address = formatted_address,
      x = geometry$location$lng,
      y = geometry$location$lat,
      gym_id = place_id
    )
  
  write_csv(df, file_name)
}

# Download all ----
if (FALSE) {
  c(state.name, "washington dc") |> 
    map(
      possibly(~get_gym_info(.x), "error")
    )
}

# Aggregate data ----
all_gyms <- 
  list.files("input/gyms/", full.names = TRUE) |> 
  read_csv()

# Save ----
all_gyms |> 
  distinct() |> 
  write_csv("output/climbing-gyms.csv")
