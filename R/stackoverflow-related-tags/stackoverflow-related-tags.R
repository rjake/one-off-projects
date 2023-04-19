library(tidyverse)
library(httr)
library(glue)

api_site <- "https://api.stackexchange.com/2.3/"

api_request <- function(tag, endpoint = "info") {
  glue(api_site, "tags/{tag}/{endpoint}?page=1&pagesize=90&site=stackoverflow")
}

# find related tags
r_related <- 
  api_request("r", "related") |> 
  httr::GET() |> 
  content(as = 'parsed')

r_related_df <- 
  map_dfr(r_related$items, flatten_df) |> 
  select(name, r_total = count) |> 
  print()

# get results from related tags
addl_tags <- 
  set_names(r_related_df$name) |> 
  map(
    ~api_request(.x, endpoint = "info") |> 
      httr::GET() |> 
      content(as = 'parsed') |> 
      pluck("items")
  )

addl_tags_df <- 
  map_dfr(addl_tags, pluck, 1) |> 
  select(name, total = count) |> 
  print()

# make plot
r_related_df |> 
  filter(name != "r") |> 
  left_join(addl_tags_df) |> 
  mutate(
    pct = r_total / total,
    name = fct_reorder(name, pct),
    label = scales::percent(pct, accuracy = 1)
  ) |> 
  top_n(n = 20, wt = pct) |> 
  ggplot(aes(y = name)) +
  geom_col(aes(x = 1), fill = "grey80") +
  geom_col(aes(x = pct), fill = "dodgerblue") +
  geom_text(aes(x = pct, label = label), size = 4) +
  scale_x_continuous(labels = scales::percent_format())


# Can also use SEDE (Stack Exchange Data Explorer) 
# https://data.stackexchange.com/stackoverflow/query/1741453?TargetTag=r
