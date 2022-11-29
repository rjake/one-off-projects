library(RSQLite)
library(tidyverse)
library(glue)
library(janitor)

get_metadata <- function(tbl, con) {
  df <- dbGetQuery(con, glue('select * from "{tbl}" limit 10'))
  
  # if (nrow(df) == 0) {
  #   return(NULL)
  # }
  
  tibble(
    table = janitor::make_clean_names(tbl),
    field = names(df) |> janitor::make_clean_names(),
    seq_num = seq_along(df),
    data_class = sapply(df, typeof)
  ) 
}


# Northwind ----
con <- RSQLite::dbConnect(drv = SQLite(), "inputs/northwind.db")
all_tables <- dbListTables(con)
all_metadata <- map_dfr(all_tables, get_metadata, con)
dbDisconnect(con)
saveRDS(all_metadata, "northwind.Rds")


# Chinook ----
con <- RSQLite::dbConnect(drv = SQLite(), "inputs/Chinook_Sqlite.sqlite")
all_tables <- dbListTables(con)
all_metadata <- map_dfr(all_tables, get_metadata, con)
dbDisconnect(con)
saveRDS(all_metadata, "chinook.Rds")


# Salika ----
# https://github.com/bradleygrant/sakila-sqlite3/blob/main/sakila_master.db
con <- RSQLite::dbConnect(drv = SQLite(), "inputs/sakila_master.db")
all_tables <- dbListTables(con)
all_metadata <- map_dfr(all_tables, get_metadata, con)

final_metadata <-
  all_metadata |> 
  mutate(
    field = case_when(
      table == "customer_list" & field == "id" ~ "customer_id",
      table == "customer_list" & field == "notes" ~ "active",
      table == "film_list" & field == "fid" ~ "film_id",
      table == "film_list" & field == "price" ~ "rental_rate",
      table == 'category' & field == "name" ~ "category_name",
      table == 'film_list' & field == "category" ~ "category_name",
      table == "staff_list" & field == "id" ~ "staff_id",
      table == "staff_list" & field == "sid" ~ "store_id",
      field == "postal_code" ~ "zip_code",
      table == "sales_by_film_category" & field == "category" ~ "category_name",
      TRUE ~ field
    )
  )

dbDisconnect(con)
saveRDS(final_metadata, "salika.Rds")
