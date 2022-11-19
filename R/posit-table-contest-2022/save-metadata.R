con <- RSQLite::dbConnect(drv = SQLite(), "databases/northwind.db")
all_tables <- dbListTables(con)

get_metadata <- function(tbl, con) {
  df <- dbGetQuery(con, glue('select * from "{tbl}" limit 10'))
  
  tibble(
    table_name = janitor::make_clean_names(tbl),
    field = names(df) |> janitor::make_clean_names(),
    seq_num = seq_along(df),
    data_class = sapply(df, typeof)
  )
}

all_metadata <- 
  map_dfr(
    .x = all_tables, 
    .f = possibly(get_metadata, NULL), 
    con = con
  )

dbDisconnect(con)

saveRDS(all_metadata, "all_metadata.Rds")




library(reactable)
df <- MASS::Cars93[, c("Manufacturer", "Model", "Type", "Price")]










output$table <- DT::renderDT({
  view_by_field <- input$search_type == "by field"
  operator <- ifelse(view_by_field, "|", ".*")
  base_data <- raw_data
  
  # if longer string is entered it needs to be split
  columns_listed <-
    ifelse(col_search() == "", ".*", col_search()) |> 
    str_split(" ") |>
    pluck(1) |> 
    trimws() |>
    sort() |> 
    glue_collapse(operator)
  
  column_regex <- glue("(?i){columns_listed}")
  
  # which dataset to use
  if (!view_by_field) {  
    base_data <- 
      raw_data |>
      group_by_at(vars(database_name:table_name)) |>
      summarise(field = paste(sort(field), collapse = " • ")) |>
      ungroup()
  }
  
  prep_df <- 
    base_data |> 
    filter(
      str_detect(database_name, input$db_name),
      str_detect(table_name, table_search()),
      str_detect(field, column_regex)
    )
  
  
  # final manipulation
  if (view_by_field) {
    base_datatable(prep_df)
    
  } else {
    # bold text if column search != ".*"
    if(columns_listed != ".*") {
      final_df <- 
        prep_df |> 
        mutate(field = map_chr(field, extract_bold, col_search()))
    }
    
    # final_df |> 
    #   reactable::reactable(
    #     columns = list(
    #       field = colDef(html = TRUE)
    #     )
    #   )
    
    
    base_datatable(
      final_df,
      autoWidth = TRUE,
      columnDefs = list(
        list(targets = c(0), visible = TRUE, width = "10%"),
        list(targets = c(1), visible = TRUE, width = "10%"),
        list(targets = c(2), visible = TRUE, width = "80%")
      )
    )
  }
  
}, server = TRUE)












reactable(
  data = df,
  columns = list(
    Manufacturer = colDef(
      filterable = TRUE,
      # Filter by case-insensitive text match
      filterMethod = JS("function(rows, columnId, filterValue) {
        const pattern = new RegExp(filterValue, 'i')

        return rows.filter(function(row) {
          return pattern.test(row.values[columnId])
        })
      }")
    )
  ),
  defaultPageSize = 5
)

custom_filter <- function(cond) {
  logic <-
    switch(
      cond,
      and = "const pattern = new RegExp(filterValue, 'i')",
      or  = 
        "const pattern = new RegExp(filterValue, 'i')
        "
    )
  
  glue(
    "function(rows, columnId, filterValue) {
        {logic}
  
        return rows.filter(function(row) {
          return pattern.test(row.values[columnId])
        })
      }"
  )
  }
  colDef(
    filterable = TRUE,
    # Filter by case-insensitive text match
    filterMethod = 
  )
}


df <- raw_data

all_defs <-
  set_names(names(df)) |> 
  map(custom_filter)


reactable(
  data = df,
  columns = all_defs,
  defaultPageSize = 5
)

htmltools::browsable(
  tagList(
    tags$button(
      tagList(fontawesome::fa("download"), "Download as CSV"),
      onclick = "Reactable.downloadDataCSV('download-table', 'data.csv')"
    ),
    reactable(
      data = df,
      searchable = TRUE,
      defaultPageSize = 5,
      elementId = "download-table"
    )
  )
)
