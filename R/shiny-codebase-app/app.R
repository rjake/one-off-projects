library(shiny)
library(shinyWidgets)

library(tidyverse)
library(glue)
library(networkD3)

if (interactive()) {
  setwd(dirname(.rs.api.getSourceEditorContext()$path))
  reactiveConsole(enabled = TRUE)
  
  dummy_input <- list(
    action = "Filter",
    build = TRUE,
    export = "Exported only",
    fn_names = c("bootstrapPage", "fluidPage", "shinyApp"),
    node_distance = 3
  )
  input <- dummy_input
}

load(".RData") # comes from https://github.com/rjake/common-files/blob/99db7b65342041be491cc09e7105a8d8e51e2002/package-dependencies.R

ui <- {
  fluidPage(
    titlePanel("Hello Shiny Ecosystem!"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        width = 3,
        selectInput(
          inputId = "fn_names",
          label = "function",
          choices = sort(export_list),
          selected = 
            c("bootstrapPage", "fluidPage", "shinyApp"),
            #c(""),
          multiple = TRUE
        ),
        radioGroupButtons(
          inputId = "action",
          label = NULL,
          choices = c("Filter", "Highlight")
        ),
        radioGroupButtons(
          inputId = "export",
          label = NULL,
          choices = c("Exported only", "All functions")
        ),
        actionBttn(
          inputId = "build", 
          label = "build", 
          icon = icon("project-diagram"), 
          color = "primary",
          style = "simple"
        ),
        hr(),
        sliderInput(
          inputId = "node_distance",
          label = "Node Distance",
          min = 1,
          max = 6,
          step = 1,
          value = 2,
          ticks = FALSE
        ),
        br(),
        p("Codebase as of 10/28/2022")
      ),
      mainPanel = mainPanel(
        forceNetworkOutput("network", width = "100%", height = "800px")
      )
    )
  )
}

#

server <- function(input, output, session) {
  network_df <- eventReactive(input$build, ignoreNULL = FALSE, {
    df <- all_nodes
    #browser()

    if (!is.null(input$fn_names) && input$action == "Filter") {
      df <- filter(df, used %in% input$fn_names | fn %in% input$fn_names)
    }

    if (input$export == "Exported only") {
      df <- filter(df, used %in% export_list & fn %in% export_list)
    }

    res <- 
      prep_for_network(
        from = df$used,
        to = df$fn,
        link_size = 3
      )
    
    res$nodes <- 
      res$nodes |> 
      mutate(
        color = case_when(
          name %in% input$fn_names ~ "orangered",
          group == TRUE ~ "dodgerblue",
          TRUE ~ "grey"
        )
      )
    
    res
  })
  
  
  observeEvent(input$export, {
    if (input$export == "Exported only") {
      use_fns <- export_list
    } else {
      use_fns <- pkg_fns
    }
    
    fn_display <- 
      use_fns |> 
      sort() |> 
      keep(str_detect, "^[[:alpha:]]{2,}")
    
    updateSelectInput(
      inputId = "fn_names",
      label = "select function",
      choices = fn_display,
      selected = intersect(input$fn_names, fn_display)
    )
  })
  
  
  # create network chart
  output$network <- renderForceNetwork({
    df <- network_df()
    color_palette <-
      glue(
        'd3.scaleOrdinal([{colors}])',
        colors = 
          glue("'{unique(df$nodes$color)}'") |> 
          glue_collapse(", ")
      )
    
    forceNetwork(
      Links = df$links,
      Nodes = df$nodes,
      Source = "source",
      Target = "target",
      Value = "n",
      NodeID = "name",
      Group = "color",
      arrows = TRUE,
      fontSize = 14,
      colourScale = JS(color_palette),
      opacityNoHover = 1,
      #linkDistance = JS("function(d){return d.value * 25}"),
      #linkDistance = 100,
      #height = 1500,
      #width = 2500,
      #bounded = TRUE,
      charge = -c(10, 50, 100, 200, 400, 500)[input$node_distance],
      zoom = TRUE
    )
  })
}

#rm(input)
shinyApp(ui, server)
#input <- dummy_input
