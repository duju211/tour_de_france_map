source("libraries.R")

sf_stages <- read_rds("tdf_stages.rds")

ui <- page_sidebar(
  title = "Tour de France Explorer",
  sidebar = sidebar(
    title = "Map Controls",
    varSelectInput(
      "col_var", "Select Color Variable",
      select(sf_stages, where(is.character)), selected = "edition"
    )
  ),
  card(
    card_header("Map"),
    leafletOutput("stage_map")
  ),
  card(
    card_header("Table"),
    reactableOutput("stage_tbl")
  )
)

server <- function(input, output, session) {
  selected <- reactive(getReactableState("stage_tbl", "selected"))
  
  selected_stages <- reactive({
    req(!is.null(selected()))
    slice(sf_stages, selected())
  })
  
  output$stage_map <- renderLeaflet({
    stages_leaflet(selected_stages(), as.character(input$col_var))
  })
  
  output$stage_tbl <- renderReactable({
    tdf_overview_tbl(sf_stages)
  })
}

shinyApp(ui, server)