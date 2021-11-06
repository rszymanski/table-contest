library(shiny)
library(sass)
library(reactable)
library(magrittr)

sass(
  sass_file("styles/main.scss"),
  output = "www/main.css"
)

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$link(href = "main.css", rel = "stylesheet", type = "text/css")
  ),
  title = "Rick and Morty Characters Explorer",
  shiny::div(
    style = "display: flex; flex-direction: column; align-items: center;",
    shiny::div(
      style = "width: 984px; padding: 12px",
      mod_filters_ui(id = "character_table_filters"),
      shiny::div(
        class = "round-box",
        shinyjs::hidden(
          shiny::div(
            id = "table_placeholder",
            class = "table-placeholder",
            shiny::icon("question-circle", class = "placeholder-icon"),
            shiny::div(
              "Oops, looks like there is no character matching your search criteria",
              class = "placeholder-text"
            )
          )
        ),
        shiny::div(
          id = "table_container",
          mod_pagination_controls_ui("chracter_table_pagination_controls"),
          mod_characters_table_ui(id = "characters_table", width = "900px", height = "70vh") %>%
            shinycssloaders::withSpinner(8) 
        )
      )
    )
  )
)

server <- function(input, output, session) {
  character_data_fetcher <- CharacterDataFetcherInMemoryImpl$new(
    api_url = "https://rickandmortyapi.com/api/character"
  )
  page_number <- shiny::reactiveVal(1)
  
  filter_settings <- mod_filters_server(id = "character_table_filters")

  
  observeEvent(current_data(), {
    shiny::req(current_data())
    
    should_table_be_showed <- nrow(current_data()$data) > 0
    shinyjs::toggle(id = "table_placeholder", condition = !should_table_be_showed)
    shinyjs::toggle(id = "table_container", condition = should_table_be_showed)
  })
  
  observeEvent(filter_settings(), {
    page_number(1)
  })

  current_data <- reactive({
    shiny::req(page_number())
    shiny::req(filter_settings())

    character_data_fetcher$fetch_page(
      page_number = page_number(),
      filter_settings = list(gender = "female")
    )
  })

  total_pages <- reactive({
    shiny::req(current_data)
    current_data()$total_pages
  })

  mod_characters_table_server(id = "characters_table", current_data = current_data)
  mod_pagination_controls_server(id = "chracter_table_pagination_controls", page_number = page_number, total_pages = total_pages)
}

shinyApp(ui, server)
