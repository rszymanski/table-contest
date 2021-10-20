library(shiny)
library(reactable)
library(magrittr)

send_request <- function(page_number, filter_settings) {
  filter_settings <- Filter(function(setting) setting != "", filter_settings)
  filter_query <-  sprintf(
      fmt = "%s=%s",
      names(filter_settings),
      filter_settings
    ) %>% paste(collapse = "&")
  
  url <- glue::glue("https://rickandmortyapi.com/api/character?page={page_number}&{filter_query}")
  api_response <- httr::GET(url = url)
  response_content <- httr::content(api_response)
  
  retrieved_data <- data.table::rbindlist(
    lapply(
      response_content$results,
      function(character) {
        data.frame(
          image = character$image,
          name = character$name,
          status = character$status,
          species = character$species,
          type = character$type,
          gender = character$gender
        )
      }
    )
  )
  list(
    total_pages = response_content$info$pages,
    data = retrieved_data,
    total_pages = response_content$info$count,
    next_page_url = response_content$info$`next`
  )
}

create_character_table <- function(table_data) {
  reactable::reactable(
    data = table_data,
    columns = list(image = reactable::colDef(cell = function(value) {
      image <- img(src = value, alt = value, height = "100px")
    })),
    pagination = FALSE
  )
}

ui <- shiny::fluidPage(
  title = "Rick and Morty Characters Explorer",
  shiny::fluidRow(
    shiny::div(
      style = "display: inline-block",
        shiny::actionButton("prev_page", label = "", icon = shiny::icon("arrow-left")),
        shiny::textOutput("current_page", inline = TRUE),
        shiny::actionButton("next_page", label = "", icon = shiny::icon("arrow-right")),
        shiny::textOutput("total_pages", inline = TRUE)
    )
  ),
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shinyBS::bsCollapse(
        id = "filters_panel", 
        shinyBS::bsCollapsePanel(
          title = shiny::tagList(shiny::icon("filter"), "Query Filters"),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::textInput(inputId = "name_filter", label = "Name"),
              shiny::selectInput(inputId = "status_filter", label = "Status", choices = c("", "alive", "dead", "unknown"))
            ),
            shiny::column(
              width = 4,
              shiny::textInput(inputId = "species_filter", label = "Species"),
              shiny::textInput(inputId = "type_filter", label = "Type")
            ),
            shiny::column(
              width = 4,
              shiny::selectInput(inputId = "gender_filter", label = "gender", choices = c("", "female", "male", "genderless", "unknown"))
            )
          )
        )
      )
    )
  ),
  reactable::reactableOutput(outputId = "data_table") %>% 
    shinycssloaders::withSpinner(type = 8)
)

server <- function(input, output, session) {
  page_number <- shiny::reactiveVal(1)
  total_pages <- shiny::reactiveVal()
  next_page_url <- shiny::reactiveVal()
  
  observeEvent(input$prev_page, {
    current_page_number <- page_number()
    page_number(current_page_number - 1)
  })

  observeEvent(input$next_page, {
    current_page_number <- page_number()
    page_number(current_page_number + 1)
  })

  filter_settings <- reactive({
    
    page_number(1)
    
    list(
      name = input$name_filter,
      status = input$status_filter,
      species = input$species_filter,
      type = input$type_filter,
      gender = input$gender_filter
    )
  })

  current_data <- reactive({
    shiny::req(page_number())
    shiny::req(filter_settings())

    send_request(
      page_number = page_number(),
      filter_settings = filter_settings()
    )
  })

  total_pages <- reactive({
    shiny::req(current_data)
    current_data()$total_pages
  })

  output$current_page <- shiny::renderText({
    page_number()
  })

  output$total_pages <- shiny::renderText({
    total_pages()
  })

  output$data_table <- reactable::renderReactable({
    shiny::req(current_data())
    
    create_character_table(
      table_data = current_data()$data
    )
  })
}

shinyApp(ui, server)
