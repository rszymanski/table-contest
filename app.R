library(shiny)
library(sass)
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
          gender = character$gender,
          status = character$status,
          species = character$species,
          type = character$type
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

create_status_badge <- function(status) {
  style_name <- switch(
    status,
    Alive = "alive",
    unknown = "unknown",
    Dead = "dead"
  )
  
  class_name <- glue::glue_safe("status-badge-{style_name}")
  
  shiny::div(
    shiny::span(status, class = class_name)
  )
}

create_character_table <- function(table_data) {
  reactable::reactable(
    data = table_data,
    height = "70vh",
    defaultColDef = reactable::colDef(
      headerClass = "character-table-header"
    ),
    sortable = FALSE,
    columns = list(
      image = reactable::colDef(
        name = "",
        maxWidth = 88,
        cell = function(value) {
          image <- img(
            src = value, 
            alt = value, 
            class = "character-image"
          )
        }
      ),
      name = reactable::colDef(
        name = "Name",
        cell = function(value, row_index) {
          shiny::div(
            shiny::div(
              class = "name-container",
              value
            ),
            shiny::div(
              class = "species-container",
              table_data[row_index, ]$species
            ),
            shiny::div(
              class = "type-container",
              table_data[row_index, ]$type
            )
          )
        }
      ),
      type = reactable::colDef(show = FALSE),
      species = reactable::colDef(show = FALSE),
      gender = reactable::colDef(
        name = "Gender",
        cell = function(value) {
          icon_name <- switch(
            value,
            Male = "mars",
            Female = "venus",
            Genderless = "genderless",
            unknown = "question"
          )
          
          shiny::div(
            class = "gender-cell-content",
            shiny::icon(icon_name, class = glue::glue_safe("{icon_name}-icon")),
            value
          )
        }
      ),
      status = reactable::colDef(
        name = "Status",
        cell = function(value) {
          create_status_badge(value)
        }
      )
    ),
    pagination = FALSE,
    theme = reactable::reactableTheme(
      cellStyle = list(
        display = "flex",
        flexDirection = "column",
        justifyContent = "center"
      )
    )
  )
}

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
      shiny::div(
        class = "filters-container",
        shiny::textInput(inputId = "name_filter", label = "Name"),
        shiny::selectInput(inputId = "gender_filter", label = "Gender", choices = c("", "female", "male", "genderless", "unknown")),
        shiny::selectInput(inputId = "status_filter", label = "Status", choices = c("", "alive", "dead", "unknown"))
      ),
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
          shiny::div(
            class = "pagination-container",
            shiny::textOutput("pages_text", inline = TRUE),
            shiny::actionButton("goto_first_page", class = "pagination-control", label = "", icon = shiny::icon("angle-double-left")),
            shiny::actionButton("prev_page", class = "pagination-control", label = "", icon = shiny::icon("angle-left")),
            shiny::actionButton("next_page", class = "pagination-control", label = "", icon = shiny::icon("angle-right")),
            shiny::actionButton("goto_last_page", class = "pagination-control", label = "", icon = shiny::icon("angle-double-right"))
          ),
          reactable::reactableOutput(outputId = "data_table", width = "900px", height = "70vh") %>%
            shinycssloaders::withSpinner(8) 
        )
      )
    )
  )
)

server <- function(input, output, session) {
  page_number <- shiny::reactiveVal(1)
  total_pages <- shiny::reactiveVal()
  next_page_url <- shiny::reactiveVal()
  
  observeEvent(input$goto_first_page, {
    page_number(1)
  })
  
  observeEvent(input$goto_last_page, {
    page_number(total_pages())
  })
  
  observeEvent(input$prev_page, {
    current_page_number <- page_number()
    page_number(current_page_number - 1)
  })

  observeEvent(input$next_page, {
    current_page_number <- page_number()
    page_number(current_page_number + 1)
  })
  
  observeEvent(page_number(), {
    shiny::req(page_number())
    shiny::req(total_pages())
    
    left_bound_condition <- page_number() > 1
    shinyjs::toggleState(id = "prev_page", condition = left_bound_condition)
    shinyjs::toggleState(id = "goto_first_page", condition = left_bound_condition)
    
    right_bound_condition <- page_number() < total_pages()
    shinyjs::toggleState(id = "next_page", condition = right_bound_condition)
    shinyjs::toggleState(id = "goto_last_page", condition = right_bound_condition)
  })
  
  observeEvent(current_data(), {
    shiny::req(current_data())
    
    should_table_be_showed <- nrow(current_data()$data) > 0
    shinyjs::toggle(id = "table_placeholder", condition = !should_table_be_showed)
    shinyjs::toggle(id = "table_container", condition = should_table_be_showed)
  })

  filter_settings <- reactive({
    
    page_number(1)
    
    list(
      name = input$name_filter,
      status = input$status_filter,
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

  output$pages_text <- shiny::renderText({
    page_number <- page_number()
    total_pages <- total_pages()
    glue::glue("{page_number} of {total_pages} pages")
  })

  output$data_table <- reactable::renderReactable({
    shiny::req(current_data())
    shiny::req(nrow(current_data()$data) > 0)
    
    create_character_table(
      table_data = current_data()$data
    )
  })
}

shinyApp(ui, server)
