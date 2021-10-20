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
  color <- switch(
    status,
    Alive = "hsl(116, 60%, 90%)",
    unknown = "hsl(230, 70%, 90%)",
    Dead = "hsl(350, 70%, 90%)"
  )
  
  text_color <- switch(
    status,
    Alive = "hsl(116, 30%, 25%)",
    unknown = "hsl(230, 45%, 30%)",
    Dead = "hsl(350, 45%, 30%)"
  )
  
  shiny::span(
    status,
    style = list(
      background = color,
      `border-radius` = "16px",
      padding = "4px 14px",
      color = text_color
    )
  )
}

create_character_table <- function(table_data) {
  reactable::reactable(
    data = table_data,
    height = "80vh",
    defaultColDef = reactable::colDef(
      headerStyle = list(`text-transform` = "uppercase", color = "hsl(203, 15%, 47%)")
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
            height = "80px",
            style = "border-radius: 50%"
          )
        }
      ),
      name = reactable::colDef(
        name = "Name",
        cell = function(value, row_index) {
          shiny::div(
            shiny::div(
              style = "font-weight: 700; font-size: 16px;",
              value
            ),
            shiny::div(
              style = "color: hsl(201, 23%, 34%); font-size: 16px;",
              table_data[row_index, ]$species
            ),
            shiny::div(
              style = "color: hsl(203, 15%, 47%); font-size: 14px",
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
          
          icon_color <- switch(
            value,
            Male = "lightblue",
            Female = "pink",
            Genderless = "gray",
            unknown = "hsl(31, 100%, 70%)"
          )
          
          shiny::div(
            style = list(`font-size` = "16px"),
            shiny::icon(icon_name, style = list(color = icon_color, `margin-right` = "6px")),
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
    pagination = FALSE
  )
}

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style("
      body { background-color: hsl(210, 0%, 88%) }
      
      .round-box {
        background-color: white;
        border-radius: 28px;
      }
      
      .pagination-control {
        border: 0px;
        outline: 0px!important;
        font-size: 18px;
        color: hsl(203, 15%, 47%);
      }
      
      .pagination-control:active {
        color: hsl(203, 15%, 47%);
      }
      
      .pagination-control:focus {
        background-color: transparent;
        color: hsl(203, 15%, 47%);
      }
      
      #pages_text {
        color: hsl(203, 15%, 47%);
        font-weight: 600;
        margin-right: 24px;
      }
    ")
  ),
  title = "Rick and Morty Characters Explorer",
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
  ),
  shiny::div(
    style = "display: flex; justify-content: center",
    shiny::div(
      class = "round-box",
      style = "padding: 32px;",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center",
        shiny::textOutput("pages_text", inline = TRUE),
        shiny::actionButton("goto_first_page", class = "pagination-control", label = "", icon = shiny::icon("angle-double-left")),
        shiny::actionButton("prev_page", class = "pagination-control", label = "", icon = shiny::icon("angle-left")),
        shiny::actionButton("next_page", class = "pagination-control", label = "", icon = shiny::icon("angle-right")),
        shiny::actionButton("goto_last_page", class = "pagination-control", label = "", icon = shiny::icon("angle-double-right"))
      ),
      reactable::reactableOutput(outputId = "data_table", width = "900px", height = "80vh") %>% 
        shinycssloaders::withSpinner(type = 8)
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

  output$pages_text <- shiny::renderText({
    page_number <- page_number()
    total_pages <- total_pages()
    glue::glue("{page_number} of {total_pages} pages")
  })

  output$data_table <- reactable::renderReactable({
    shiny::req(current_data())
    
    create_character_table(
      table_data = current_data()$data
    )
  })
}

shinyApp(ui, server)
