mod_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "filters-container",
    shiny::textInput(
      inputId = ns("name_filter"),
      label = "Name"
    ),
    shiny::selectInput(
      inputId = ns("gender_filter"),
      label = "Gender", 
      choices = c("", "female", "male", "genderless", "unknown")
    ),
    shiny::selectInput(
      inputId = ns("status_filter"),
      label = "Status",
      choices = c("", "alive", "dead", "unknown")
    )
  )
}

mod_filters_server <- function(id) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {
      filter_settings <- reactive({
        
        list(
          name = input$name_filter,
          status = input$status_filter,
          gender = input$gender_filter
        )
      })
      
      return(filter_settings)
    }
  )
}