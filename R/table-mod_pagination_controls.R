mod_pagination_controls_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "pagination-container",
    shiny::span(id = ns("pages_text"), class = "pages-text"),
    shiny::actionButton(
      inputId = ns("goto_first_page"), 
      class = "pagination-control", 
      label = "", 
      icon = shiny::icon("angle-double-left")
    ),
    shiny::actionButton(
      inputId = ns("prev_page"),
      class = "pagination-control",
      label = "", 
      icon = shiny::icon("angle-left")
    ),
    shiny::actionButton(
      inputId = ns("next_page"),
      class = "pagination-control",
      label = "",
      icon = shiny::icon("angle-right")
    ),
    shiny::actionButton(
      inputId = ns("goto_last_page"),
      class = "pagination-control", 
      label = "", 
      icon = shiny::icon("angle-double-right")
    )
  )
}

mod_pagination_controls_server <- function(id, page_number, total_pages) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {
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
      
      observeEvent(list(page_number(), total_pages()), {
        shiny::req(page_number())
        shiny::req(total_pages())
        
        left_bound_condition <- page_number() > 1
        shinyjs::toggleState(id = "prev_page", condition = left_bound_condition)
        shinyjs::toggleState(id = "goto_first_page", condition = left_bound_condition)
        
        right_bound_condition <- page_number() < total_pages()
        shinyjs::toggleState(id = "next_page", condition = right_bound_condition)
        shinyjs::toggleState(id = "goto_last_page", condition = right_bound_condition)
      })
      
      observeEvent(list(page_number(), total_pages()), {
        page_number <- page_number()
        total_pages <- total_pages()
        text <- glue::glue("{page_number} of {total_pages} pages")
        shinyjs::html(id = "pages_text", html = text)
      })
    }
  )
}