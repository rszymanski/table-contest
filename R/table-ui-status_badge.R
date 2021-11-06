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
