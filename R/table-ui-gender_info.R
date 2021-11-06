create_gender_column_content <- function(gender_value) {
  icon_name <- switch(
    gender_value,
    Male = "mars",
    Female = "venus",
    Genderless = "genderless",
    unknown = "question"
  )
  
  shiny::div(
    class = "gender-cell-content",
    shiny::icon(icon_name, class = glue::glue_safe("{icon_name}-icon")),
    gender_value
  )
}
