mod_characters_table_ui <- function(id, width, height) {
  ns <- shiny::NS(id)
  shiny::tagList(
    reactable::reactableOutput(
      outputId = ns("data_table"),
      width = width, 
      height = height
    ) 
  )
}

mod_characters_table_server <- function(id, current_data) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {
      output$data_table <- reactable::renderReactable({
        shiny::req(current_data())
        shiny::req(nrow(current_data()$data) > 0)
        
        create_character_table(
          table_data = current_data()$data
        )
      })
    }
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
        cell = create_character_image
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
        cell = create_gender_column_content
      ),
      status = reactable::colDef(
        name = "Status",
        cell = create_status_badge
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