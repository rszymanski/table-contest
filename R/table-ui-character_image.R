create_character_image <- function(image_url) {
  shiny::img(
    src = image_url, 
    alt = image_url, 
    class = "character-image"
  )
}