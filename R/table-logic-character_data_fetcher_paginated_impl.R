CharacterDataFetcherPaginatedImpl <- R6::R6Class(
  classname = "CharacterDataFetcherPaginatedImpl",
  inherit = CharacterDataFetcher,
  public = list(
    initialize = function(api_url) {
      private$api_url <- api_url
    },
    fetch_page = function(page_number, filter_settings) {
      filter_settings <- Filter(function(setting) setting != "", filter_settings)
      filter_query <-  sprintf(
        fmt = "%s=%s",
        names(filter_settings),
        filter_settings
      ) %>% paste(collapse = "&")
      
      url <- glue::glue("{private$api_url}?page={page_number}&{filter_query}")
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
        page_count = response_content$info$count,
        next_page_url = response_content$info$`next`
      )
    }
  ),
  private = list(
    api_url = NA
  )
)