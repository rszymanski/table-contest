CharacterDataFetcherInMemoryImpl <- R6::R6Class(
  classname = "CharacterDataFetcherInMemoryImpl",
  inherit = CharacterDataFetcher,
  public = list(
    initialize = function(api_url) {
      private$api_url <- api_url
    },
    fetch_page = function(page_number, filter_settings) {
      private$fetch_data(filter_settings)
      
      data_page <- private$in_memory_data$data %>% 
        head(page_number * 20) %>% 
        tail(20)
      
      list(
        data = data_page,
        total_pages = private$in_memory_data$total_pages
      )
    }
  ),
  private = list(
    api_url = NA,
    current_filter_query = "",
    in_memory_data = NA,
    fetch_single_page = function(page_number, filter_query) {
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
        total_pages = response_content$info$count,
        next_page_url = response_content$info$`next`
      )
    },
    fetch_data = function(filter_settings) {
      filter_settings <- Filter(function(setting) setting != "", filter_settings)
      filter_query <-  sprintf(
        fmt = "%s=%s",
        names(filter_settings),
        filter_settings
      ) %>% paste(collapse = "&")
      
      if (is.na(private$in_memory_data) || private$current_filter_query != filter_query) {
        private$current_filter_query <- filter_query
        first_page_data <- private$fetch_single_page(
          page_number = 1, filter_query = filter_query
        )
        
        retrieved_data <- lapply(seq_len(first_page_data$total_pages), function(page_number) {
          private$fetch_single_page(page_number, filter_query)$data
        }) %>% data.table::rbindlist()
        
        private$in_memory_data <- list(
          data = retrieved_data,
          total_pages = first_page_data$total_pages
        )
      }
    } 
  )
)