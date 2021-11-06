CharacterDataFetcherFactory <- R6::R6Class(
  classname = "CharacterDataFetcherFactory",
  public = list(
    initialize = function(api_url) {
      private$api_url <- api_url
      invisible(self)
    },
    create = function(data_fetcher_type) {
      switch(
        data_fetcher_type,
        paginated = CharacterDataFetcherPaginatedImpl$new(private$api_url),
        in_memory = CharacterDataFetcherInMemoryImpl$new(private$api_url)
      )
    }
  ),
  private = list(
    api_url = NA
  )
)