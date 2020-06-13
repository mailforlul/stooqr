#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

stooq_get <- function(symbol, ...) {

  if (missing(symbol)) symbol <- NULL

  # Setup
  dots <- list(...)
  ua   <- httr::user_agent("https://github.com/mailforlul")

  # Overides
  dots$s      <- symbol

  # Generate URL
  url_params <- stringr::str_c(names(dots), dots, sep = "=", collapse = "&")
  url <- glue::glue("https://stooq.pl/q/d/l/?{url_params}&i=d")

  # call call
  response <- httr::GET(url, ua)

  # Handle bad status codes errors
  if (!(httr::status_code(response) >= 200 && httr::status_code(response) < 300)) {
    stop(httr::content(response, as="text"), call. = FALSE)
  }

  # # Clean data
  content_type <- httr::http_type(response)
  if (content_type == "text/plain") {
    # CSV Returned - Good Call - Time Series CSV file
    content <- httr::content(response, as = "text", encoding = "UTF-8") %>%
      readr::read_csv()
  }

  # Fix names
  names(content) <- names(content) %>%
    stringr::str_replace_all("[0-9]+\\. ", "") %>%
    make.names() %>%
    stringr::str_replace_all("\\.", "_") %>%
    tolower()

  content <- cbind(symbol, content)
  # Return desc
  if ("timestamp" %in% names(content)) content <- content %>% dplyr::arrange(timestamp)

  return(content)

}
