#' Make a request to the OpenFEC API
#'
#' This function constructs the correct URL to send a GET request to the OpenFEC API. Returns the results a parsed JSON list. OpenFEC query parameters are documented here https://api.open.fec.gov/developers/.
#' @param path A character string of the OpenFEC endpoint.
#' @param query_parameters A list with values of the parameters to use in the request.
#'
#' @export 

query_openfec <- function(path, query_parameters = list()) {
  
  #Construct URL to scrape from
  url <- httr::modify_url('https://api.open.fec.gov/', path = paste0("/v1",path), query = query_parameters)
  
  # Querty Open API
  response <- httr::GET(url)

  # Parse response into readable list
  parsed_response <- jsonlite::fromJSON(httr::content(response, "text", encoding="UTF-8"), simplifyVector = FALSE)
  
  return(parsed_response) # returns a list including both the raw JSON and the parsed JSON
}