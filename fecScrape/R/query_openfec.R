#' Make a request to the OpenFEC API
#'
#' This function constructs the correct URL to send a GET request to the OpenFEC API. Returns the results as a list including both raw and parsed JSON information. OpenFEC query parameters are documented here https://api.open.fec.gov/developers/.
#' @param path A character string of the OpenFEC endpoint.
#' @param query_parameters A list with values of the parameters to use in the request.
#'
#' @export 

query_openfec <- function(path, query_parameters = list()) {
  
  #Construct URL to scrape from
  url <- httr::modify_url('https://api.open.fec.gov/', path = paste0("/v1",path), query = query_parameters)
  
  response <- httr::GET(url)

  parsed_response <- jsonlite::fromJSON(httr::content(response, "text", encoding="UTF-8"), simplifyVector = FALSE)
  
  all <- list(
    raw = response,
    parsed = parsed_response
  )
  
  return(all) # returns a list including both the raw JSON and the parsed JSON
  
}