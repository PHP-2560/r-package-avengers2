#' Search for political candidates in FEC 
#' 
#' This function constructs searches for candidates listed in the FEC based on input parameters. 
#' @param api_key An API key required to use OpenFEC
#' @param state A two-letter acronym which specifies the state to search (e.g., "TX")
#' @param election_year The year the election occured
#' @param office Specifies which office the candidate ran for: "S" = Senate, "H" = House, "G" = Governor, "P" = President
#' @param candidate_status Specifies whether the candidate is active during that election. Defaults to "C" for current. 
#' 
#' @import purrr dplyr magrittr
#' 
#' @export 

#Construct FEC URL to get data from
query_candidate_list <- function(api_key = NULL, state = NULL, election_year = NULL, office = NULL, candidate_status = "C") {
  if (is.null(api_key)) {
    stop('An API key is required. Obtain one at https://api.data.gov/signup.')
  }

  #1: Find candidates
  
  query_param <- list(
    api_key = api_key, 
    state = state, 
    election_year = election_year, 
    office = office, 
    candidate_status = candidate_status, 
    page = 1, 
    per_page = 100
  )
  
  query_param <- query_param[!sapply(query_param, is.null)] # fill in query inputs from the function inputs 
  
  #Set up for responses to our requests.
  responses <- list()

  responses[[1]] <- query_openfec(path = "/candidates/search/", query_parameters = query_param)

  total_pages <- responses[[1]][["pagination"]][["pages"]]

  total_count <- responses[[1]][["pagination"]][["count"]]

  message(paste0("Candidates found: ", total_count))
  
  #Automate Pagination, only run if necessary
  if(total_pages > 1){

     message(paste0("There are ",total_pages," pages of results to get."))

     for(i in 2:total_pages){

        if(i == 2 | i %% 10 == 0 | i == total_pages){
           message("On page ", i,"/",total_pages)
        }

        ### Rate Limit Controls
        Sys.sleep(.5) #With an upgraded key, max limit is 120 calls per minute.

        #Update the page in our query and send another request
        query_param$page <- i

        responses[[i]] <- query_openfec(path = "/candidates/search/", query_parameters = query_param)

     }

  }
  
  # CLEAN RESPONSES RETRIEVED 
  
  tidy_candidates <- purrr::map(responses, function(x) x$results) %>%
     unlist(recursive = F) %>%
     tibble::tibble(
        load_date = map_chr(. , "load_date", .default = NA),
        last_file_date = map_chr(. , "last_file_date", .default = NA),
        candidate_id = map_chr(. , "candidate_id", .default = NA),
        election_years = map(. , "election_years", .default = NA),
        party = map_chr(. , "party", .default = NA),
        candidate_status = map_chr(. , "candidate_status", .default = NA),
        office_full = map_chr(. , "office_full", .default = NA),
        state = map_chr(. , "state", .default = NA),
        federal_funds_flag = map_lgl(. , "federal_funds_flag", .default = NA),
        has_raised_funds = map_lgl(. , "has_raised_funds", .default = NA),
        office = map_chr(. , "office", .default = NA),
        election_districts = map(. , "election_districts", .default = NA),
        name = map_chr(., "name", .default = NA),
        principal_committees = map(. , "principal_committees"),
        district_number = map_int(. , "district_number", .default = NA),
        cycles = map(. , "cycles", .default = NA),
        incumbent_challenge = map_chr(. , "incumbent_challenge", .default = NA),
        party_full = map_chr(. , "party_full", .default = NA),
        first_file_date = map_chr(. , "first_file_date", .default = NA),
        active_through = map_int(. , "active_through", .default = NA),
        incumbent_challenge_full = map_chr(. , "incumbent_challenge_full", .default = NA),
        last_f2_date = map_chr(. , "last_f2_date", .default = NA),
        district = map_chr(. , "district", .default = NA)
     ) %>%
     #for candidates with no principal committee, we still want a list filled with NA values so other functions don't freak.
     tidyr::replace_na(list(principal_committees = list(list(list(organization_type_full = NA,
                                                                  designation = NA,
                                                                  state = NA,
                                                                  cycles = NA,
                                                                  party_full = NA,
                                                                  committee_type_full = NA,
                                                                  organization_type = NA,
                                                                  committee_id = NA,
                                                                  first_file_date = NA,
                                                                  party = NA,
                                                                  committee_type = NA,
                                                                  last_file_date = NA,
                                                                  candidate_id = NA,
                                                                  designation_full = NA,
                                                                  last_f1_date = NA,
                                                                  treasurer_name = NA,
                                                                  name = NA)))))
  
  return(tidy_candidates)
  
}
