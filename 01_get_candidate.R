#' @param input_api
#' @param input_state
#' @param input_year
#' @param input_office
#'
#' @export

# Initialize libraries
required_packages <- c("httr", "rvest", "jsonlite", "dplyr", "stringr", "purrr", "tidyr") # list of packages required
# Check if any listed packages are not installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# Install packages_new if it's not empty
if(length(new_packages)) install.packages(new_packages)
# Load packages
lapply(required_packages, library, character.only = TRUE)

source("./fecScrape/R/query_openfec.R")
source("01a_get_candlist.R")
source("01b_choose_cand.R")


get_candidates<-function(input_api, input_state, input_year, input_office) {
   candlist<-get_fec(state = input_state,
                     election_year = input_year,
                     api_key = input_api,
                     office = input_office)
   return(candlist)
}
my_candidates<-get_candidates(input_api = "jFTYk34OsWkFoEHLcUDa7G1Ax4GCyhJyAgCwB8oz",
                              input_state = "WY",
                              input_year = 2018,
                              input_office = "S")





   showtable<-candlist%>%
      select(c(name, party, committee_name))
   print(showtable)
   candidates<-choose_cand(candlist)
   return(candidates)
}

my_candidates<-get_candidates(input_api = "jFTYk34OsWkFoEHLcUDa7G1Ax4GCyhJyAgCwB8oz",
                        input_state = "WY",
                        input_year = 2018,
                        input_office = "S")
