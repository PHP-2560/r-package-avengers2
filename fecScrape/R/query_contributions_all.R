#' Find all individual donations to candidate
#' 
#' This function constructs searches for donations listed in the FEC based on a candidate input
#' @param input_candlist Requires a df with candidate name and committee ID. Use query_candidate.R to get this information.
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#'
#' @import purrr dplyr magrittr 
#'
#' @export


query_contributions_all <- function(input_candlist, api_key) {
   contributions<-list()
   for (c in 1:dim(input_candlist)[1]) {
      cname<-input_candlist$name[c] %>%
         strsplit(split = ",") %>%
         simplify() %>%
         first()
      temp <- query_itemized_contributions(data = input_candlist[c,], api_key = api_key)
      contributions[[c]]<-temp
      contributions[[c]]$candidate<-cname
      names(contributions)[c]<-paste0("contributions_",cname)

   }
   if (dim(input_candlist)[1]==1) {
      return(contributions)
   } else {
      # contributions[[(dim(input_candlist)[1] + 1)]]<-append(contributions[[1:dim(candidates)[1]]]) # to finish
      contributions_all<-do.call(rbind, contributions)
      return(contributions_all)
   }
}
