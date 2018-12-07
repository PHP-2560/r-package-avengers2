input_api <- "jFTYk34OsWkFoEHLcUDa7G1Ax4GCyhJyAgCwB8oz"




source("./fecScrape/R/query_openfec.R")

get_contributions<- function(input_candlist) {
   clist<-input_candlist
   source("./fecScrape/R/get_itemized_contributions.R")

}

get_contributions(candidates)
