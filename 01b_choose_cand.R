#' @param clist the list of candidates 

choose_cand <-function(clist) {
   
   selection <- readline("type in the numbers associated with the candidates selected separated by a comma: ")
   selection <-unlist(strsplit(selection, ","))
   new_candlist<-candlist[selection,]
   
   
   return(new_candlist)
}

candidates<-choose_cand(candlist)
