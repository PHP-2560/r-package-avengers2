#' @param clist the list of candidates

choose_cand <-function(clist) {

   selection <- readline("type in the numbers associated with the candidates selected separated by a comma: ")
   selection <-unlist(strsplit(selection, ","))
   new_candlist<-clist[selection,]


   return(new_candlist)
}


