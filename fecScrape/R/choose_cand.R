#' Selects candidates from a list
#' 
#' @param df the list of candidates
#' @param firstCandidate The number of the first candidate in the list, if known (otherwise will prompt)
#' @param secondCandidate The number of the second candidate in the list, if known 
#' 
#' @export

choose_cand <-function(df, firstCandidate = NULL, secondCandidate = NULL) {
  
  # Can either choose by looking at list or input known numbers
  if (is.null(firstCandidate) & is.null(secondCandidate)) {
    # Select candidates
    df %>%
      select(name) %>%
      print()
    
    selection <- readline("Type in the numbers associated with TWO candidates of different parties selected separated by a comma: ")
  } else {
    selection <- paste0(firstCandidate, ",", secondCandidate)
  }
  
   selection <- unlist(strsplit(selection, ","))
   new_candlist <- df[selection,]

   return(new_candlist)
}


