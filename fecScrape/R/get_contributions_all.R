get_contributions<- function(input_candlist) {
   contributions<-list()
   for (c in 1:dim(input_candlist)[1]) {
      cname<-input_candlist$name[c] %>%
         strsplit(split = ",") %>%
         simplify() %>%
         first()
      temp<-get_itemized_contributions(data = input_candlist[c,])
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
