# this function produces summary statistics and graphic visualization of the top n occupations in terms of donations for the input data.

# Initialize libraries
required_packages <- c("httr", "rvest", "jsonlite", "dplyr", "stringr", "purrr", "tidyr", "ggplot2", "scales") # list of packages required
# Check if any listed packages are not installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# Install packages_new if it's not empty
if(length(new_packages)) install.packages(new_packages)
# Load packages
lapply(required_packages, library, character.only = TRUE)

load(file = "WY_contr")

#variables of interest: is_individual, contributor_city, contributor_occupation, candidate, contribution_receipt_amount

#' @param n number of top-contribution cities desired
#' @param contributions contribtion dataset retured using previous function
#'
topn<-5

#extract party from commitee info to identify republican vs democratic candidate
contributions$party<- lapply(contributions$committee, `[[`, "party")

#compute total donations by party
totdon<-contributions %>%
   group_by(candidate) %>%
   summarize(count1 = n(), tot = sum(contribution_receipt_amount))
totdon

#compute total donation for top 5 occupations for each party
tottop<-contributions %>%
   mutate(city_state = paste0(contributor_city, ", ", contributor_state)) %>%
   group_by(candidate, contributor_occupation) %>%
   summarize(count = n(), total_donations = sum(contribution_receipt_amount)) %>%
   arrange(desc(total_donations)) %>%
   top_n(topn, total_donations) %>%
   group_by(candidate) %>%
   summarize(count2 = sum(count), tottop = sum(total_donations))
tottop

# generate residual donations category (not top 5 occupation) to create percentage stacked bar
others<-totdon %>%
   left_join(tottop) %>%
   mutate(count = count1 - count2, total = tot - tottop) %>%
   mutate(contributor_occupation = "OTHERS") %>%
   select("candidate", "contributor_occupation", "count", "total")
others


# candlist<-unique(contributions$candidate)
# for (c in candlist) {
#    summstat<-contributions %>%
#       group_by(candidate, contributor_occupation) %>%
#       summarize(party = first(party) , count = n(), total = sum(contribution_receipt_amount, na.rm = T), average = mean(contribution_receipt_amount, na.rm = T), sd = sd(contribution_receipt_amount, na.rm = T), min = min(contribution_receipt_amount, na.rm = T), max = max(contribution_receipt_amount, na.rm = T)) %>%
#       arrange(desc(total)) %>%
#       top_n(topn, total) %>%
#       # select(candidate==c) %>%
#       arrange(candidate, desc(total))
#    names(summstat)<-paste0("topocc_",c)
# }
# topocc_BARASSO
# topocc_TRAUNER





#prepare summary stats
topocc_data<-contributions %>%
   group_by(candidate, contributor_occupation) %>%
   summarize(party = first(party) , count = n(), total = sum(contribution_receipt_amount, na.rm = T), average = mean(contribution_receipt_amount, na.rm = T), sd = sd(contribution_receipt_amount, na.rm = T), min = min(contribution_receipt_amount, na.rm = T), max = max(contribution_receipt_amount, na.rm = T)) %>%
   arrange(desc(total)) %>%
   top_n(topn, total) %>%
   arrange(candidate, desc(total))
topocc_data$contributor_occupation[is.na(topocc_data$contributor_occupation)]<-"NOT AVAILABLE"

topocc_data

## GRAPH
#graph theme settings
graph_theme = theme_bw(base_size = 12) +
   theme(panel.grid.major = element_line(size = .1, color = "grey"), # Increase size of gridlines
         axis.line = element_line(size = .7, color = "black"), # Increase size of axis lines
         text = element_text(size = 12), legend.text=element_text(size=rel(0.5))) # Increase the font size


blues<-RColorBrewer::brewer.pal(topn+1, "Blues")
# blues[levels(topocc_data$contributor_occupation)=="OTHERS"]<-"gray"
reds<-RColorBrewer::brewer.pal(topn+1, "Reds")
# oranges[levels(topocc_data$contributor_occupation)=="OTHERS"]<-"gray"

if (topocc_data$party[1]=="DEM") {
   cols<-c(blues, reds)
} else {
   cols<-c(reds, blues)
}

#add others category to data
topocc_data<-topocc_data %>%
   bind_rows(others)

# occ_levels<-names(sort(tapply(topocc_data$total, topocc_data$contributor_occupation, sum)))
# topocc_data$candidate<-as.factor(topocc_data$candidate)

topocc_data$contributor_occupation<-factor(topocc_data$contributor_occupation, levels = unique(topocc_data$contributor_occupation)[order(topocc_data$total, decreasing = T)])

#prepare data for barplot
topocc_data %>%
   arrange(candidate, desc(total)) %>%
   ggplot(aes(x = candidate, y = total, fill = interaction(contributor_occupation, candidate))) +
   geom_bar(stat= "identity", position = "fill") +
   scale_y_continuous(labels = percent_format()) +
   # scale_fill_hue(h = c(45, 365)) +
   scale_fill_manual(values = cols, name = "Top Occupations")+
   ylab("Donation Shares") +
   labs(caption = "The OTHERS category groups all the residual occupations not in the top-n positions")
   graph_theme


