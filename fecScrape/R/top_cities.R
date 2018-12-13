# this function produces summary statistics and graphic visualization of the top n cities in terms of donations for the input data.

# Initialize libraries
required_packages <- c("httr", "rvest", "jsonlite", "dplyr", "stringr", "purrr", "tidyr", "ggplot2") # list of packages required
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


top_cities<- function(n, contributions) {
#extract party from commitee info to identify republican vs democratic candidate
contributions$party<- lapply(contributions$committee, `[[`, "party")


#aggregate by city
citydf <- contributions %>%
   mutate(city_state = paste0(contributor_city, ", ", contributor_state)) %>%
   group_by(candidate, city_state) %>%
   summarize(party = first(party) , count = n(), total = sum(contribution_receipt_amount, na.rm = T), average = mean(contribution_receipt_amount, na.rm = T), sd = sd(contribution_receipt_amount, na.rm = T), min = min(contribution_receipt_amount, na.rm = T), max = max(contribution_receipt_amount, na.rm = T)) %>%
   arrange(desc(total)) %>%
   top_n(n, total)

citydf

## GRAPH
# set theme
graph_theme = theme_bw(base_size = 12) +
   theme(panel.grid.major = element_line(size = .1, color = "grey"), # Increase size of gridlines
axis.line = element_line(size = .7, color = "black"), # Increase size of axis lines
text = element_text(size = 14)) # Increase the font size

if (citydf$party[1]=="DEM") { #this if statement assign the right color to each candidate: red for Republicans and Blue for Democrats
   group_colors <- c("#377EB8", "#E41A1C") # set colors
} else {
   group_colors <- c("#E41A1C", "#377EB8") # set colors
}

#prepare labels
brk<-citydf$total %>%
   range() %>%
   max()
brk<-c(brk/2,brk)
brk<-  round(brk/10000,0)*10000
brk<-c(-brk,0,brk)
brk<-sort(brk)
brk
lab<-abs(brk)

#plot
plot<- citydf %>%
   mutate(total = ifelse(party=="REP", -total, total )) %>%
   arrange(desc(total), party) %>%
   ggplot(aes(x = reorder(city_state, -total), y = total, fill = candidate, group = candidate)) +
   geom_bar(stat = "identity") +
   coord_flip() +
   xlab("Top Donations Cities") +
   scale_y_continuous(breaks = brk, labels=lab, name = "Total Donations")  +
   scale_fill_manual(values = group_colors) +
   graph_theme


output<-list(citydf, plot)

return(output)
}

output<-top_cities(5, contributions)

output
