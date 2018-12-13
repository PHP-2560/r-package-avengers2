#' Plot zipcode level data for cities with the most donations
#'
#' @param n number of top-contribution cities desired
#' @param df A dataframe from query_contributions_all which specifies individual donations to candidates
#'
#' @import ggplot2 dplyr lubridate
#' @export 

plot_top_cities <- function(n, df) {
  
#extract party from commitee info to identify republican vs democratic candidate
df$party<- lapply(df$committee, `[[`, "party")


#aggregate by city
citydf <- df %>%
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
