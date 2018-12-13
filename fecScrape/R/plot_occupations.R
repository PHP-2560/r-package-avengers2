#' Plot occputation level data for donations
#'
#' @param n number of occpuations (max 8)
#' @param df A dataframe from query_contributions_all which specifies individual donations to candidates
#'
#' @import ggplot2 dplyr lubridate scales
#' @export 

plot_occupations <- function(n, df) {
  
  if (n >= 8 ) n = 8
  
  #extract party from commitee info to identify republican vs democratic candidate
  df$party<- lapply(df$committee, `[[`, "party")
  
  #compute total donations by party
  totdon<-df %>%
    group_by(candidate) %>%
    summarize(count1 = n(), tot = sum(contribution_receipt_amount))
  #totdon
  
  #compute total donation for top 5 occupations for each party
  tottop<-df %>%
    mutate(city_state = paste0(contributor_city, ", ", contributor_state)) %>%
    group_by(candidate, contributor_occupation) %>%
    summarize(count = n(), total_donations = sum(contribution_receipt_amount)) %>%
    arrange(desc(total_donations)) %>%
    top_n(n, total_donations) %>%
    group_by(candidate) %>%
    summarize(count2 = sum(count), tottop = sum(total_donations))
  #tottop
  
  # generate residual donations category (not top 5 occupation) to create percentage stacked bar
  others<-totdon %>%
    left_join(tottop) %>%
    mutate(count = count1 - count2, total = tot - tottop) %>%
    mutate(contributor_occupation = "OTHERS") %>%
    select("candidate", "contributor_occupation", "count", "total")
  #others
  
  #prepare summary stats
  topocc_data<-df %>%
    group_by(candidate, contributor_occupation) %>%
    summarize(party = first(party) , count = n(), total = sum(contribution_receipt_amount, na.rm = T), average = mean(contribution_receipt_amount, na.rm = T), sd = sd(contribution_receipt_amount, na.rm = T), min = min(contribution_receipt_amount, na.rm = T), max = max(contribution_receipt_amount, na.rm = T)) %>%
    arrange(desc(total)) %>%
    top_n(n, total) %>%
    arrange(candidate, desc(total))
  topocc_data$contributor_occupation[is.na(topocc_data$contributor_occupation)]<-"NOT AVAILABLE"
  
  topocc_data
  
  ## GRAPH
  #graph theme settings
  graph_theme = theme_bw(base_size = 12) +
    theme(panel.grid.major = element_line(size = .1, color = "grey"), # Increase size of gridlines
          axis.line = element_line(size = .7, color = "black"), # Increase size of axis lines
          text = element_text(size = 12), legend.text=element_text(size=rel(0.5))) # Increase the font size
  
  
  blues<-RColorBrewer::brewer.pal(n+1, "Blues")
  reds<-RColorBrewer::brewer.pal(n+1, "Reds")

  if (topocc_data$party[1]=="DEM") {
    cols<-c(blues, reds)
  } else {
    cols<-c(reds, blues)
  }
  
  #add others category to data
  topocc_data<-topocc_data %>%
    bind_rows(others)
  
  topocc_data$contributor_occupation<-factor(topocc_data$contributor_occupation, levels = unique(topocc_data$contributor_occupation)[order(topocc_data$total, decreasing = F)])
  
  #prepare data for barplot
  plot_final <- topocc_data %>%
    arrange(candidate, desc(total)) %>%
    ggplot(aes(x = candidate, y = total, fill = interaction(contributor_occupation, candidate))) +
    geom_bar(stat= "identity", position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    # scale_fill_hue(h = c(45, 365)) +
    scale_fill_manual(values = cols, name = "Top Occupations")+
    ylab("Donation Shares") +
    labs(caption = "The OTHERS category groups all the residual occupations not in the top-n positions") +
  graph_theme
  
  return(plot_final)
}