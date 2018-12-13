#' Plot the cummulative donation for candidates over the time window
#'
#' @param df A dataframe from query_contributions_all which specifies individual donations to candidates
#'
#' @import ggplot2 dplyr lubridate
#' @export 

plot_cum_donation <- function(df) {
  
  # Initialize graph attributes 
  graph_theme = theme_bw(base_size = 12) +
    theme(panel.grid.major = element_line(size = .1, color = "grey"), # Increase size of gridlines 
          axis.line = element_line(size = .7, color = "black"), # Increase size of axis lines 
          axis.text.x = element_text(angle = 90, hjust = 1), #Rotate text
          text = element_text(size = 12)) # Increase the font size
  group_colors = c("#377EB8", "#E41A1C") # blue, red
  
  # Get dates
  dates <- df %>% select(contribution_receipt_date) %>%  mutate(date = as.Date(contribution_receipt_date)) %>%
    summarise(min = min(date), max = max(date))
  title <- paste("From", dates$min, "To", dates$max)
  
  # Extract party from the committee list
  df$party <- as.character(lapply(df$committee, `[[`, "party"))
  
  # Donation data cleanup: clean
  data_clean = df %>% 
    mutate(amount = contribution_receipt_amount, date = as.Date(contribution_receipt_date)) %>%
    select(amount, date, party) %>%
    mutate(date = as.Date(date))
  
  # Donation data cleanup: cumulative
  data_cumulative = data_clean %>%
    group_by(party) %>%
    arrange(date) %>%
    mutate(cum_donation = cumsum(amount))
  
  plot_cum <- ggplot(data_cumulative,
                               aes(x = date, y = cum_donation, group = party, color = party)) + 
    geom_line(alpha = .3) + 
    geom_smooth(method = "loess") + # loess
    xlab(label = "Date") +
    scale_y_continuous(name = "Cummulative Donation per Party Candidate") +    
    ggtitle(paste("Senate Donations", title, "\nDaily")) + 
    scale_color_manual(values = group_colors) +
    graph_theme 
  
  return(plot_cum)
}
