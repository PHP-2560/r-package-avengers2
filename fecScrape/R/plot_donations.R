# Purpose: take the scrape output and make a lineplot to show donations over time
rm(list=ls())
# Initialize libraries 
required_packages = c("dplyr", 
                      "tidyverse", 
                      "scales", 
                      "ggplot2") # list of packages required

# Check if any listed packages are not installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# Install packages_new if it's not empty
if(length(new_packages)) install.packages(new_packages) 
# Load packages
lapply(required_packages, library, character.only = TRUE)

###########################
# Manually pulling in data to test 
load("./Data/WY_contr")
###########################

df = contributions %>% 
  select(id = contributor_id, 
         city = contributor_city, 
         employer = contributor_employer, 
         occupation = contributor_occupation, 
         zipcode = contributor_zip, 
         amount = contribution_receipt_amount, 
         date = contribution_receipt_date,
         fec_election_year) %>%
  mutate(party = unlist(lapply(contributions$committee, `[[`, "party")))
 
plot_donations = function(df) {
  # Initialize graph attributes 
  graph_theme = theme_bw(base_size = 12) +
    theme(panel.grid.major = element_line(size = .1, color = "grey"), # Increase size of gridlines 
          axis.line = element_line(size = .7, color = "black"), # Increase size of axis lines 
          axis.text.x = element_text(angle = 90, hjust = 1), #Rotate text
          text = element_text(size = 12)) # Increase the font size
  group_colors = c("#377EB8", "#E41A1C") # blue, red

  # Get dates
  dates = df %>% select(date) %>%  mutate(date = as.Date(date)) %>%
    summarise(min = min(date), max = max(date))
  title <- paste("From", dates$min, "To", dates$max)
  
  # Donation data cleanup: clean
  data_clean = df %>% 
    select(amount, date, party) %>%
    mutate(date = as.Date(date)) %>%
    mutate(month = as.Date(date, format = "%Y-%m"))

  # Donation data cleanup: average daily
  data_average_daily = data_clean %>% 
    group_by(party, date) %>%
    summarise(Mean = mean(amount), SD = sd(amount), N = n(), SE = SD / sqrt(N), na.rm = TRUE) %>%
    mutate(lower_CI = Mean - qt(1 - (0.05 / 2), N - 1) * SE,
           upper_CI = Mean + qt(1 - (0.05 / 2), N - 1) * SE)

  # Donation data cleanup: cumulative
  data_cumulative = data_clean %>%
    group_by(party) %>%
    arrange(date) %>%
    mutate(cum_donation = cumsum(amount))
  
  plot_average_daily = ggplot(data_average_daily,
                      aes(x = date, y = Mean, group = party, color = party)) + 
    geom_point() + 
    geom_ribbon(aes(ymin=Mean-SE, ymax=Mean+SE), linetype = 3, alpha=0.1) + 
    geom_line() + 
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +
    xlab(label = "Date") +
    scale_y_continuous(name = "Average Donation") +    
    ggtitle(paste("Senate Donations", title, "\nDaily")) + 
    scale_color_manual(values = group_colors) +
    graph_theme 
  
  plot_cumulative = ggplot(data_cumulative,
                             aes(x = date, y = cum_donation, color = party, group = party)) +
    geom_line(alpha = .3) +
    geom_smooth() +
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +
    xlab(label = "Date") +
    scale_y_continuous(name = "Cumulative Donation") +
    ggtitle(paste("Senate Donations", title, "\nCumulative")) + 
    scale_color_manual(values = group_colors) +
    graph_theme
  
  plot_individual = ggplot(data_clean, 
                           aes(x = date, y = amount, group = party, color = party)) +
    scale_color_manual(values = group_colors) +
    geom_point() +
    geom_jitter(width = .4, height = 0) +
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +
    graph_theme
  
  plots = list("average" = plot_average_daily, "cumulative" = plot_cumulative, "individual" = plot_individual)
  return(plots)
}
plot_donations(df)
