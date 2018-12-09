# Purpose: take the scrape output and make a lineplot to show donations over time

# Initialize libraries 
required_packages = c("dplyr", 
                      "tidyverse", 
                      "ggplot2") # list of packages required

# Check if any listed packages are not installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# Install packages_new if it's not empty
if(length(new_packages)) install.packages(new_packages) 
# Load packages
lapply(required_packages, library, character.only = TRUE)

###########################
# Manually pulling in data to test 
# input_state <- "TX"
# input_candidates <- c("O'Rourke, Beto", "Cruz, Ted")
input_state <- "WY"
input_candidates <- c("Barrasso, John", "Trauner, Gary")
df <- read.csv(paste0("../week-09-inclass-avengers/data/", input_state,"_ind_cont.csv")) # use stored data to save some time
###########################

plot_donations = function(df) {
  # Initialize graph attributes 
  graph_theme = theme_bw(base_size = 12) +
    theme(panel.grid.major = element_line(size = .1, color = "grey"), # Increase size of gridlines 
          axis.line = element_line(size = .7, color = "black"), # Increase size of axis lines 
          axis.text.x = element_text(angle = 90, hjust = 1), #Rorate text
          text = element_text(size = 12)) # Increase the font size
  group_colors = c("#377EB8", "#E41A1C") # blue, red

  # Get dates
  dates = df %>% select(date) %>%  mutate(date = as.Date(date)) %>% 
    summarise(min = min(date), max = max(date))
  title <- paste("From", dates$min, "To", dates$max)  

  # Donation data cleanup: clean
  data_clean = df %>% 
    select(amount, date, party) %>%
    mutate(date = as.Date(date))
  
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
    scale_x_date(date_breaks = "days" , date_labels = "%d-%b") +
    xlab(label = "Date") +
    scale_y_continuous(name = "Average Donation") +    
    ggtitle(paste("Senate Donations", title, "\nDaily")) + 
    scale_color_manual(values = group_colors) +
    graph_theme 
  
  plot_cumulative = ggplot(data_cumulative,
                             aes(x = date, y = cum_donation, color = party, group = party)) +
    geom_line(alpha = .3) +
    geom_smooth() +
    scale_x_date(date_breaks = "days" , date_labels = "%d-%b") +    
    xlab(label = "Date") +
    scale_y_continuous(name = "Cumulative Donation") +
    ggtitle(paste("Senate Donations", title, "\nCumulative")) + 
    scale_color_manual(values = group_colors) +
    graph_theme
  
  plots = list("average" = plot_average_daily, "cumulative" = plot_cumulative)
  return(plots)
}