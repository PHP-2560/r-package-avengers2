# Purpose: take the scrape output and make a lineplot to show donations over time

# Manually pulling in data to test 
# input_state <- "TX"
# input_candidates <- c("O'Rourke, Beto", "Cruz, Ted")
input_state <- "WY"
input_candidates <- c("Barrasso, John", "Trauner, Gary")

df <- read.csv(paste0("../week-09-inclass-avengers/data/", input_state,"_ind_cont.csv")) # use stored data to save some time

# Initialize libraries 
required_packages <- c(
                        "scales", 
                        "dplyr", 
                        "tidyverse", 
                        "ggplot2", 
                        "lme4", 
                        "expss", 
                        "gridExtra", 
                        "stargazer", 
                        "OneR"
) # list of packages required

# Check if any listed packages are not installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# Install packages_new if it's not empty
if(length(new_packages)) install.packages(new_packages) 
# Load packages
lapply(required_packages, library, character.only = TRUE)

state=input_state






avg_donation_plot <- function(df) {
  
  # Initialize graph attributes 
  graph_theme = theme_bw(base_size = 12) +
    theme(panel.grid.major = element_line(size = .1, color = "grey"), # Increase size of gridlines 
          axis.line = element_line(size = .7, color = "black"), # Increase size of axis lines 
          text = element_text(size = 14)) # Increase the font size
  group_colors = c("#377EB8", "#E41A1C") # blue, red
  
  # Donation data cleanup 
  donation_data <- df %>% 
    select(amount, date, party, party) %>%
    mutate(date = as.Date(date)) %>%
    group_by(party, date) %>%
    summarise(Mean = mean(amount), SD = sd(amount), N = n(), SE = SD / sqrt(N)) %>%
    mutate(lower_CI = Mean - qt(1 - (0.05 / 2), N - 1) * SE,
           upper_CI = Mean + qt(1 - (0.05 / 2), N - 1) * SE)
  
  donation_plot <- ggplot(donation_data, aes(x = date, y = Mean, group = party, color = party)) + 
    geom_point() + 
    geom_ribbon(aes(ymin=Mean-SE, ymax=Mean+SE), linetype = 3, alpha=0.1) + 
    geom_line() + 
    xlab(label = "Date") +
    ylab("Average Donation") + 
    ggtitle("Senate Donations in October 2018") + 
    scale_color_manual(values = group_colors) +
    graph_theme 
  donation_plot
  
  return(donation_plot)
}

# avg_donation_plot(df)
# Save file
# ggsave(filename = paste0("./output/", state,"_avg_donation.pdf"),
#        plot = "donation_plot", width = 6, height = 4, useDingbats = F)

cum_donation_plot <- function(df) {
  
  # Initialize graph attributes 
  graph_theme = theme_bw(base_size = 12) +
    theme(panel.grid.major = element_line(size = .1, color = "grey"), # Increase size of gridlines 
          axis.line = element_line(size = .7, color = "black"), # Increase size of axis lines 
          text = element_text(size = 14)) # Increase the font size
  
  group_colors = c("#377EB8", "#E41A1C") # blue, red
  
  # Donation data cleanup 
  cum_donation_data <- df %>% 
    select(amount, date, party, party) %>%
    mutate(date = as.Date(date)) %>%
    group_by(party) %>%
    arrange(date) %>%
    mutate(cum_donation = cumsum(amount))
  
  cum_donation_plot <- ggplot(cum_donation_data, aes(x = date, y = cum_donation, color = party, group = party)) + 
    geom_line(alpha = .3) +
    geom_smooth() +
    xlab(label = "Date") +
    scale_y_continuous(label=dollar_format(), name = "Cumulative Donation") +
    ggtitle("Senate Donations in October 2018") +
    graph_theme + 
    scale_color_manual(values = group_colors)
  cum_donation_plot
  
  return(cum_donation_plot)
  
}

# cum_donation_plot(df)

# Save file 
# ggsave(filename = paste0("./output/", state,"_cum_donation.pdf"),
#        plot = "cum_donation_plot", width = 6, height = 4, useDingbats = F)

