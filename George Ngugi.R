# title: "Insupply Health Written Test"
# author: "George Mwangi"
# date: "2023-02-23"


# loading essential packages
pacman::p_load(tidyverse)

# Reading data
methods_fac <- read_rds(
  file = "datasets/methodsfac.rds"
  ) %>%  
  # clean variable names into a more intuitive format
  janitor::clean_names()


# Let's start With some Exploratory analysis--------------

# print first 6 rows
head(methods_fac)


# count distinct methods used and arrange in descending order
methods_fac %>% count(method, sort = TRUE)


# Data visualization------------------

# This code pivots the data from wide to long format
# Then it creates a grouped bar chart with faceting by organization (Assuming means the org.)
# The x-axis is ordered by the amount of each method used
methods_fac %>%
  pivot_longer(
    3:6, # Specify the columns to pivot
    names_to = "Organization", 
    values_to = "Amount"
  ) %>%
  ggplot(aes(fct_reorder(method, Amount), Amount)) + # Specify the data and aesthetics
  geom_col(fill = "red", alpha = .5) + # Add columns with red fill and 50% opacity
  coord_flip() + # Flip the x- and y-axes
  facet_wrap(~Organization, scales = "free_x") + # Facet the plot by organization with free x-axis scales
  labs( # Add plot labels and captions
    title = "Method by Organization",
    subtitle = "Analysis of Women of reproductive age in the community",
    x = "Contraceptive Method Used",
    y = "Units of each Method",
    caption = "Data Source: InSupplyHealth Ltd."
  ) +
  theme_bw() + # Set a black-and-white theme
  theme(plot.background = element_rect(fill = "gray90")) -> p1 # Save the plot to p1 and set the plot background to light gray

plotly::ggplotly(p1) # Convert p1 to a Plotly object and display the interactive plot







