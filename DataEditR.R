# load packages
pacman::p_load(tidyverse, DataEditR)

movies <- read_csv("datasets/movies.csv", show_col_types = F) %>% janitor::clean_names()

movies_subset <- movies %>% select(1:4) %>% slice_head(prop = .3)

movies_subset1 <- data_edit(x = movies_subset)
