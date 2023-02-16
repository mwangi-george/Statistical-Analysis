# load packages
pacman::p_load(tidyverse)

movies <- read_csv("datasets/movies.csv", show_col_types = F) %>% janitor::clean_names()

movies_subset <- movies %>%
  select(1:4) %>%
  slice_head(prop = .3)

movies_subset1 <- data_edit(x = movies_subset)




mpg %>%
  select(-year) %>%
  group_by(class) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns = median,
      .names = "{.col}_median"
    ),
    count = n()
  ) %>%
  ungroup() %>%
  mutate(
    prop = count / sum(count),
    all_groups = "all_groups",
    class = fct_reorder(class, prop)
  )


movies %>% select(movies, where(is.numeric))

diamonds %>% 
  group_by(cut) %>% 
  summarise(
    across(
      .cols = where(is.numeric),
      .fns = mean,
      .names = "mean_{.col}"
    ),
    count = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    prop = count / sum(count),
    cut = fct_reorder(cut, prop)
  )

library(radiant)

data_edit(jobs)

head(jobs)

jobs %>% filter(!is.na(salary))

jobs <- jobs %>% select(-salary)

jobs %>%
  mutate(title = str_remove_all(title, fixed("(Remote) - $60,000/year USD"))) %>% 
  count(title, sort = T, name = "counts") %>%
  slice_head(n = 10) %>%
  ggplot(aes(fct_reorder(title, counts), counts, fill = as.factor(counts))) +
  geom_col(show.legend = F) +
  coord_flip()+
  labs(
    x = "",
    title = "Top Data Related Jobs",
    subtitle = ""
       )+ 
  theme_bw()





















