

install.packages("rtweet")
library(rtweet)
library(naniar)

# authenticate
auth_setup_default()

# get full timeline
my_twitter <- get_timeline("mwangi__george", n = Inf)

# get follower id's
followers <- get_followers("mwangi__george")

# get follower names
follower_names <- lookup_users(followers$from_id)

my_followers <- followers %>%
  # join follower_names table
  inner_join(
    follower_names, 
    by = c("from_id" = "id_str")
    ) %>%
  # select variables of interest
  select(id, name, screen_name, location)

my_followers %>% 
  filter(str_detect(location, "igeria")) %>% 
  count()

my_followers %>% 
  replace_with_na(
    replace = list(
      location = ""
    )) %>% 
  drop_na() %>% 
  count(location) %>% 
  arrange(-n) %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(location, n))+
  geom_col(fill = "blue", alpha = .5)+
  coord_flip()+
  theme_clean()
