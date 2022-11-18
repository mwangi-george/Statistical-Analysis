

install.packages("rtweet")
library(rtweet)

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
