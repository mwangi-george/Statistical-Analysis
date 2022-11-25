install.packages("repurrrsive")
library(repurrrsive)


data("wesanderson")

wesanderson[[1]]
map(wesanderson, ~length(.x)) %>% 
  as_vector()
map_dbl(wesanderson, ~length(.x))
lapply(wesanderson, length) %>% as_vector()
data("sw_films")

str(sw_films)
sw_films %>% set_names(map(sw_films, "title"))
names(sw_films %>% set_names(map(sw_films, "title")))

map(wesanderson, ~.x %>% length() %>% dim())

names(wesanderson) <- NULL
names(wesanderson)

library(tidyverse)

mylist <- list(12, 20, 50, 100)

sim <- map(
  mylist, ~data.frame(
    a = rnorm(mean = .x, 
              n = 200, 
              sd =2.5
              )
    )
)
head(sim[[2]])

rnorm(1:3, n = 3, sd = 1.1)

library(modeldata)
library(janitor)

data("car_prices")

car_prices %>% 
  clean_names() %>% 
  select(price, mileage, cylinder)-> car_prices


my_model_sum <- as.list(car_prices) %>% 
  map(~lm(
    price ~ mileage, data = .x
  )) %>% 
  map(summary)

car_prices <- as.list(car_prices)

str(car_prices)
remove.packages(snippet)

map_df(car_prices, ~tibble(price = .x[["price"]],
                            mileage = .x[["mileage"]]))


car_prices[[1]]
names(car_prices)
car_prices %>% 
  map_df(
    ~tibble(price = ~.x[[1]],
            mileage = ~.x[[2]])
  )

car_prices


# Pull out the director element of sw_films in a list and character vector
map(sw_films, ~.x[["director"]])
map_chr(sw_films, ~.x[["director"]])

# Compare outputs when checking if director is George Lucas
map(sw_films, ~.x[["director"]] == "George Lucas")
map_lgl(sw_films, ~.x[["director"]] == "George Lucas")

# Pull out episode_id element as list
map(sw_films, ~.x[["episode_id"]])

# Pull out episode_id element as integer vector
map_dbl(sw_films, ~.x[["episode_id"]])


# Simulating data with multiple inputs using map2()
# The map() function is great if you need to iterate over one list, however, 
# you will often need to iterate over two lists at the same time. 
# This is where map2() comes in. While map() takes the list as the .x argument; 
# map2() takes two lists as two arguments: .x and .y.


# List of 1, 2 and 3
means <- list(1, 2, 3)

# Create sites list
sites <- list("north", "west", "east")

# Map over two arguments: sites and means
list_of_files_map2 <- map2(sites, means, ~ data.frame(
  sites = .x,
  a = rnorm(mean = .y, n = 200, sd = (5 / 2))
))

list_of_files_map2


# To iterate over more than two lists, whether it's three, four, or even 20, 
# you'll need to use pmap(). However, pmap() does require us to supply our list arguments a bit differently.
# To use pmap(), you first need to create a master list of all the lists we want to iterate over. 
# The master list is the input for pmap(). Instead of using .x or .y, use the list names as the argument names.


# Create a master list, a list of lists
pmapinputs <- list(
  sites = sites, means = means, sigma = sigma,
  means2 = means2, sigma2 = sigma2
)

# Map over the master list
list_of_files_pmap <- pmap(
  pmapinputs,
  function(sites, means, means2, sigma, sigma2) {
    data.frame(
      sites = sites,
      a = rnorm(mean = means, n = 200, sd = sigma),
      b = rnorm(mean = means2, n = 200, sd = sigma2)
    )
  }
)

list_of_files_pmap

rm(list = ls())

data("car_prices")
car_prices <- clean_names(car_prices)

names(car_prices)

glimpse(car_prices)

car_prices %>%
  mutate(
    # change cylinder to factor
    cylinder = as.factor(cylinder)
  ) %>%
  # split the dataset into parts 
  group_split(cylinder) %>%
  # iterate over every part
  map_dbl(
    # run linear model without global intercept
    ~ lm(price ~ mileage + doors + cruise + sound + leather + 0,
      data = .
    ) %>%
      # return model performance metrics
      glance() %>%
      # pull out residual standard error of the models
      pull(sigma)
  )


data("sw_vehicles")

head(sw_vehicles)

str(sw_vehicles)
as.data.frame(sw_vehicles[[1]]) %>% 
  select(1:6) %>% 
  slice_head() %>% 
  pivot_longer(
    everything(),
    names_to = "aspects",
    values_to = "info"
  )


list(1, "I can", 10, 0, "purrr") %>%
  map(safely(log, otherwise = NA_real_)) %>%
  # Transpose the result
  transpose()



map(sw_vehicles, "length") %>% 
  map(function(x){
    ifelse(x == "unknown", NA,
           as.numeric(x))
  })


# Map over sw_people and pull out the height element
height_ft <- map(sw_people, "height") %>% 
  map(safely(function(x){
    x * 0.0328084
    #set quiet to F so that errors are printed
  }, quiet = F)) %>% 
  transpose()

# Print your list, the result element, and the error element
height_ft
height_ft[["result"]]
height_ft[["error"]]


# Take the log of each element in the list
list(1, "I can", 10, 0, "purrr") %>% 
  map(possibly(function(x){
    log(x)
  }, otherwise  = NA_real_))

sw_people[[87]]["height"]

sw_people %>% 
  map("height") %>% 
  map(function(x){
    ifelse(x == "unknown", NA, as.numeric(x))
  })


sw_people %>%
  # retrieve all heights from the list
  map("height") %>%
  # change unknown to NA
  map(function(x) {
    ifelse(x == "unknown", NA, as.numeric(x))
  }) %>%
  # multiply each element by 2
  map(possibly(function(x) {
    x * 2
  }, otherwise = NA_real_))


# Print with walk
walk(people_by_film, print)
head(walk(sw_people, print))

library(gapminder)

gapminder %>% 
  group_split(country) %>% 
  map(~ggplot(
    data = ., aes(year, lifeExp)
  )+
    geom_point()
  )


# Load the gap_split data
data(gap_split)

# Map over the first 10 elements of gap_split
plots <- map2(gap_split[1:10], 
              names(gap_split[1:10]), 
              ~ ggplot(.x, aes(year, lifeExp)) + 
                geom_line() +
                labs(title = .y))

# Object name, then function name
walk(plots, print)



# Load the data
data(gh_users)

# Check if data has names
names(gh_users)

# Map over name element of list
map(gh_users, ~.x[["name"]])


# Name gh_users with the names of the users
gh_users_named <- gh_users %>% 
  set_names(map_chr(gh_users, "name"))

# Check gh_repos structure
str(gh_repos)

# Name gh_repos with the names of the repo owner
gh_repos_named <- gh_repos %>% 
  map_chr(~ .[[1]]$owner$login) %>% 
  set_names(gh_repos, .)


# Determine who joined github first
map_chr(gh_users, ~.x[["created_at"]]) %>%
  set_names(map_chr(gh_users, "name")) %>%
  sort()

# Determine user versus organization
map_lgl(gh_users, ~.x[["type"]] == "User") 

# Determine who has the most public repositories
map_int(gh_users, ~.x[["public_repos"]]) %>%
  set_names(map_chr(gh_users, "name")) %>%
  sort()



# Map over gh_repos to generate numeric output
map(gh_repos,
    ~map_dbl(.x, 
             ~.x[["size"]])) %>%
  # Grab the largest element
  map_dbl(~max(.x))

names(sw_people)

glimpse(sw_people)


# create  a dataframe
sw_people %>% 
  map_df(`[`, c("name", "height"))


# Create a data frame with four columns
map_df(gh_users, `[`, 
       c("login", "name", "followers","public_repos")) %>%
  # Plot followers by public_repos
  ggplot(., 
         aes(x = followers, y = public_repos)) + 
  # Create scatter plots
  geom_point()



# Turn data into correct data frame format
film_by_character <- tibble(filmtitle = map_chr(sw_films, "title")) %>%
  mutate(filmtitle, characters = map(sw_films, "characters")) %>%
  unnest(cols = c(characters))

# Pull out elements from sw_people
sw_characters <- map_df(sw_people, `[`, c("height","mass","name","url"))

# Join our two new objects
character_data <- inner_join(film_by_character, sw_characters, by = c("characters" = "url")) %>%
  # Make sure the columns are numbers
  mutate(height = as.numeric(height), mass = as.numeric(mass))

# Plot the heights, faceted by film title
ggplot(character_data, aes(x = height)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ filmtitle)
