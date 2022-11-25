install.packages("repurrrsive")
library(repurrrsive)
wesanderson

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






