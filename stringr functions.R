pacman::p_load(tidyverse)

machakos <- read_csv("datasets/machakos.csv")

# str_split()
machakos %>% 
  mutate(text1 = str_split(text, pattern = fixed(" "), simplify = T))


# str_replace()
machakos %>% 
  mutate(text = str_replace(text, pattern = "#", replacement = "")) %>% 
  view()

# str_replace_all()
machakos %>% 
  mutate(text = str_replace_all(text, pattern = "#", replacement = "")) %>% 
  view()


# Some (fake) phone numbers
phone_numbers <- c("510-555-0123", "541-555-0167")

# Use str_replace() to replace "-" with " "
str_replace(phone_numbers, "-", " ")

# Use str_replace_all() to replace "-" with " "
str_replace_all(phone_numbers, fixed("-"), " ")

# Turn phone numbers into the format xxx.xxx.xxxx
str_replace_all(phone_numbers, fixed("-"), ".")

# Find the number of nucleotides in each sequence
str_length(genes)

# Find the number of A's occur in each sequence
str_count(genes, fixed("A"))

# Return the sequences that contain "TTTTTT"
str_subset(genes, fixed("TTTTTT"))

# Replace all the "A"s in the sequences with a "_"
str_replace_all(genes, fixed("A"), "_")




# Define some full names
names <- c("Diana Prince", "Clark Kent")

# Split into first and last names
names_split <- str_split(names, fixed(" "), simplify = T, n = 2)

# Extract the first letter in the first name
abb_first <- str_sub(names_split[,1], 1, 1)

# Combine the first letter ". " and last name
str_c(abb_first, ". ", names_split[,2])


# Use all names in babynames_2014
all_names <- babynames_2014$name

# Get the last two letters of all_names
last_two_letters <- str_sub(all_names, -2, -1)

# Does the name end in "ee"?
ends_in_ee <- str_detect(last_two_letters, "ee")

# Extract rows and "sex" column
sex <- babynames_2014$sex[ends_in_ee]

# Display result as a table
table(sex)








