pacman::p_load(tidyverse, jsonlite, janitor,
               ggthemes)


spotify <- read_csv("datasets/top10s.csv", 
                    show_col_types = F) %>% 
  clean_names()


spotify %>%
  # rename column
  rename(id = x1) %>%
  # change character variables to factors
  mutate(across(is.character, as.factor)) %>%
  # get no of levels of all factor variables
  summarise(across(is.factor, nlevels))



spotify %>%
  # rename column
  rename(id = x1) %>%
  slice_head(n = 30) %>% 
  ggplot(fct_infreq(top_genre))+
  geom_col()

spotify %>% 
  slice_head(n = 50) %>% 
  select(
    top_genre, artist
  ) %>% 
  mutate(top_genre = as.factor(top_genre)) %>% 
  ggplot(
    aes(x = fct_rev(fct_infreq(top_genre))
        )
    )+
  geom_bar(fill = "blue", alpha = .7, width = .5)+
  coord_flip()+
  theme_few()


multiple_choice_responses %>%
  # Remove NAs
  filter(!is.na(EmployerIndustry) & !is.na(Age)) %>%
  # Get mean_age by EmployerIndustry
  group_by(EmployerIndustry) %>%
  summarize(mean_age = mean(Age)) %>%
  # Reorder EmployerIndustry by mean_age
  mutate(EmployerIndustry = fct_reorder(EmployerIndustry, mean_age)) %>%
  # Make a scatterplot of EmployerIndustry by mean_age
  ggplot(aes(x = EmployerIndustry, y = mean_age)) +
  geom_point() +
  coord_flip()



multiple_choice_responses %>%
  # Move "I did not complete any formal education past high school" and "Some college/university study without earning a bachelor's degree" to the front
  mutate(FormalEducation = fct_relevel(FormalEducation, "I did not complete any formal education past high school", "Some college/university study without earning a bachelor's degree")) %>%
  # Move "I prefer not to answer" to be the last level.
  mutate(FormalEducation = fct_relevel(FormalEducation, "I prefer not to answer", after = Inf)) %>%
  # Move "Doctoral degree" to be after the 5th level
  mutate(FormalEducation = fct_relevel(FormalEducation, "Doctoral degree", after = 5)) %>%
  # Examine the new level order
  pull(FormalEducation) %>%
  levels()


library(tidyverse)

diamonds %>% head()
glimpse(diamonds)

diamonds %>% 
  ggplot(aes(
    x = fct_rev(fct_infreq(color))
  ))+
  geom_bar(fill = "#2171B5", alpha = .9)+
  coord_flip()+
  labs(
    title = "Count of Diamond Colors",
    x = "Color"
  )+
  theme(
    plot.title = element_text(face = "bold")
  )+
  ggthemes::theme_few()-> plot1

diamonds %>% 
  group_by(color) %>% 
  summarise(mean_price = mean(price)) %>% 
  ggplot(
    aes(
      fct_reorder(color, mean_price), mean_price
    )
  )+
  geom_col(fill = "#2171B5", alpha = .9)+
  coord_flip()+
  labs(
    title = "Average Price of Diamond by  Color",
    x = "Color",
    y = "Average price"
  )+
  theme(
    plot.title = element_text(face = "bold")
  )+
  ggthemes::theme_few()-> plot2


diamonds %>% 
  ggplot(
    aes(
      carat, price
    )
  )+
  geom_jitter(
    fill = "#2171B5", alpha = .5
  )+
  ggtitle("Price Versus Carat")+
  ggthemes::theme_few()->plot3

replicate(5, 
          expr = {
            plots <- diamonds %>% 
  group_by(cut) %>% 
  summarise(mean_price = mean(price)) %>% 
  ggplot(
    aes(
      fct_reorder(cut, mean_price), mean_price
    )
  )+
  geom_col(fill = "#2171B5", alpha = .5)+
  coord_flip()+
  labs(
    title = "Diamonds Prices by Cut",
    y = "Average Price (USD)",
    x = ""
  )+
  ggthemes::theme_few()
          })

library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4)

diamonds %>% 
  drop_na() %>% 
  write_csv("datasets/diamonds.csv")


multiple_choice_responses %>%
  # Rename the appropriate levels to "High school" and "Some college"
  mutate(FormalEducation = fct_recode(FormalEducation,
    "High school" = "I did not complete any formal education past high school",
    "Some college" = "Some college/university study without earning a bachelor's degree"
  )) %>%
  # Make a bar plot of FormalEducation
  ggplot(aes(x = FormalEducation)) +
  geom_bar()


multiple_choice_responses %>%
  # Create new variable, grouped_titles, by collapsing levels in CurrentJobTitleSelect
  mutate(grouped_titles = fct_collapse(CurrentJobTitleSelect,
    "Computer Scientist" = c("Programmer", "Software Developer/Software Engineer"),
    "Researcher" = "Scientist/Researcher",
    "Data Analyst/Scientist/Engineer" = c(
      "DBA/Database Engineer", "Data Scientist",
      "Business Analyst", "Data Analyst",
      "Data Miner", "Predictive Modeler"
    )
  )) %>%
  # Keep all the new titles and turn every other title into "Other"
  mutate(grouped_titles = fct_other(grouped_titles,
    keep = c(
      "Computer Scientist",
      "Researcher",
      "Data Analyst/Scientist/Engineer"
    )
  )) %>%
  # Get a count of the grouped titles
  count(grouped_titles)



multiple_choice_responses %>%
  # Remove NAs of MLMethodNextYearSelect
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # Create ml_method, which lumps all those with less than 5% of people into "Other"
  mutate(ml_method = fct_lump_prop(
    MLMethodNextYearSelect, prop = .05
    )) %>%
  # Count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = T)



multiple_choice_responses %>%
  # Remove NAs 
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # Create ml_method, retaining the 5 most common methods and renaming others "other method" 
  mutate(ml_method = fct_lump_n(
    MLMethodNextYearSelect, 
    n = 5, 
    other_level = "other method"
    )) %>%
  # Count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = TRUE)



multiple_choice_responses %>%
  # Select columns with LearningPlatformUsefulness in title
  select(
    contains("LearningPlatformUsefulness")
    ) %>%
  # Change data from wide to long
  pivot_longer(
    everything(), 
    names_to = "learning_platform", 
    values_to = "usefulness"
    ) %>%
  # Remove rows where usefulness is NA
  filter(
    !is.na(usefulness)
    ) %>%
  # Remove "LearningPlatformUsefulness" from each string in learning_platform 
  mutate(
    learning_platform = str_remove(
    learning_platform, "LearningPlatformUsefulness")
    )



perc_useful_platform <- learning_platform_usefulness %>%
  # Change dataset to one row per learning_platform usefulness pair with number of entries for each
  count(learning_platform, usefulness) %>%
  # Use add_count to create column with total number of answers for that learning_platform
  add_count(learning_platform, wt = n, name = "nn") %>%
  # Create a new column, perc, that is the percentage of people giving that response for that learning_platform
  mutate(perc = n / nn)

# Create a line graph for each question with usefulness on x-axis and percentage of responses on y
ggplot(perc_useful_platform, aes(x = usefulness, y = perc, group = learning_platform)) +
  geom_line() +
  facet_wrap(~learning_platform)



usefulness_by_platform %>%
  # Reorder learning_platform by avg_usefulness
  mutate(
    learning_platform = fct_reorder(
      learning_platform, avg_usefulness
      )
    ) %>%
  # Reverse the order of learning_platform
  mutate(
    learning_platform = fct_rev(learning_platform)
    ) %>%
  ggplot(
    aes(
      x = learning_platform, 
      y = avg_usefulness
      )
    ) + 
  geom_point() + 
  theme(
    axis.text.x = element_text(
      angle = 90, 
      hjust = 1
      )
    ) + 
  labs(
    x = "Learning Platform", 
    y = "Percent finding at least somewhat useful"
    ) + 
  scale_y_continuous(labels = scales::percent_format())



mtcars %>%
  filter(between(mpg, 10, 20))

multiple_choice_responses %>%
  # Filter for rows where Age is between 10 and 90
  filter(between(Age, 10, 90)) %>%
  # Create the generation variable based on age
  mutate(generation = case_when(
    between(Age, 10, 22) ~ "Gen Z", 
    between(Age, 23, 37) ~ "Gen Y", 
    between(Age, 38, 52) ~ "Gen X", 
    between(Age, 53, 71) ~ "Baby Boomer", 
    between(Age, 72, 90) ~ "Silent"
  )) %>%
  # Get a count of how many answers in each generation
  count(generation)


multiple_choice_responses %>%
  # Filter out people who selected Data Scientist as their Job Title
  filter(CurrentJobTitleSelect != "Data Scientist") %>%
  # Create a new variable, job_identity
  mutate(job_identity = case_when(
    CurrentJobTitleSelect == "Data Analyst" &
      DataScienceIdentitySelect == "Yes" ~ "DS analysts",
    CurrentJobTitleSelect == "Data Analyst" &
      DataScienceIdentitySelect %in% c("No", "Sort of (Explain more)") ~ "NDS analyst",
    CurrentJobTitleSelect != "Data Analyst" &
      DataScienceIdentitySelect == "Yes" ~ "DS non-analysts",
    TRUE ~ "NDS non analysts"
  )) %>%
  # Get the average job satisfaction by job_identity, removing NAs
  group_by(job_identity) %>%
  summarise(avg_js = mean(JobSatisfaction, na.rm = T))


iris %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  map(~class(.))

me <- "I love R over python"
str_detect(me, "")

str_remove_all(me, ".v")


library(magrittr)

iris %$%
  cor(Sepal.Length, Sepal.Width)





# Reorder country factor levels
ilo_data <- ilo_data %>%
  # Arrange data frame
  arrange(year) %>%
  # Reorder countries by working hours in 2006
  mutate(country = fct_reorder(
    country,
    working_hours,
    last
  ))

# Plot again
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  ) +
  geom_text(
    aes(
      x = working_hours,
      y = country,
      label = round(working_hours, 1)
    )
  )

library(gapminder)

gapminder %>% 
  filter(year %in% c(2002, 2007) & continent == "Africa", gdpPercap >5000) %>% 
  select(country, year, lifeExp, gdpPercap) %>% 
  arrange(year) %>% 
  mutate(
    year = as.factor(year),
    country = fct_reorder(country, gdpPercap, mean)
  ) %>% 
  ggplot()+
  geom_path(aes(
    x = gdpPercap, y = country
  ),arrow = arrow(length = unit(1.2, "mm"), type = "closed")
  )+
  geom_text(
    aes(
      x = gdpPercap, y = country, label = round(gdpPercap),
      hjust = ifelse(year == "2006", 1.4, -0.4)
    ),
    # Change the appearance of the text
    size = 3,
    family = "Times",
    color = "gray25"
  )
  
  

# Save plot into an object for reuse
ilo_dot_plot <- ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  # Specify the hjust aesthetic with a conditional value
  geom_text(
    aes(x = working_hours,
        y = country,
        label = round(working_hours, 1),
        hjust = ifelse(year == "2006", 1.4, -0.4)
    ),
    # Change the appearance of the text
    size = 3,
    family = "Bookman",
    color = "gray25"
  )

  
  
  
# Compute temporary data set for optimal label placement
median_working_hours <- ilo_data %>%
  group_by(country) %>%
  summarize(median_working_hours_per_country = median(working_hours)) %>%
  ungroup()

# Have a look at the structure of this data set
str(median_working_hours)

ilo_dot_plot +
  # Add label for country
  geom_text(data = median_working_hours,
            aes(y = country,
                x = median_working_hours_per_country,
                label = country),
            vjust = 2,
            family = "Bookman",
            color = "gray25") +
  # Remove axes and grids
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    # Also, let's reduce the font size of the subtitle
    plot.subtitle = element_text(size = 9)
  )
  
  
  




