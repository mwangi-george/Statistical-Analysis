
library(tidyverse)

pharmacy_data <- readxl::read_excel(
  path = "datasets/pharmacy_data.xlsx"
  ) %>% 
  janitor::clean_names()

pharmacy_data %>% view()



DataExplorer::create_report(
  data = pharmacy_data,
  output_format = "html_document",
  output_file = "pharmacy_data_EDA_report.html"
)



pharmacy_data %>% count(archetype, sort = T)

pharmacy_data %>% count(cluster, sort = T)

pharmacy_data %>% count(product_variable, sort = T) %>% view()

pharmacy_data %>% count(issue_unit, sort = T)

pharmacy_data %>% count(pharmacy_name, sort = T)

pharmacy_data %>% count(archetype_reclassification, sort = T)

head(pharmacy_data)




pharmacy_data %>% 
  count(archetype, product_variable, sort = T) %>% view()

pharmacy_data %>% 
  mutate(sales = as.numeric(sales)) %>% 
  group_by(archetype) %>% 
  summarise(mean_sales = mean(as.numeric(sales), na.rm = T)) %>% 
  ggplot(
    aes(
      x= fct_reorder(archetype, mean_sales), 
      mean_sales
      )
    )+
  geom_col(fill = "#2171B5", alpha = .9)+
  coord_flip()+
  labs(
    title = "Average Sales by Region",
    y = "Average Sales",
    x = "Region"
  )


pharmacy_data %>% 
  mutate(sold_units = as.numeric(sold_units)) %>% 
  group_by(archetype) %>% 
  summarise(mean_sold_units = mean(as.numeric(sold_units), na.rm = T)) %>% 
  ggplot(
    aes(
      x= fct_reorder(archetype, mean_sold_units), 
      mean_sold_units
    )
  )+
  geom_col(fill = "#2171B5", alpha = .9)+
  coord_flip()+
  labs(
    title = "Average sold units by Region",
    y = "Average sold units",
    x = "Region"
  )



pharmacy_data %>% 
  group_by(product_variable) %>% 
  mutate(sales = as.numeric(sales)) %>% 
  summarise(mean_sales = mean(sales, na.rm = T)) %>% 
  mutate(product_variable = fct_reorder(product_variable, mean_sales)) %>% 
  slice_max(mean_sales,n = 10) %>% 
  ggplot(
    aes(
      x = product_variable, y =mean_sales
    )
  )+
  geom_col(fill = "#2171B5", alpha = .9)+
  coord_flip()+
  labs(
    title = "Average Sales by Product Variable",
    subtitle = "Analysis of the top 10 products"
  )+
  theme(
    plot.background = element_rect(fill = "gray90")
  )+ theme_bw()


pharmacy_data %>% 
  group_by(pharmacy_name) %>% 
  mutate(sales = as.numeric(sales)) %>% 
  summarise(mean_sales = mean(sales, na.rm = T)) %>% 
  mutate(
    pharmacy_name = as.factor(pharmacy_name),
    pharmacy_name = fct_reorder(pharmacy_name, mean_sales)) %>% 
  slice_max(mean_sales,n = 10) %>% 
  ggplot(
    aes(
      x = pharmacy_name, y =mean_sales
    )
  )+
  geom_col(fill = "#2171B5", alpha = .9)+
  coord_flip()+
  labs(
    title = "Average Sales by Pharmacy Names",
    subtitle = "Analysis of the top 10 Pharmacies"
  )+
  theme(
    plot.background = element_rect(fill = "gray90")
  )+ theme_bw()


pharmacy_data %>% 
  group_by(pharmacy_name) %>% 
  mutate(sold_units = as.numeric(sold_units)) %>% 
  summarise(mean_sold_units = mean(sold_units, na.rm = T)) %>% 
  mutate(
    pharmacy_name = as.factor(pharmacy_name),
    pharmacy_name = fct_reorder(pharmacy_name, mean_sold_units)) %>% 
  slice_max(mean_sold_units,n = 10) %>% 
  ggplot(
    aes(
      x = pharmacy_name, y =mean_sold_units
    )
  )+
  geom_col(fill = "#2171B5", alpha = .9)+
  coord_flip()+
  labs(
    title = "Average sold units by Pharmacy Names",
    subtitle = "Analysis of the top 10 Pharmacies"
  )+
  theme(
    plot.background = element_rect(fill = "gray90")
  )+ theme_bw()

class(pharmacy_data$date)

pharmacy_data$sales <- as.numeric(pharmacy_data$sales)

pharmacy_data %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>% 
  summarise(mean_sales = mean(sales, na.rm = T)) #%>% 
  ggplot(aes(month, mean_sales,group = 1))+
  geom_line()+
  facet_wrap(~ year = ludrid)
  
pharmacy_data %>% 
  filter(!is.na(sales)) %>% 
  ggplot(aes(date, sales))+
  geom_line()




timetk::plot_time_series(pharmacy_data, .date_var = date, .value = sales, .title = 
                           "Sales Over Time")


pharmacy_data %>% 
  mutate(month = lubridate::month(date, label = T)) %>% 
  group_by(month) %>% 
  summarise(total_sales = sum(sales, na.rm = T)) %>% 
  ggplot(aes(fct_reorder(month, total_sales), total_sales, group = 1))+
  geom_col(fill = "#2171B5", alpha = .9)+
  coord_flip()+
  labs(
    title = "Total Sales by month",
    subtitle = "Sum of all sales for all years",
    x = "Month",
    y = "Total Sales"
  )+
  theme(
    plot.background = element_rect(fill = "gray90")
  )+ theme_bw()















