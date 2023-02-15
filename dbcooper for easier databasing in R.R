# load essential packages
pacman::p_load(tidyverse, DBI, RSQLite, dbcooper)

# create a database connection (SQLite connection)
lite_connect <- dbConnect(drv = SQLite(), dbname = "datasets/sample_db.sqlite")

# write the airquality table from datasets package into the database
dbWriteTable(conn = lite_connect, name = "airquality", value = airquality)

# write a few tables into the database
dbWriteTable(conn = lite_connect, name = "iris", value = iris )

# initialize functions using dbc_init()
dbc_init(con = lite_connect, con_id = "mysql")

# list all the tables in the database
mysql_list()

# work with database tables as local R objects
mysql_airquality() %>% select(everything()) %>% summary()

p <- mysql_airquality() %>% janitor::clean_names() %>% group_by(month) %>% 
    summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% as_tibble() %>% 
    ggplot(aes(month, mean_temp)) + geom_line(linewidth = 1, color = "#2171B5")+ geom_point(size = 1.5)+
    theme_bw()+labs(
      y = "Average Temperature", title = "Monthly Average Temperature", 
      subtitle = "Analysis of Airquality dataset", caption = "Data Source: R's datasets package"
      )

plotly::ggplotly(p = p)

mysql_query(
  query = "SELECT * FRom airquality WHERE Ozone IS NOT NULL OR `Solar.R` IS NULL"
)

mysql_query("select month, avg(Wind) from airquality group by month")


mysql_airquality() %>% 
  as_tibble() %>% janitor::clean_names() %>% group_by(day) %>% 
  summarise(mean_wind = mean(wind)) %>% 
  ggplot(aes(day, mean_wind)) + geom_line() + geom_point()+ theme_bw()+
  labs(y = "Average Wind", x = "Day of the Month", 
       title = "Daily mean Wind for five months", 
       subtitle = "Analysis of Airquality dataset", 
       caption = "Data Source: R's datasets package"
       ) -> p1
plotly::ggplotly(p = p1)








