# install the shiny themes package
install.packages("shinythemes")

pacman::p_load(
  tidyverse,
  shiny,
  janitor,
  shinythemes,
  babynames,
  gapminder
)

# load data
mobile <- read_csv("datasets/mobile_money_data.csv",
                   show_col_types = F)

# create the user interface
ui <- fluidPage(
  titlePanel("Laterite Mobile Money Data Explorer"),
  theme = shinytheme("readable"),
  DT::DTOutput("data")
)

# create server
server <- function(input, output, session){
  explore_data <- function(){
    mobile %>% 
      slice_sample(prop = .1)
  }
  output$data <- DT::renderDT({
    explore_data() %>% 
    DT::datatable()
  })
} 

# run app
shinyApp(ui, server)





ui <- fluidPage(
  titlePanel(
    "Baby Names Explorer"
  ),
  theme = shinytheme(
    "readable"
    ),
  sidebarLayout(
    sidebarPanel(
      selectInput("name", "Select Name", top_trendy_names$name)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          plotly::plotlyOutput("plot_trendy_names")
        ),
        tabPanel(
          "table",
          DT::DTOutput("table_trendy_names")
        )
      )
    )
  )
)

server <- function(input, output, session){
  # Function to plot trends in a name
  plot_trends <- function(){
    babynames %>% 
      filter(name == input$name) %>% 
      ggplot(aes(x = year, y = n)) +
      geom_col()
  }
  output$plot_trendy_names <- plotly::renderPlotly({
    plot_trends()
  })
  
  output$table_trendy_names <- DT::renderDT({
    babynames %>% 
      filter(name == input$name)
  })
}

# run app
shinyApp(ui = ui, server = server)








top_trendy_names <- read.table(
  "clipboard",
  header = F,
  col.names = c("id", "name", "sex",   "total",   "max", "nb_years", "trendiness")) %>% 
  select(-id)




# create a fluid page interface with a title panel and a sidebar layout
ui <- fluidPage(
  titlePanel(
    "Explore the Gapminder Dataset"
  ),
  # set the theme to "readable"
  theme = shinytheme(
    "readable"
  ),
  sidebarLayout(
    sidebarPanel(
      # create a dropdown menu for selecting a continent
      selectInput(
        "continent",
        "Select Continent",
        # populate the dropdown menu with the distinct values of the "continent" column in the gapminder dataset
        choices = gapminder %>% 
          distinct(continent),
        # set the default selection to "Africa"
        selected = "Africa"
      ),
      # create a slider for selecting a year
      sliderInput(
        "year",
        "Select Year",
        # set the minimum year to 1952
        min = 1952,
        # set the maximum year to 2007
        max = 2007,
        # set the default year to 1952
        value = 1952,
        # set the step size to 5
        step = 5
      )
    ),
    mainPanel(
      # create a tabset panel with two tabs: "Chart" and "Table"
      tabsetPanel(
        tabPanel(
          "Chart",
          # create a plotly output in the "Chart" tab
          plotly::plotlyOutput("plot")
        ),
        tabPanel(
          "Table",
          # create a DT output in the "Table" tab
          DT::DTOutput("table")
        )
      )
    )
  )
)

# create the server
server <- function(input, output, session){
  # create a function to generate the data to be plotted or displayed in the table
  generate_data <- function(){
    # filter the gapminder data by the selected continent and year
    gapminder %>% 
      filter(continent == input$continent) %>% 
      filter(year == input$year)
  }
  # create a function to generate the plot
  plot_function <- function(){
    # use the generate_data function to get the data to be plotted
    generate_data() %>%
      # create a scatter plot using ggplot
      ggplot(
        # map the x-axis to the "gdp_percap" column, y-axis to the "life_exp" column, 
        # color to the "country" column, and size to the "pop" column
        aes(
          x = gdp_percap,
          y = life_exp,
          color = country,
          size = pop
        )
      ) +
      # add points to the plot
      geom_point() +
      # use the "few" theme from the ggthemes package
      ggthemes::theme_few()+
      # add labels and a caption to the plot
      labs(
        title = "Relationship between Life Expectancy and GDP per Capita",
        subtitle = "Analysis of the gapminder Dataset",
        caption = "Data Source::Gapminder Library",
        x = "GDP per Capita",
        y = "Life Expectancy"
      )+
      # customize the plot background and remove the legend
      theme(
        plot.background = element_rect(fill = "gray90"),
        legend.position = "none"
      )
  }
  # create a plotly plot using the plot_function
  output$plot <- plotly::renderPlotly({
    plot_function()
  })
  # create a DT table using the generate_data function
  output$table <- DT::renderDT({
    generate_data()
  })
}

# run the Shiny app with the defined UI and server
shinyApp(ui, server)







