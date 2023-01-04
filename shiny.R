# load required packages
pacman::p_load(
  tidyverse,
  shiny,
  janitor,
  shinythemes,
  gapminder
)


# This Shiny app allows users to explore the Gapminder dataset by selecting a continent and year 
# using dropdown menus and sliders. The app displays the resulting data in both a plot and table format. 
# The plot shows the relationship between life expectancy and GDP per capita, while the table displays the raw data.

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







