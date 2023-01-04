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








