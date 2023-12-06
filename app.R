library('shiny')
library('ggplot2')
library('fmsb')
library('dplyr')
data <- read.csv('CleanedDataOfAllStudents.csv')

intro_page <- fluidPage(
  h1('introduction to our project (overview and background)'),
)

page1 <- fluidPage(
  sidebarPanel(
    selectInput(
      inputId = 'variable',
      label = 'Select a Variable',
      choices = c('Median_Household_Income', 'StudentCount', 'PercentLowGrowth', 'PercentTypicalGrowth', 'PercentHighGrowth')
    ),
  ),
  plotOutput(outputId = 'p1plot'),
)

page2 <- fluidPage(
  
)

page3 <- fluidPage(
  
)

ui <- navbarPage(
  'INFO 201 Final Project',
  tabPanel('About', intro_page),
  tabPanel('Page 1', page1),
  tabPanel('Page 2', page2),
  tabPanel('Page 3', page3)
)

server <- function(input, output){
  output$p1plot <- renderPlot({
    plot1 <- ggplot(data, aes(x = data[ , input$variable])) +
      geom_histogram() +
      labs(title = paste('Distribution of', input$variable), y = 'Number of Districts', x = input$variable)
    return(plot1)
  })
}

shinyApp( ui = ui, server = server)