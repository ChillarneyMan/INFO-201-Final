library('shiny')
library('ggplot2')
library('fmsb')
library('dplyr')
data <- read.csv('CleanedDataOfAllStudents.csv')
intro_page <- fluidPage(
  h1('introduction to our project (overview and background)')
)

page1 <- fluidPage(
  sidebarPanel(
    h1('Explore the data!'),
    p('Select two variables to see individual distributions and a scatterplot comparing the two variables!'),
    selectInput(
      inputId = 'variable',
      label = 'Select a variable',
      choices = c('Median_Household_Income', 'StudentCount', 'PercentLowGrowth', 'PercentTypicalGrowth', 'PercentHighGrowth')
    ),
    selectInput(
      inputId = 'variable2',
      label = 'Select another variable',
      choices = c('Median_Household_Income', 'StudentCount', 'PercentLowGrowth', 'PercentTypicalGrowth', 'PercentHighGrowth'),
      selected = 'StudentCount'
    ),
    selectInput(
      inputId = 'year', 
      label = 'Select year',
      choices = c('2015', '2016', '2017', '2018', '2019')
    ),
    br(),
    textOutput(outputId = 'p1text1'),
    br(),
    textOutput(outputId = 'p1text2')
  ),
  mainPanel(
    plotOutput(outputId = 'p1plot'),
    br(),
    plotOutput(outputId = 'p1plot2'),
    br(),
    plotOutput(outputId = 'p1plot3')
  )
)

page2 <- fluidPage(
  sidebarPanel(
    sliderInput(inputId = 'income_range', label = 'Select Income Range', min = min(data$Median_Household_Income), max = max(data$Median_Household_Income), value = c(min(data$Median_Household_Income), max(data$Median_Household_Income))),
    selectInput(inputId = 'subject', label = 'Select Subject', choices = unique(data$Subject))
  ),
  mainPanel(
            fluidRow(
              splitLayout(cellWidths = c("33%", "33%", "33%"), plotOutput("p2plot"), plotOutput("p2plot2"), plotOutput("p2plot3"))
            )
  )
)

page3 <- fluidPage(
  
)

ui <- navbarPage(
  'INFO 201 Final Project',
  tabPanel('About', intro_page),
  tabPanel('Explore', page1),
  tabPanel('Compare and Contrast', page2),
  tabPanel('Page 3', page3)
)

server <- function(input, output){
  output$p1plot <- renderPlot({
    plot1 <- ggplot(data[data$SchoolYear == input$year, ], aes(x = data[data$SchoolYear == input$year, input$variable])) +
      geom_histogram(color = 'Red') +
      labs(title = paste('Distribution of', input$variable), y = 'Number of Districts', x = input$variable) +
      theme(plot.title = element_text(size = 22))
    return(plot1)
  })
  output$p1plot2 <- renderPlot({
    plot1_2 <- ggplot(data[data$SchoolYear == input$year, ], aes(x = data[data$SchoolYear == input$year, input$variable2])) +
      geom_histogram(color = 'Blue') +
      labs(title = paste('Distribution of', input$variable2), y = 'Number of Districts', x = input$variable2) +
      theme(plot.title = element_text(size = 22))
    return(plot1_2)
  })
  output$p1plot3 <- renderPlot({
    plot1_3 <- ggplot(data[data$SchoolYear == input$year, ], aes(x = data[data$SchoolYear == input$year, input$variable], y = data[data$SchoolYear == input$year, input$variable2])) +
      geom_point(aes(color = Subject)) +
      labs(x = input$variable, y = input$variable2, title = paste(input$variable, 'Compared To', input$variable2)) +
      theme(plot.title = element_text(size = 22))
    return(plot1_3)
  })
  
  output$p1text1 <- renderText({
    if(input$variable == 'Median_Household_Income'){
      return('Median_Household_Income: This variable is the median household income in US dollars ($) within a given school district.')
    }
    if(input$variable == 'StudentCount'){
      return('StudentCount: This variable is the number of students in each district.')
    }
    if(input$variable == 'PercentLowGrowth'){
      return('PercentLowGrowth: This variable is the percentage of students in a given school district that had low growth from the previous school year.')
    }
    if(input$variable == 'PercentTypicalGrowth'){
      return('PercentTypicalGrowth: This variable is the percentage of students in a given school district that had average/expected growth from the previous school year.')
    }
    if(input$variable == 'PercentHighGrowth'){
      return('PercentHighGrowth: This variable is the percentage of students in a given school district that had high growth from the previous school year.')
    }
  })
  output$p1text2 <- renderText({
    if(input$variable2 == 'Median_Household_Income'){
      return('Median_Household_Income: This variable is the median household income in US dollars ($) within a given school district.')
    }
    if(input$variable2 == 'StudentCount'){
      return('StudentCount: This variable is the number of students in each district.')
    }
    if(input$variable2 == 'PercentLowGrowth'){
      return('PercentLowGrowth: This variable is the percentage of students in a given school district that had low growth from the previous school year.')
    }
    if(input$variable2 == 'PercentTypicalGrowth'){
      return('PercentTypicalGrowth: This variable is the percentage of students in a given school district that had average/expected growth from the previous school year.')
    }
    if(input$variable2 == 'PercentHighGrowth'){
      return('PercentHighGrowth: This variable is the percentage of students in a given school district that had high growth from the previous school year.')
    }
  })
  
  
  
  output$p2plot <- renderPlot({
    plot2 <- ggplot(summarize(group_by(data[data$Median_Household_Income >= input$income_range[1] & data$Median_Household_Income <= input$income_range[2] & data$Subject == input$subject, ], SchoolYear), 'low' = mean(PercentLowGrowth)), aes(x = SchoolYear, y = low)) +
      geom_area( fill = '#ffa07a', alpha=0.4) +
      geom_line(color = 'Orange') +
      coord_cartesian(ylim = c(0.25, 0.45)) +
      labs(x = 'Year', y = 'Percent of Students', title = 'Percent of Students with Low Yearly Growth')
    return(plot2)
  })
  output$p2plot2 <- renderPlot({
    plot2_2 <- ggplot(summarize(group_by(data[data$Median_Household_Income >= input$income_range[1] & data$Median_Household_Income <= input$income_range[2] & data$Subject == input$subject, ], SchoolYear), 'typical' = mean(PercentTypicalGrowth)), aes(x = SchoolYear, y = typical)) +
      geom_area( fill = '#bae1ff', alpha=0.4) +
      geom_line(color = '#189bcc') +
      coord_cartesian(ylim = c(0.25, 0.45)) +
      labs(x = 'Year', y = 'Percent of Students', title = 'Percent of Students with Typical Yearly Growth')
    return(plot2_2)
  })
  output$p2plot3 <- renderPlot({
    plot2_3 <- ggplot(summarize(group_by(data[data$Median_Household_Income >= input$income_range[1] & data$Median_Household_Income <= input$income_range[2] & data$Subject == input$subject, ], SchoolYear), 'high' = mean(PercentHighGrowth)), aes(x = SchoolYear, y = high)) +
      geom_area( fill = '#96ceb4', alpha=0.4) +
      geom_line(color = 'Green') +
      coord_cartesian(ylim = c(0.25, 0.45)) +
      labs(x = 'Year', y = 'Percent of Students', title = 'Percent of Students with High Yearly Growth')
    return(plot2_3)
  })
}

shinyApp( ui = ui, server = server)