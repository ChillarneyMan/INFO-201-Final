library('shiny')
library('ggplot2')
library('fmsb')
library('dplyr')
library('plotly')
data <- read.csv('CleanedDataOfAllStudents.csv')
df <- data %>%
  mutate(Rural = ifelse(StudentCount <= 110, "Yes", "No"))

intro_page <- fluidPage(
  h4("Introduction"),
  h5("Authors: Maxx Ferrian and Arnav Khare"),
  p("“Everybody is a genius, but if you judge a
               fish by its ability to climb a tree, it will live its whole live
               believing it is stupid.” For many students this is the reality. They spend much of their lives
               attending schools that are underfunded, and due to the schools inability to teach these students they 
               lose motivation and become to dislike the idea of learning. In order to fix the issue, we must first understand
               what is educational innequality and how can we work towards solving the problem."),
  p("Educational innequality is essentially the disparity between the quality of education in different school districts,
               where some school districts have lost of money and others do not. Innequality can also exist within a district as well, for example,
               the Seattle Public Schools struggles with educational innequality as schools like Roosevelt receive millions in funding and schools
               like Rainier Beach receive close to nothing. The cause of this disparity is actually rooted in our history"),
  p("Redlining, a discriminatory practice dating back to the 1930s in the US, systematically segregated communities based on 
               race, leading to socioeconomic disparities that persist today. Government-backed policies restricted mortgage lending in 
               predominantly Black neighborhoods, creating segregated areas marked as risky for investment. These practices resulted in 
               unequal access to quality education, as redlined areas faced underfunded schools and limited resources. This historical discrimination 
               entrenched educational disparities, perpetuating unequal opportunities for minority students. Redlining's lasting impact continues to 
               shape educational inequality, influencing funding allocation, school quality, and access to resources, highlighting the systemic nature 
               of educational inequity rooted in discriminatory policies of the past."),
  p("In order to view the current state of edcuation in the United States, we wanted to ask a few question to further guide our discovery. How
               different population metrics affect the number of districts impacted by that metric? How the data changes over time when comparing median test
               scores of different districts? Finally, how are rural and urban schools impacted differently in testing?")
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
    plotOutput(outputId = 'p1plot3'),
    br(),
    br(),
    br(),
    h1('Discoveries:'),
    p('Both the Median_Household_Income and StudentCount are distributed with a heavy focus on the lower end, which means as those two variables increase there is a smaller sample size, which when compared to a different variable there will be less and less points as you increase.'),
    p('Despite this, you can still make out a general trend line, which is shown with the added regression line on the scatter plot.'),
    p('When you compare high growth to income, you see that generally there is an increase in percent of high growth students as the median household incoe increases. Inversely when you look at low growth.'),
    p('Interestingly, you will see similar, but slightly less significant changes when comparing the percent of students in a certain growth category and the student count in a district.'),
    p('This makes more sense when you see that there is somewhat of a correlation between the student count and the median household income.')
  )
)

page2 <- fluidPage(
  sidebarPanel(
    sliderInput(inputId = 'income_range', label = 'Select Income Range', min = min(data$Median_Household_Income), max = max(data$Median_Household_Income), value = c(min(data$Median_Household_Income), max(data$Median_Household_Income))),
    selectInput(inputId = 'subject', label = 'Select Subject', choices = unique(data$Subject)),
    br(),
    p('Adjust the median household income range to see how the the percentage of students in the three different growth categories has changed from 2015 to 2019. You can additionally break this down by the specific subject of either language arts or math.')
  ),
  mainPanel(
            fluidRow(
              splitLayout(cellWidths = c("33%", "33%", "33%"), plotOutput("p2plot"), plotOutput("p2plot2"), plotOutput("p2plot3"))
            ),
            br(),
            h1('Discoveries:'),
            p('If you make the income range to be about $35000, slide it all the way to the upper end, and select English Language Arts, then you will see that for high income areas there is a higher percentage of students in high growth than typical and low growth. But over the years, the percent of high growth students is decreasing to be closer to the typical growth and the low growth is increasing to the same point.'),
            p('If you then slide the range all the way down to the low end, you will see the opposite happen, the high growth students are increasing to be closer to typical growth and the low growth students are decreasing to the typical growth percentage.'),
            p('If you put the slider in the middle approximately, you will see practically no change to the percentages, only a slight increase in high growth and a slight decrease in low growth.'),
            br(),
            br(),
            p('Now if you repeat this same process but while selecting the Math subject, you will notice that there is almost zero net change over the time range for any of the the percentages in any income group.'),
            p('Interestingly, both low income and middle range income are very similar, but high income has a significantly larger percentage of high growth and a significantly lower percentage of low growth.')
  )
)

page3 <- fluidPage(
  fluidRow(
    column(6,
           h3("Educational Testing in Rural vs Non-Rural"),
           sliderInput(inputId = "slider", label = "Select a year:",
                       min = min(df$SchoolYear), max = max(df$SchoolYear), value = min(df$SchoolYear), step = 1),
           plotlyOutput("pie_chart_rural"),  # Display the pie chart for rural students here
           plotlyOutput("pie_chart_non_rural"),  # Display the pie chart for non-rural students here
           p("In many rural areas, access to quality education can be limited due to factors like sparse population density,
                        fewer resources, and geographical isolation. This often results in reduced educational opportunities, impacting
                        both the curriculum and teacher availability. In contrast, non-rural areas typically benefit from more
                        concentrated educational resources, facilitating better infrastructure, diverse academic programs, and a larger
                        pool of educators. Such disparities in educational access and resources between rural and non-rural settings
                        significantly influence the overall learning experiences and outcomes of students. The way we deicide if a district
                        is rural is based on the schools population. If a school has fewer than 110 kids in a 10 mile radius then we consider
                        it rural. Above there are two charts. One shows the number of students who showed low, medium, and high growth in test scores for rural
                        places, and the other shows the same but for non-rural schools. The difference in data is no much between the two different types, 
                        however, the rate of growth is. Rural districts saw a roughly one percent increase in low growth percentage, meaning that 
                        growth rates did not increase. 1 percent may not sounds like alot, however, when looking at 1 percent in terms of the total population
                        the impact is of a few hundred students which is significant. A change that non-rural districts did not have."),
           p("We have chosen a pie chart for the data representation because it best showcases the 3 different categories of low, medium, and high
                        educational growth rates")
    )
  )
)

ui <- navbarPage(
  #'INFO 201 Final Project',
  titlePanel("Educational Inequality in Washington State"),
  tabsetPanel(
  tabPanel('About', intro_page),
  tabPanel('Page 1', page1),
  tabPanel('Page 2', page2),
  tabPanel('Page 3', page3)
  )
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
      geom_smooth(method = 'lm', se = FALSE) +
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
  
  
  output$pie_chart_rural <- renderPlotly({
    filtered_data <- df[df$Rural == "Yes" & df$SchoolYear == input$slider, ]
    
    low_growth_count <- sum(filtered_data$NumberLowGrowth)
    typical_growth_count <- sum(filtered_data$NumberTypicalGrowth)
    high_growth_count <- sum(filtered_data$NumberHighGrowth)
    
    pie_data <- data.frame(
      category = c("Low Growth", "Typical Growth", "High Growth"),
      count = c(low_growth_count, typical_growth_count, high_growth_count)
    )
    
    pie_chart <- plot_ly(pie_data, labels = ~category, values = ~count, type = "pie") %>%
      layout(title = "Growth Distribution - Rural Students")
    
    return(pie_chart)
  })
  
  output$pie_chart_non_rural <- renderPlotly({
    filtered_data <- df[df$Rural == "No" & df$SchoolYear == input$slider, ]
    
    low_growth_count <- sum(filtered_data$NumberLowGrowth)
    typical_growth_count <- sum(filtered_data$NumberTypicalGrowth)
    high_growth_count <- sum(filtered_data$NumberHighGrowth)
    
    pie_data <- data.frame(
      category = c("Low Growth", "Typical Growth", "High Growth"),
      count = c(low_growth_count, typical_growth_count, high_growth_count)
    )
    
    pie_chart <- plot_ly(pie_data, labels = ~category, values = ~count, type = "pie") %>%
      layout(title = "Growth Distribution - Non-Rural Students")
    
    return(pie_chart)
  })
}

shinyApp( ui = ui, server = server)