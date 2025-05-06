pacman::p_load(tidyverse, readxl, lubridate, janitor, shiny, plotly, shinythemes)

# App setup
ui <- navbarPage(
  title = "FAA Wildlife Strikes",
  theme = shinythemes::shinytheme("readable"),
  tabPanel("Overview",
           fluidPage(
             h3("Project Overview"),
             p("DATA-413 Final Project"),
             p("Carson Cherniss & Ainsley Gallagher"),
             h4("Research Question:"),
             p("During what time of day are wildlife strikes by aircraft most common?"), 
             h4("Dataset:"),
             p("The dataset comes from the Federal Aviation Administration (FAA) Wildlife Strike Database. 
             It contains detailed records of wildlife strikes involving civil aircraft in the United States. 
             This analysis uses data from 1990 to 2023, with approximately 300,000 observations and variables including 
               'incident date,' 'time of day,' 'species,' and 'location.'"), 
             h4("Methods:"),
             tags$ul(
               tags$li("Exploratory Data Analysis: Visualizations of wildlife strikes by time of day and hour of day."),
               tags$li("Seasonal Trends: Interactive plot allowing users to view monthly breakdowns."),
               tags$li("Statistical Test: Chi-square test to evaluate whether wildlife strikes are evenly distributed across different times of day.")
             )
           )
  ),
  tabPanel("Data Visualization: Time of Day",
           fluidPage(
             h3("Wildlife Strikes by Time of Day"),
             plotlyOutput("timeOfDayPlot"),
             p("Based on this visualization, it appears that wildlife strikes are most common during the day. 
               We investigate this further with statisitcal testing."),
             p("Without a codebook, we do not know what hours of the day were classified as 'day', 'night', 'dawn' or 'dusk'. 
               Thus, we can also visualize the distribution of wildlife strikes by hour of the day."),
             h3("Wildlife Strikes by Hour of Day"),
             plotlyOutput("timePlot"),
             p("Here, we can see that strikes appear most common between 7 and 11.
               Wildlife strikes seem common between the hours of 7 and 23. We can investigate this further with statistical testing.")
           )
  ),
  tabPanel("Data Visualization: Monthly Trends",
           fluidPage(
             h3("Wildlife Strikes by Month"),
             p("Data visualization has shown that wildlife strikes appear most common during the day.
               Those visualizations are for all strikes throughout the year. However, things like migration habits of birds may cause this trend to change throughout the year.
               With this visualization, we can see how trends change throughout the year."),
             selectInput("selected_month", "Select a Month:", choices = month.name),
             plotlyOutput("monthlyPlot"),
             p("Here, we can see that the overall trend of 'day' having the most wildlife strikes holds true for most months.
               However, October has an interesting discrepency, where 'night' has more strikes than 'day'.
               This could be due to bird migrations."), 
             p("The winter months (January, February and December) as well as the summer months (June, July and August) 
               have the greatest difference between the amount of 'day' and 'night' strikes.
               Whereas spring and fall months have a smaller difference between 'day' and 'night' strikes. This could be due to the migratory patterns of birds, 
               as birds tend to migrate in the fall and the spring and may be more active in the skies during these times.")
           )
  ), 
  tabPanel("Statistical Test",
           fluidPage(
             h3("Chi-Square Test Results")
           )
  )
)

# Server
server <- function(input, output) {
  # Load data inside the server so it only loads when the app runs
  faa_data <- reactive({
    read_excel("Public.xlsx") |>
      mutate(
        INCIDENT_DATE = as_date(INCIDENT_DATE),
        LUPDATE = as_date(LUPDATE),
        TIME = lubridate::hm(TIME)
      ) |>
      janitor::clean_names()
  })
  
  # Bar plot of time_of_day
  output$timeOfDayPlot <- renderPlotly({
    ggplotly(
      faa_data() |> 
        filter(!is.na(time_of_day)) |> 
        count(time_of_day) |> 
        ggplot(aes(x = fct_reorder(time_of_day, n), y = n, fill = time_of_day)) +
        geom_col() +
        labs(title = "Strikes by Time of Day", x = "Time of Day", y = "Number of Strikes")
    )
  })
  
  # Bar plot of time
  output$timePlot <- renderPlotly({
    ggplotly(
      faa_data() |> 
        filter(!is.na(time)) |> 
        mutate(hour = hour(time)) |> 
        ggplot(aes(x = hour)) +
        geom_histogram(binwidth = 1, fill = "tomato") +
        labs(title = "Wildlife Strikes by Hour of Day",
             x = "Hour",
             y = "Number of Strikes")
    )
  })
  
  # Monthly plot if we want to keep it
  output$monthlyPlot <- renderPlotly({
    req(input$selected_month)
    
    ggplotly(
      faa_data() |> 
        filter(!is.na(time_of_day)) |> 
        mutate(month = lubridate::month(incident_date, label = TRUE, abbr = FALSE)) |>
        filter(month == input$selected_month) |>
        count(time_of_day) |> 
        ggplot(aes(x = time_of_day, y = n, fill = time_of_day)) +
        geom_col() +
        labs(
          title = paste("Wildlife Strikes in", input$selected_month),
          x = "Time of Day",
          y = "Number of Strikes",
          fill = "Time of Day"
        )
    )
  })
}

# Run app
shinyApp(ui = ui, server = server)
