pacman::p_load(tidyverse, readxl, lubridate, janitor, shiny, plotly, shinythemes)

# App setup
ui <- navbarPage(
  title = "FAA Wildlife Strikes",
  theme = shinythemes::shinytheme("darkly"),
  tabPanel("Overview",
           fluidPage(
             h3("Project Overview"),
             p("DATA-413 Final Project"),
             p("Carson Cherniss & Ainsley Gallagher"),
             p("Research Question: During what time of day are wildlife strikes most common?")
           )
  ),
  tabPanel("EDA: Time of Day",
           fluidPage(
             h3("Wildlife Strikes by Time of Day"),
             plotlyOutput("timeOfDayPlot"), 
             h3("Wildlife Strikes by Hour of Day"),
             plotlyOutput("timePlot")
           )
  ),
  tabPanel("EDA: Monthly Trends",
           fluidPage(
             h3("Wildlife Strikes by Month"),
             selectInput("monthSelect", "Select a Month:", choices = month.name),
             plotlyOutput("monthlyPlot")
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
  # Load data *inside* the server so it only loads when the app runs
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
        mutate(month = lubridate::month(incident_date, label = TRUE, abbr = FALSE)) |>  # full month names
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
