pacman::p_load(tidyverse, readxl, lubridate, janitor, shiny, plotly)

# App setup
ui <- fluidPage(
  titlePanel("FAA Wildlife Strikes"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_month",
        label = "Select a Month:",
        choices = month.name,
        selected = "January"
      )
    ),
    mainPanel(
      plotlyOutput("timeOfDayPlot"),
      plotlyOutput("timePlot"),
      plotlyOutput("monthlyPlot")
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
    req(input$selected_month)  # ensures a selection is made
    
    ggplotly(
      faa_data() |> 
        filter(!is.na(time_of_day)) |> 
        mutate(month = lubridate::month(incident_date, label = TRUE, abbr = FALSE)) |>  # full month names
        filter(month == input$selected_month) |>  # filter by the selected month
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
