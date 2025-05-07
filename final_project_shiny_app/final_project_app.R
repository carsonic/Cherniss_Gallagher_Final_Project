pacman::p_load(tidyverse, readxl, lubridate, janitor, shiny, plotly, shinythemes)

# App setup
ui <- navbarPage(
  title = "FAA Wildlife Strikes",
  theme = shinythemes::shinytheme("cerulean"),
  tabPanel("Overview",
           fluidPage(
             h3("Project Overview"),
             p("DATA-413 Final Project"),
             p("Carson Cherniss & Ainsley Gallagher"),
             p("This project combines exploratory and inferential analysis to investigate the relationship between time of day and the frequency of wildlife strikes reported by the FAA. Our goal is to understand when wildlife strikes are most common and determine whether the observed distribution differs significantly from what we would expect by chance."), 
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
               tags$li("Monthly and Species Trends: Interactive plots allowing users to view monthly and species breakdowns. These plots could offer explanations for why we got the results we did for our statistical test."),
               tags$li("Statistical Test: We used a Chi-square goodness-of-fit test to determine whether the categorical variable time_of_day shows a uniform distribution or not.")
             ), 
             h4("Download Data:"),
             downloadButton("download_data", "Download Cleaned FAA Data")
           )
  ),
  tabPanel("Data Visualization: Time of Day",
           fluidPage(
             h3("Wildlife Strikes by Time of Day"),
             plotlyOutput("timeOfDayPlot"),
             p("Based on this visualization, it appears that wildlife strikes are most common during the day. 
               We investigate this further with statistical testing."),
             p("Without a codebook, we do not know what hours of the day were classified as 'day', 'night', 'dawn' or 'dusk'. 
               Thus, we can also visualize the distribution of wildlife strikes by hour of the day."),
             h3("Wildlife Strikes by Hour of Day"),
             plotlyOutput("timePlot"),
             p("Here, we can see that strikes appear most common between 7 and 11.
               Wildlife strikes seem common between the hours of 7 and 23. The lack of strikes between the hours of 0 and 6 makes sense, as this is when the least amount of flights would be occuring.")
           )
  ),
  tabPanel("Data Visualization: Monthly and Species Trends",
           fluidPage(
             h3("Wildlife Strikes by Month"),
             p("Data visualization has shown that wildlife strikes appear most common during the day.
               Those visualizations are for all strikes throughout the year. However, things like migration habits of birds may cause this trend to change throughout the year.
               With this visualization, we can see how trends change throughout the year."),
             selectInput("selected_month", "Select a Month:", choices = month.name),
             plotlyOutput("monthlyPlot"),
             p("Here, we can see that the overall trend of 'day' having the most wildlife strikes holds true for most months.
               However, October has an interesting discrepancy, where 'night' has more strikes than 'day'.
               This could be due to bird migrations."), 
             p("The winter months (January, February and December) as well as the summer months (June, July and August) 
               have the greatest difference between the amount of 'day' and 'night' strikes.
               Whereas spring and fall months have a smaller difference between 'day' and 'night' strikes. This could be due to the migratory patterns of birds, 
               as birds tend to migrate in the fall and the spring and may be more active in the skies during these times."),
             p("The higher amount of night strikes in the fall could be due to the fact that the sun sets earlier, so there are more 'night' hours.
               However, if this were the case, we would expect to see more 'night' strikes in the winter months as well as the fall, which we do not."),
             h3("Wildlife Strikes by Species"),
             uiOutput("species_selector"),
             plotlyOutput("speciesPlot"),
             p("This can help us understand if certain species are more active, and thus more likely to be struck, during particular times of day.")
           )
  ),
  tabPanel("Statistical Test",
           fluidPage(
             h3("Chi-Square Test Results"), 
             p("To formally test whether wildlife strikes are evenly distributed across time-of-day categories, we used a Chi-square goodness-of-fit test."),
             p(strong("Null hypothesis (H₀):"), " Wildlife strikes are evenly distributed across time-of-day categories."),
             p(strong("Alternative hypothesis (H₁):"), " Wildlife strikes are not evenly distributed across time-of-day categories."),
             h4("Observed Counts:"),
             tableOutput("chi_counts"),
             h4("Observed vs. Expected Counts:"),
             plotlyOutput("obs_exp_plot"), 
             p("The Chi-square test compares the observed number of strikes in each time-of-day category to the counts we would expect if wildlife strikes occurred uniformly throughout the day (i.e., equal probability for 'dawn', 'day', 'dusk', and 'night')."),
             h4("Test Summary:"),
             verbatimTextOutput("chi_result"),
             p("Because the p-value is less than 0.001, we reject the null hypothesis. This provides strong evidence that wildlife strikes are not evenly distributed across times of day. 
               The distribution is significantly skewed, with most strikes occurring during daylight hours, as our earlier exploratory plots suggested.")
           )
  ), 
  tabPanel("Conclusions",
           fluidPage(
             h3("Conclusion and Discussion"),
             p("Our analysis of FAA wildlife strike data from 1990 to 2023 found that wildlife strikes are significantly more likely to occur during the day. 
       This conclusion is supported by both exploratory visualizations and a Chi-square statistical test rejecting the null hypothesis of equal distribution across times of day."),
             h4("Why This Matters:"),
             tags$ul(
               tags$li("Airlines and airports may benefit from increased awareness or mitigation efforts during daylight hours."),
               tags$li("Understanding time-of-day and seasonal trends can inform flight scheduling, wildlife hazard management, and radar monitoring strategies.")
             ),
             h4("Limitations:"),
             tags$ul(
               tags$li("Time-of-day labels are unclear without a codebook, limiting precise interpretation."),
               tags$li("The FAA dataset may underreport minor strikes or vary by airport reporting standards.")
             ),
             h4("Future Work:"),
             tags$ul(
               tags$li("Incorporate airport-level variables (e.g., location, size, habitat) to account for spatial differences."),
               tags$li("Compare trends before and after major bird migration seasons.")
             )
           )
  )
)

# Server
server <- function(input, output) {
  # Load data inside the server so it only loads when the app runs
  # Some changes had to be made to the data cleaning when using csv instead of excel file
  faa_data <- reactive({
    read_csv("Public.csv") |> 
      janitor::clean_names() |> 
      mutate(
        # Dates are already date type, so no need to parse
        # incident_date = as_date(incident_date),
        # lupdate = as_date(lupdate),
        # time = lubridate::hm(time)
      )
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
  
  # Monthly plot 
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
  
  # Species Plot
  output$species_selector <- renderUI({
    species_choices <- faa_data() |> 
      filter(!is.na(species), !is.na(time_of_day)) |>  # filter out species with all NA time_of_day
      count(species) |> 
      arrange(desc(n)) |> 
      pull(species)
    
    selectInput("selected_species", "Select a Species:", choices = species_choices)
  })
  
  output$speciesPlot <- renderPlotly({
    req(input$selected_species)
    
    ggplotly(
      faa_data() |> 
        filter(!is.na(time_of_day), species == input$selected_species) |> 
        count(time_of_day) |> 
        ggplot(aes(x = time_of_day, y = n, fill = time_of_day)) +
        geom_col() +
        labs(
          title = paste("Wildlife Strikes Involving", input$selected_species),
          x = "Time of Day",
          y = "Number of Strikes",
          fill = "Time of Day"
        )
    )
  })
  
  # Chi-square test reactive expression
  chi_test_result <- reactive({
    data <- faa_data() |> 
      filter(!is.na(time_of_day)) |> 
      count(time_of_day) |> 
      arrange(time_of_day)
    
    test <- chisq.test(data$n)
    expected <- test$expected
    
    data <- data |> 
      mutate(expected = expected)
    
    list(counts = data, result = test)
  })
  
  output$chi_counts <- renderTable({
    chi_test_result()$counts
  })
  
  output$chi_result <- renderPrint({
    chi_test_result()$result
  })
  
  output$obs_exp_plot <- renderPlotly({
    data <- chi_test_result()$counts |> 
      pivot_longer(cols = c(n, expected), names_to = "type", values_to = "count")
    
    ggplotly(
      ggplot(data, aes(x = time_of_day, y = count, fill = type)) +
        geom_col(position = "dodge") +
        labs(
          title = "Observed vs. Expected Wildlife Strike Counts",
          x = "Time of Day",
          y = "Number of Strikes",
          fill = "Count Type"
        ) +
        scale_fill_manual(values = c("n" = "dodgerblue3", "expected" = "tomato"),
                          labels = c("Observed", "Expected"))
    )
  })

  # Download Data Through Shiny App
  output$download_data <- downloadHandler(
    filename = function() {
      paste("cleaned_faa_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      faa_data() |> 
        write_csv(file)
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)
