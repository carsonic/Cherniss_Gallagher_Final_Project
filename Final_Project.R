# Load Packages
pacman::p_load(tidyverse, readxl, lubridate, janitor, plotly)

# Read the data
faa_data <- read_excel("Public.xlsx")

glimpse(faa_data)

# Parse INCIDENT_DATE and LUPDATE and TIME as dates
faa_data <- faa_data |> 
  mutate(
    INCIDENT_DATE = as_date(INCIDENT_DATE),
    LUPDATE = as_date(LUPDATE),
    TIME = lubridate::hm(TIME)
  )

# Clean names using janitor package
faa_data <- faa_data |> 
  janitor::clean_names()

glimpse(faa_data)

# Research Questions: 
# During what time of day ("time_of_day" variable) are collisions most common? 

# Count occurrences of each time_of_day category
faa_data |> 
  count(time_of_day, sort = TRUE)

# Data Visualization

# Bar chart showing time_of_day variable distribution
faa_data |> 
  filter(!is.na(time_of_day)) |> 
  count(time_of_day) |> 
  ggplot(aes(x = fct_reorder(time_of_day, n), y = n)) +
  geom_col(fill = "tomato") +
  labs(
    title = "Wildlife Strikes by Time of Day",
    x = "Time of Day",
    y = "Number of Strikes"
  ) 

# Visually, it appears that the "day" has the most wildlife strikes
# However, without a codebook, we don't know which time of day is classified as which (day vs. night etc.)

# Visualization of the same distribution of strikes but using the "time" variable
faa_data |> 
  filter(!is.na(time)) |> 
  mutate(hour = hour(time)) |> 
  ggplot(aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "tomato") +
  labs(title = "Wildlife Strikes by Hour of Day",
       x = "Hour",
       y = "Number of Strikes")

# If we want to see if this pattern changes throughout the year, we can do that too
faa_data |> 
  filter(!is.na(time_of_day)) |> 
  mutate(month = lubridate::month(incident_date, label = TRUE)) |> 
  count(month, time_of_day) |> 
  ggplot(aes(x = month, y = n, fill = time_of_day)) +
  geom_col(position = "dodge") +
  labs(
    title = "Wildlife Strikes by Time of Day and Month",
    x = "Month",
    y = "Number of Strikes",
    fill = "Time of Day"
  ) 

# We can see that the Day variable seems to be consistently highest throughout the year, except in October which is interesting

# Statistical Test: 
# H0: Wildlife strikes are uniformly distributed across times of day.
# HA: Wildlife strikes are NOT uniformly distributed across times of day.
