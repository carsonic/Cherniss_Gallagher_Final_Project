# Load Packages
pacman::p_load(tidyverse, readxl, lubridate, janitor)

# Read the first sheet
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

