# Load Packages
pacman::p_load(tidyverse, readxl)

# Read the first sheet
faa_data <- read_excel("Public.xlsx")
