# Load required library
library(tidyverse)
library(lubridate)  # For date handling

# Load data from CSV files
first_file <- read.csv("Data/sold_houses_Remaining.csv", header = TRUE)
second_file <- read.csv("Data/sold_houses_till_Franklin.csv", header = TRUE)

# Combine both datasets into a single dataframe
canberra_sold_houses <- bind_rows(first_file, second_file)

# Convert 'sold_date' column to Date format (assuming day-month-year format)
canberra_sold_houses <- canberra_sold_houses %>%
  mutate(sold_date = dmy(sold_date))

# Calculate the number of unique years in the dataset
num_years <- canberra_sold_houses %>%
  mutate(sale_year = year(sold_date)) %>%  # Extract year from 'sold_date'
  distinct(sale_year) %>%  # Keep only unique years
  summarise(total_years = n())  # Count number of unique years

# View cleaned dataset (removing rows with missing values in 'price' or 'sold_date')
canberra_sold_houses %>%
  drop_na(price, sold_date) %>%
  View()  # Opens the cleaned dataset in RStudio's Viewer

# Filter houses sold from 2015 onwards
canb_houses_till2015 <- canberra_sold_houses %>%
  mutate(sale_year = year(sold_date)) %>%
  filter(sale_year >= 2015)