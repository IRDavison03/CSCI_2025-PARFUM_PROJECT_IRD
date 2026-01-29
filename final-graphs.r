# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(ggdendro)

# Insert data and run given cleaning script
tuesdata <- tidytuesdayR::tt_load('2024-12-10')
parfumo_data_clean <- tuesdata$parfumo_data_clean

#Create specific cleaned data for 2000s to now, 2010 to now, and 2020 to now
parfum_accords_millenium <- parfumo_data_clean %>%
  select(Name, Brand, Release_Year, Main_Accords) %>%
  drop_na(Release_Year, Main_Accords) %>%
  filter(Release_Year > 1999)

parfum_accords_tens <- parfumo_data_clean %>%
  select(Name, Brand, Release_Year, Main_Accords) %>%
  drop_na(Release_Year, Main_Accords) %>%
  filter(Release_Year > 2009)

parfum_accords_twenties <- parfumo_data_clean %>%
  select(Name, Brand, Release_Year, Main_Accords) %>%
  drop_na(Release_Year, Main_Accords) %>%
  filter(Release_Year > 2019)

# Separate each unique accord into a T/F variable per perfume
perfume_matrix_mil <- parfum_accords_millenium %>%
  select(Release_Year, Main_Accords) %>%
  mutate(row_id = row_number()) %>%             # Keep track of original rows
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  mutate(present = 1) %>%                       # Create a marker for the "count"
  pivot_wider(
    names_from = Main_Accords, 
    values_from = present, 
    values_fill = 0                             # Replace NAs with 0, mark as false
  )

perfume_matrix_tens <- parfum_accords_tens %>%
  select(Release_Year, Main_Accords) %>%
  mutate(row_id = row_number()) %>%             
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  mutate(present = 1) %>%                      
  pivot_wider(
    names_from = Main_Accords, 
    values_from = present, 
    values_fill = 0                             
  )

perfume_matrix_twenty <- parfum_accords_twenties %>%
  select(Release_Year, Main_Accords) %>%
  mutate(row_id = row_number()) %>%             
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  mutate(present = 1) %>%                      
  pivot_wider(
    names_from = Main_Accords, 
    values_from = present, 
    values_fill = 0                            
  )


#Group all instances of accords into respective release years 
accord_summary_mill <- parfum_accords_millenium %>%
  #Break the list strings into individual rows
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  #Group by Year and the Accord name to count occurrences
  count(Release_Year, Main_Accords) %>%
  #Pivot the Accords into columns
  pivot_wider(
    names_from = Main_Accords, 
    values_from = n, 
    values_fill = 0 )

accord_summary_tens <- parfum_accords_tens %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords) %>%
  pivot_wider(
    names_from = Main_Accords, 
    values_from = n, 
    values_fill = 0 )

accord_summary_twenty <- parfum_accords_twenties %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords) %>%
  pivot_wider(
    names_from = Main_Accords, 
    values_from = n, 
    values_fill = 0 )

# Prep data for GGplot for each 'era'
trend_data_mill <- parfum_accords_millenium %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords)

trend_data_tens <- parfum_accords_tens %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords)

trend_data_twenty <- parfum_accords_twenties %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords)

#Create the line graph for each 'era'
ggplot(trend_data_mill, aes(x = Release_Year, y = n, color = Main_Accords)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Scent Trends Over Time",
    x = "Year of Release",
    y = "Number of Perfumes",
    color = "Accord"
  ) +
  theme_minimal()

ggplot(trend_data_tens, aes(x = Release_Year, y = n, color = Main_Accords)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Scent Trends Over Time",
    x = "Year of Release",
    y = "Number of Perfumes",
    color = "Accord"
  ) +
  theme_minimal()

ggplot(trend_data_twenty, aes(x = Release_Year, y = n, color = Main_Accords)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Scent Trends Over Time",
    x = "Year of Release",
    y = "Number of Perfumes",
    color = "Accord"
  ) +
  theme_minimal()