# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(ggdendro)

# Insert data and run given cleaning script
tuesdata <- tidytuesdayR::tt_load('2024-12-10')
parfumo_data_clean <- tuesdata$parfumo_data_clean

#Create specific cleaned data
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

