library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

tuesdata <- tidytuesdayR::tt_load('2024-12-10')

parfumo_data_clean <- tuesdata$parfumo_data_clean

glimpse(parfumo_data_clean)

# Removes any row with at least one NA in any column
pristine_parfum <- parfumo_data_clean %>% 
  drop_na(Name, Brand, Release_Year, Concentration, Main_Accords)

glimpse(pristine_parfum)

# Top_Notes, Middle_Notes, Base_Notes

drop_number <- parfumo_data_clean %>% 
  drop_na(Number)

drop_name <- parfumo_data_clean %>% 
  drop_na(Name)

drop_brand <- parfumo_data_clean %>% 
  drop_na(Brand)

drop_release <- parfumo_data_clean %>% 
  drop_na(Release_Year)

drop_concentration <- parfumo_data_clean %>% 
  drop_na(Concentration)

drop_accords <- parfumo_data_clean %>% 
  drop_na(Main_Accords)

toilette <- parfumo_data_clean |>
  filter(Concentration == "Eau de Toilette")

 parfum_types <- parfumo_data_clean %>% 
  count(Concentration)

parfum_brands <- parfumo_data_clean %>% 
  count(Brand)

parfum_brandtype <- drop_concentration %>% 
  count(Brand, Concentration)

parfum_year_accords <- parfumo_data_clean %>%
  select(Name, Brand, Release_Year, Concentration, Main_Accords, Top_Notes, Middle_Notes, Base_Notes) %>%
  drop_na(Release_Year, Main_Accords) %>%
  filter(Release_Year > 1999)

perfume_matrix <- parfum_year_accords %>%
  select(Release_Year, Main_Accords) %>%
  mutate(row_id = row_number()) %>%             # Keep track of original rows
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  mutate(present = 1) %>%                       # Create a marker for the "count"
  pivot_wider(
    names_from = Main_Accords, 
    values_from = present, 
    values_fill = 0                             # Replace NAs with 0
  )


accord_summary <- parfum_year_accords %>%
  # 1. Break the list strings into individual rows
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  
  # 2. Group by Year and the Accord name to count occurrences
  count(Release_Year, Main_Accords) %>%
  
  # 3. Pivot the Accords into columns
  pivot_wider(
    names_from = Main_Accords, 
    values_from = n, 
    values_fill = 0 )


# 1. Prepare the data (keep it in long format for ggplot)
trend_data <- parfum_year_accords %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords)

# 2. Create the line graph
ggplot(trend_data, aes(x = Release_Year, y = n, color = Main_Accords)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Scent Trends Over Time",
    x = "Year of Release",
    y = "Number of Perfumes",
    color = "Accord"
  ) +
  theme_minimal()

# 1. Calculate the total unique perfumes per year first
yearly_totals <- parfum_year_accords %>%
  group_by(Release_Year) %>%
  summarize(total_perfumes_that_year = n())

# 2. Break down accords and join with the totals
trend_percentage_data <- parfum_year_accords %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords) %>%
  left_join(yearly_totals, by = "Release_Year") %>%
  mutate(pct = (n / total_perfumes_that_year) * 100)

# 3. Plot the percentage trend
ggplot(trend_percentage_data, aes(x = Release_Year, y = pct, color = Main_Accords)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Scent Popularity as % of Yearly Releases",
    subtitle = "Accounting for variation in the number of perfumes scraped per year",
    x = "Year",
    y = "Percentage of Total Perfumes (%)",
    color = "Accord"
  ) +
  theme_minimal()
