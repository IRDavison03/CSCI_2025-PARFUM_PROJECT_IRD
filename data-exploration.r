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

# Remove parfumes that have no number
drop_number <- parfumo_data_clean %>% 
  drop_na(Number)

# Remove parfumes that have no name
drop_name <- parfumo_data_clean %>% 
  drop_na(Name)

# Remove Parfumes that have no brand
drop_brand <- parfumo_data_clean %>% 
  drop_na(Brand)

# Remove Parfums that have no release year
drop_release <- parfumo_data_clean %>% 
  drop_na(Release_Year)

# Remove parfumes that have no concentratoin
drop_concentration <- parfumo_data_clean %>% 
  drop_na(Concentration)

# Remove parfumes that have no accords
drop_accords <- parfumo_data_clean %>% 
  drop_na(Main_Accords)

# Find all Eau De Toilette Parfumes
toilette <- parfumo_data_clean |>
  filter(Concentration == "Eau de Toilette")

# Find the most common concentrations of parfume
 parfum_types <- parfumo_data_clean %>% 
  count(Concentration)

# Find out which brands are most represented in the dataset
parfum_brands <- parfumo_data_clean %>% 
  count(Brand)

# Find if certain brands produce more of certain concentrations
parfum_brandtype <- drop_concentration %>% 
  count(Brand, Concentration)

# Getting Specific now! We actually use this.
# This removes all of the data that isn't relevant and removes all
# Parfumes released after 1999, so we're only looking at 2000 to now basically.
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


#Trend data analysis START

# Calculate the difference between the first 5 years and last 5 years of data
growth_analysis <- trend_percentage_data %>%
  group_by(Main_Accords) %>%
  summarize(
    start_pct = mean(pct[Release_Year <= min(Release_Year) + 5], na.rm = TRUE),
    end_pct = mean(pct[Release_Year >= max(Release_Year) - 5], na.rm = TRUE),
    change = end_pct - start_pct
  )

# Identify the Top 5 Risers and Top 5 Fallers
top_risers <- growth_analysis %>% slice_max(change, n = 5) %>% pull(Main_Accords)
top_fallers <- growth_analysis %>% slice_min(change, n = 5) %>% pull(Main_Accords)

# Plot top risers
ggplot(trend_percentage_data %>% filter(Main_Accords %in% top_risers), 
       aes(x = Release_Year, y = pct, color = Main_Accords)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", alpha = 0.5) +
  labs(title = "The Rising Stars: Top 5 Accords with Biggest Increase",
       y = "% of Yearly Releases", x = "Year") +
  theme_minimal()

glimpse(top_risers)

# Plot top fallers
ggplot(trend_percentage_data %>% filter(Main_Accords %in% top_fallers), 
       aes(x = Release_Year, y = pct, color = Main_Accords)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", alpha = 0.5) +
  labs(title = "The Vanishing Notes: Top 5 Accords with Biggest Drop",
       y = "% of Yearly Releases", x = "Year") +
  theme_minimal()

glimpse(top_fallers)


creamy_matrix <- parfum_year_accords %>%
  # 1. Keep only rows where 'Creamy' is mentioned in the Accords
  filter(str_detect(Main_Accords, "Creamy")) %>%
  mutate(row_id = row_number()) %>%             # Keep track of original rows
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  mutate(present = 1) %>%                       # Create a marker for the "count"
  pivot_wider(
    names_from = Main_Accords, 
    values_from = present, 
    values_fill = 0                             # Replace NAs with 0
  )

view(creamy_matrix)

# Count releases per brand per year
brand_trends <- creamy_matrix %>%
  group_by(Release_Year, Brand) %>%
  summarize(perfume_count = n(), .groups = "drop")



# Find the top 10 brands with the most creamy perfumes
top_creamy_brands <- creamy_matrix %>%
  count(Brand, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  pull(Brand)

# Plot only those brands
brand_trends %>%
  filter(Brand %in% top_creamy_brands) %>%
  ggplot(aes(x = Release_Year, y = perfume_count, color = Brand)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(title = "Top 5 Brands: Creamy Perfume Release Trends")
