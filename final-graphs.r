# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(ggdendro)

# Insert data and run given cleaning script
tuesdata <- tidytuesdayR::tt_load('2024-12-10')
parfumo_data_clean <- tuesdata$parfumo_data_clean

#Create specific cleaned data for 2010s to now

parfum_accords_tens <- parfumo_data_clean %>%
  select(Name, Brand, Release_Year, Main_Accords) %>%
  drop_na(Release_Year, Main_Accords) %>%
  filter(Release_Year > 2009)


# Separate each unique accord into a T/F variable per perfume
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



#Group all instances of accords into respective release years 

accord_summary_tens <- parfum_accords_tens %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords) %>%
  pivot_wider(
    names_from = Main_Accords, 
    values_from = n, 
    values_fill = 0 )

# Prep data for GGplot

trend_data_tens <- parfum_accords_tens %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords)


#Create the line graph
ggplot(trend_data_tens, aes(x = Release_Year, y = n, color = Main_Accords)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Scent Trends Over Time",
    x = "Year of Release",
    y = "Number of Perfumes",
    color = "Accord"
  ) +
  theme_minimal()


# Adjust line graphs for % of releases rather than raw # of releases
yearly_totals_tens <- parfum_accords_tens %>%
  group_by(Release_Year) %>%
  summarize(total_perfumes_that_year = n())

trend_percent_data_tens <- parfum_accords_tens %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords) %>%
  left_join(yearly_totals, by = "Release_Year") %>%
  mutate(pct = (n / total_perfumes_that_year) * 100)

ggplot(trend_percent_data_tens, aes(x = Release_Year, y = pct, color = Main_Accords)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Scent Popularity as % of Yearly Releases",
    subtitle = "Accounting for variation in the number of perfumes scraped per year",
    x = "Year",
    y = "Percentage of Total Perfumes (%)",
    color = "Accord"
  ) +
  theme_minimal()

#Trend Data Analysis
# Calculate the difference between the first 5 years and last 5 years of data
growth_analysis_tens <- trend_percent_data_tens %>%
  group_by(Main_Accords) %>%
  summarize(
    start_pct = mean(pct[Release_Year <= min(Release_Year) + 5], na.rm = TRUE),
    end_pct = mean(pct[Release_Year >= max(Release_Year) - 5], na.rm = TRUE),
    change = end_pct - start_pct
  )

# Identify the Top 5 Risers and Top 5 Fallers
top_risers_tens <- growth_analysis_tens %>% slice_max(change, n = 5) %>% pull(Main_Accords)
top_fallers_tens <- growth_analysis_tens %>% slice_min(change, n = 5) %>% pull(Main_Accords)

glimpse(top_risers_tens)
glimpse(top_fallers_tens)

# Plot top risers
ggplot(trend_percent_data_tens %>% filter(Main_Accords %in% top_risers_tens), 
       aes(x = Release_Year, y = pct, color = Main_Accords)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", alpha = 0.5) +
  labs(title = "The Rising Stars: Top 5 Accords with Biggest Increase",
       y = "% of Yearly Releases", x = "Year") +
  theme_minimal()


# Plot top fallers
ggplot(trend_percent_data_tens %>% filter(Main_Accords %in% top_fallers_tens), 
       aes(x = Release_Year, y = pct, color = Main_Accords)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", alpha = 0.5) +
  labs(title = "The Vanishing Notes: Top 5 Accords with Biggest Drop",
       y = "% of Yearly Releases", x = "Year") +
  theme_minimal()
