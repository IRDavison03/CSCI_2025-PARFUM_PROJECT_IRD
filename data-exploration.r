library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(ggdendro)

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

# Separate each unique accord into a T/F variable per perfume
perfume_matrix <- parfum_year_accords %>%
  select(Release_Year, Main_Accords) %>%
  mutate(row_id = row_number()) %>%             # Keep track of original rows
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  mutate(present = 1) %>%                       # Create a marker for the "count"
  pivot_wider(
    names_from = Main_Accords, 
    values_from = present, 
    values_fill = 0                             # Replace NAs with 0, mark as false
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

# Create the subset for 2014
creamy_2014 <- creamy_matrix %>%
  filter(Release_Year == 2014)

# View the result in Positron
View(creamy_2014)



#Accord trends but from 2010 instead of 2000
parfum_year_accords_2010 <- parfumo_data_clean %>%
  select(Name, Brand, Release_Year, Concentration, Main_Accords, Top_Notes, Middle_Notes, Base_Notes) %>%
  drop_na(Release_Year, Main_Accords) %>%
  filter(Release_Year > 2009)

perfume_matrix2 <- parfum_year_accords_2010 %>%
  select(Release_Year, Main_Accords) %>%
  mutate(row_id = row_number()) %>%             # Keep track of original rows
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  mutate(present = 1) %>%                       # Create a marker for the "count"
  pivot_wider(
    names_from = Main_Accords, 
    values_from = present, 
    values_fill = 0                             # Replace NAs with 0
  )


accord_summary2 <- parfum_year_accords_2010 %>%
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
trend_data2 <- parfum_year_accords_2010 %>%
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
yearly_totals2 <- parfum_year_accords_2010 %>%
  group_by(Release_Year) %>%
  summarize(total_perfumes_that_year = n())

# 2. Break down accords and join with the totals
trend_percentage_data2 <- parfum_year_accords_2010 %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Release_Year, Main_Accords) %>%
  left_join(yearly_totals, by = "Release_Year") %>%
  mutate(pct = (n / total_perfumes_that_year) * 100)

# 3. Plot the percentage trend
ggplot(trend_percentage_data2, aes(x = Release_Year, y = pct, color = Main_Accords)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(min(brand_trends$Release_Year), 
                                  max(brand_trends$Release_Year), 
                                  by = 1)) +
  labs(
    title = "Scent Popularity as % of Yearly Releases",
    subtitle = "Accounting for variation in the number of perfumes scraped per year",
    x = "Year",
    y = "Percentage of Total Perfumes (%)",
    color = "Accord"
  ) +
  theme_minimal()


# NEW
# 1. Calculate the total number of "Creamy" perfumes per year across ALL brands
total_creamy_per_year <- creamy_matrix %>%
  group_by(Release_Year) %>%
  summarize(total_creamy_count = n())

# 2. Calculate each brand's count and join with the yearly totals
brand_share_data <- creamy_matrix %>%
  group_by(Release_Year, Brand) %>%
  summarize(brand_creamy_count = n(), .groups = "drop") %>%
  left_join(total_creamy_per_year, by = "Release_Year") %>%
  mutate(share_pct = (brand_creamy_count / total_creamy_count) * 100)

# 3. Identify the Top 10 brands (by total volume of creamy perfumes) to keep the graph readable
top_10_creamy_brands <- creamy_matrix %>%
  count(Brand, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  pull(Brand)

# 4. Plot the results
ggplot(brand_share_data %>% filter(Brand %in% top_10_creamy_brands), 
       aes(x = Release_Year, y = share_pct, color = Brand)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(min(brand_share_data$Release_Year), 
                                  max(brand_share_data$Release_Year), 
                                  by = 2)) + # Label every 2 years to save space
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Brand 'Market Share' of Creamy Perfumes",
    subtitle = "Percentage of each year's total creamy releases attributed to each brand",
    x = "Year",
    y = "Share of Annual Creamy Releases (%)",
    color = "Brand"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graphs that Aren't line graphs

# 1. Create a FULL binary matrix (not just filtered for creamy)
# We'll limit this to the Top 50 accords to keep the calculation clean
top_50_accords <- parfum_year_accords %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Main_Accords, sort = TRUE) %>%
  slice_head(n = 50) %>%
  pull(Main_Accords)

full_matrix <- parfum_year_accords %>%
  mutate(row_id = row_number()) %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  filter(Main_Accords %in% top_50_accords) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Main_Accords, values_from = present, values_fill = 0)

# 2. Calculate Correlation of every column with the "Creamy" column
# We remove metadata columns (row_id, Brand, Release_Year) first
cor_data <- full_matrix %>%
  select(where(is.numeric), -row_id, -Release_Year) %>%
  cor() %>%
  as.data.frame() %>%
  select(Creamy) %>%                  # Look specifically at Creamy's relationships
  rownames_to_column("Accord") %>%
  filter(Accord != "Creamy") %>%      # Remove the self-correlation (which is 1)
  slice_max(Creamy, n = 15)           # Get the top 15 strongest correlations

# 3. Build the Correlation Graph
ggplot(cor_data, aes(x = reorder(Accord, Creamy), y = Creamy, fill = Creamy)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "navajowhite", high = "chocolate4") +
  labs(
    title = "Which Accords are Most 'Attracted' to Creamy?",
    subtitle = "Correlation coefficient showing the strength of association",
    x = "Accord",
    y = "Correlation with 'Creamy'",
    fill = "Strength"
  ) +
  theme_minimal()


#HEATMAP
# 1. Identify the top 25 accords to keep the map clean
top_accords <- parfum_year_accords %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  count(Main_Accords, sort = TRUE) %>%
  slice_head(n = 25) %>%
  pull(Main_Accords)

# 2. Create a binary matrix for these accords
# We use all_of() to ensure we only select the accords in our list
scent_matrix <- parfum_year_accords %>%
  mutate(row_id = row_number()) %>%
  separate_longer_delim(Main_Accords, delim = ", ") %>%
  filter(Main_Accords %in% top_accords) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Main_Accords, values_from = present, values_fill = 0) %>%
  select(all_of(top_accords))

# 3. Calculate the correlation matrix
# This calculates the Pearson correlation coefficient ($r$) for every pair
cor_matrix <- cor(scent_matrix)

# 4. Turn the matrix into a "Long" format for ggplot
cor_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column("Accord_A") %>%
  pivot_longer(-Accord_A, names_to = "Accord_B", values_to = "Correlation")

# 5. Generate the Heatmap
ggplot(cor_long, aes(x = Accord_A, y = Accord_B, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#0571b0",   # Blue for negative correlation
    high = "#ca0020",  # Red for positive correlation
    mid = "white", 
    midpoint = 0, 
    limit = c(-0.2, 0.6) # Adjusted limits to highlight subtle relationships
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major = element_blank()
  ) +
  labs(
    title = "The Scent Map: Correlation Between Top Accords",
    subtitle = "Red clusters indicate 'Scent Families' that are frequently paired together",
    x = NULL, y = NULL
  )


#Branch map attempt
# 1. Use the correlation matrix we built for the Scent Map
# We convert correlation (similarity) into "distance" (dissimilarity)
dist_matrix <- as.dist(1 - cor_matrix)

# 2. Perform Hierarchical Clustering
clusters <- hclust(dist_matrix, method = "ward.D2")

# 3. Create the Dendrogram plot
ggdendrogram(clusters, rotate = TRUE, theme_dendro = FALSE) +
  theme_minimal() +
  labs(
    title = "Fragrance Family Tree",
    subtitle = "Accords grouped by how often they are paired together in formulas",
    x = "Accord",
    y = "Distance (Dissimilarity)"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )

library(dendextend)

# 1. Convert our cluster results into a 'dendrogram' object
dend <- as.dendrogram(clusters)

# 2. Color the branches based on 5 main scent families (clusters)
# You can change k = 5 to 4 or 6 depending on how many groups you see
dend <- dend %>% 
  color_branches(k = 5) %>% 
  color_labels(k = 5)

# 3. Plot the results
# We use 'horiz = TRUE' because it's easier to read the accord names
par(mar = c(5, 2, 2, 10)) # Adjust margins to make room for labels
plot(dend, horiz = TRUE, main = "Clustered Scent Families")