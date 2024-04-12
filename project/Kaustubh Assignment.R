# Install and load required packages
install.packages(c("tidyverse", "plotly", "ggplot2"))
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)

# Import datasets
Unicef <- read_csv("Unicef.csv")
Meta_Data <- read_csv("Meta_Data.csv")

# Rename column in UNICEF
colnames(Unicef)[colnames(Unicef) == 'time_period'] <- 'year'

# Join datasets
data_join <- full_join(Unicef, Meta_Data)

# Filter dataset
data_filtered <- data_join[c('country', 'year', 'Life expectancy at birth, total (years)','GNI (current US$)','Population, total')]
colnames(data_filtered)[colnames(data_filtered) == 'Life expectancy at birth, total (years)'] <- 'LifeExp'

# TASK 1: Average of Life Expectancy
Avg_LE <- data_filtered %>%
  group_by(country) %>%
  summarize(Avg_LE = mean(LifeExp, na.rm = TRUE))
Avg_LE <- na.omit(Avg_LE)

# Import World Map coordinates
map_world <- map_data("world")
colnames(map_world)[colnames(map_world) == 'region'] <- 'country'

# New Map Dataset
map_data_join <- full_join(Avg_LE, map_world)


# Plot the map with legend at the top
ggplot(map_data_join) + 
  aes(x = long, y = lat, group = group, fill = Avg_LE) + 
  geom_polygon() + 
  labs(fill = "Average Life Expectancy") +
  theme(legend.position = "top")

# TASK 2: Average Gross Net Income
colnames(data_filtered)[colnames(data_filtered) == 'GNI (current US$)'] <- 'GNI'
Avg_GNI <- data_filtered %>%
  group_by(country) %>%
  summarize(Avg_GNI = mean(GNI, na.rm = TRUE))
Avg_GNI <- na.omit(Avg_GNI)


# New Map Dataset
map_GNI <- full_join(Avg_GNI, map_world)

# Plot the map with adjusted legend
ggplot(map_GNI) + 
  aes(x = long, y = lat, group = group, fill = Avg_GNI) + 
  geom_polygon() +
  labs(fill = "Average Gross National Income (in US $)") +
  scale_fill_continuous(label = function(x) {
    paste(round(x / 1e12), "Trillion", sep = " ")
  }) +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm"),  # Adjust legend key width
        legend.text = element_text(size = 8))  # Adjust legend text size



# TASK 3: Total Population for Each Country
colnames(data_filtered)[colnames(data_filtered) == 'Population, total'] <- 'Population'
country_population <- data_filtered %>%
  group_by(country) %>%
  summarize(Total_Population = sum(Population, na.rm = TRUE)) %>%
  arrange(desc(Total_Population)) %>%
  top_n(10)

# Filter the original data to include only the top 10 countries
data_filtered_top10 <- data_filtered %>%
  filter(country %in% country_population$country)

# Create separate Dataframe for Graph
CP <- data_filtered_top10[c('country', 'year', 'Population')]

# Create the time series plot
timeseries_plot <- CP %>%
  na.omit() %>% 
  ggplot() +
  aes(year, Population, group = country, colour = country) +
  geom_line() +
  scale_y_continuous(labels = function(x) paste(round(x / 1e6), "M", sep = "")) +
  labs(x = "Year", y = "Population", title = "Decades of Demography: Top 10 countires by population from 1960 to 2022")

ggplotly(timeseries_plot)

# TASK 4: Average of Military Expenditures
colnames(Meta_Data)[colnames(Meta_Data) == 'Military expenditure (% of GDP)'] <- 'ME'
Avg_ME <- Meta_Data %>%
  group_by(country) %>%
  summarize(Avg_MEI = round(mean(ME, na.rm = TRUE), digits = 2))

# Top 10 countries with highest Military Expenditures
Avg_ME_10 <- Avg_ME %>%
  filter(country %in% country_population$country)

# Plot bar Graph
ggplot(Avg_ME_10, aes(x = country, y = Avg_MEI)) +
  geom_bar(stat = "identity", fill = rgb(runif(n = nrow(Avg_ME_10), min = 0, max = 255) / 255,
                                         runif(n = nrow(Avg_ME_10), min = 0, max = 255) / 255,
                                         runif(n = nrow(Avg_ME_10), min = 0, max = 255) / 255)) +
  geom_text(aes(label = Avg_MEI, vjust = -1, hjust = 0.5)) +
  labs(title = "Military Spending Leaders: % of GDP (Top 10 countries)",
       x = "Country",
       y = "Average Percentage") +
  theme_classic() +
  theme_bw() +
  coord_cartesian(ylim = c(0, max(Avg_ME_10$Avg_MEI) * 1.1))  # Adjust the multiplier as needed
