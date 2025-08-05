
library(tidyverse)
library(stringr)

school_2021_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2021-2022.csv")
school_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2022-2023.csv")
school_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2023-2024.csv")


#Boxplot for Average attainment 8 score 2022 – South Yorkshire (Variable District and Score)
filtered_data_south = school_2022_2023 %>%
  filter(
    toupper(TOWN) == "SOUTH YORKSHIRE" |
      toupper(ADDRESS3) == "SOUTH YORKSHIRE"
  ) %>%
  filter(!is.na(EBACCAPS))

ggplot(filtered_data_south, aes(x = LANAME, y = EBACCAPS, fill = LANAME)) +
  geom_boxplot() +
  labs(
    title = "Attainment 8 (EBACCAPS) by District - South Yorkshire (2022)",
    x = "District",
    y = "Attainment 8 Score (EBACCAPS)"
  ) +
  theme_minimal()







#Boxplot for Average attainment 8 score 2022 – West Yorkshire (Variable District and Score)
filtered_data_west = school_2022_2023 %>%
  filter(
    toupper(TOWN) == "WEST YORKSHIRE" |
      toupper(ADDRESS3) == "WEST YORKSHIRE"
  ) %>%
  filter(!is.na(EBACCAPS))

ggplot(filtered_data_west, aes(x = LANAME, y = EBACCAPS, fill = LANAME)) +
  geom_boxplot() +
  labs(
    title = "Attainment 8 (EBACCAPS) by District - West Yorkshire (2022)",
    x = "District",
    y = "Attainment 8 Score (EBACCAPS)"
  ) +
  theme_minimal()









#Line Graph to show the relationship between attainment 8 score and years over multiple districts in South Yorkshire and west Yorkshire

school_2021_2022 = school_2021_2022 %>%
  mutate(Year = 2022L)

school_2022_2023 = school_2022_2023 %>%
  mutate(Year = 2023L)

school_2023_2024 = school_2023_2024 %>%
  mutate(Year = 2024L)


# Binding all years
all_years_data = bind_rows(school_2021_2022, school_2022_2023, school_2023_2024)


all_years_data = all_years_data %>%
  mutate(Year = as.integer(Year))


filtered_all_years = all_years_data %>%
  filter(
    toupper(TOWN) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") |
      toupper(ADDRESS3) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")
  ) %>%
  filter(!is.na(EBACCAPS))


ggplot(filtered_all_years, aes(x = Year, y = EBACCAPS, colour = LANAME, group = LANAME)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2.5) +
  labs(
    title = "Average Attainment 8 Score (EBACCAPS) Over Years",
    subtitle = "South Yorkshire and West Yorkshire Districts",
    x = "Year",
    y = "Average Attainment 8 Score",
    colour = "District"
  ) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  scale_colour_manual(values = c(
    "Barnsley" = "#1f77b4",
    "Sheffield" = "#ff7f0e",
    "Rotherham" = "#2ca02c",
    "Doncaster" = "#d62728",
    "Leeds" = "#9467bd",
    "Bradford" = "#8c564b",
    "Wakefield" = "#e377c2",
    "Kirklees" = "#7f7f7f",
    "Calderdale" = "#bcbd22"
  )) +
  theme_minimal()