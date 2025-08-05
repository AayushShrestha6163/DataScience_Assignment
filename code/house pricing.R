library(tidyverse)

HousePrices = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_House_Prices.csv")


#Extracting Year from Date------------
library(lubridate)

HousePrices = HousePrices %>%
  mutate(Year = year(ymd(Date)))

unique(HousePrices$Year)
#-------------------------------------




#By County average by County & Year
county_year_avg = HousePrices %>%
  group_by(County, Year) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")

#Line graph: Average price 2021‑2024, both counties in same panel
ggplot(county_year_avg, aes(x = Year, y = AveragePrice, colour = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average House Prices by County (2021–2024)",
       x = "Year", y = "Average Price") +
  theme_minimal()



#By District
district_year_avg = HousePrices %>%
  group_by(District, Year) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")

Plot: Average price by District and Year
ggplot(district_year_avg, aes(x = Year, y = AveragePrice, colour = District)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average House Prices by District (2021–2024)",
       x = "Year", y = "Average Price") +
  theme_minimal() +
  theme(legend.position = "right")





#-------------------------------------------------------------------------------------------------------------------------------

#by county average by County in 2023 only
county_2023_avg = county_year_avg %>%
  filter(Year == 2023)

#Bar chart: Average price in 2023, both counties

ggplot(county_2023_avg, aes(x = County, y = AveragePrice, fill = County)) +
  geom_col(width = 0.3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average House Prices by County (2023)",
       x = NULL, y = "Average Price") +
  theme_minimal() +
  theme(legend.position = "none")



#by district
district_2023_avg = HousePrices %>%
  mutate(Year = year(ymd(Date))) %>%
  filter(Year == 2023) %>%
  group_by(District) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")

#Bar chart: Average house price by District in 2023
ggplot(district_2023_avg, aes(x = reorder(District, -AveragePrice), y = AveragePrice, fill = District)) +
  geom_col(width = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average House Prices by District (2023)",
       x = "District", y = "Average Price") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))



