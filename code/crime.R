library(tidyverse)
library(ggplot2)

crime = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_Crime_Dataset.csv")
colnames(crime)
view(crime)
town = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv")
colnames(town)
view(town)

crime = crime %>%
  mutate(Year = as.integer(substr(Month, 1, 4)),
         District = str_extract(LSOAname, "^[^ ]+")) 

crime = crime %>%
  mutate(County = str_replace(County, " Police$", ""))



#Box plot– Drug Offense Rate per District (Two Diagrams)


# South Yorkshire Drug Offenses by District-Year
south_yorkshire = crime %>%
  filter(CrimeType == "Drugs", County == "South Yorkshire", !is.na(District)) %>%
  group_by(District, Year) %>%
  summarise(Offenses = n(), .groups = "drop")

# Boxplot for South Yorkshire
ggplot(south_yorkshire, aes(x = reorder(District, Offenses, FUN = median), y = Offenses)) +
  geom_boxplot(fill = "brown", outlier.alpha = 0.9) +
  labs(title = "South Yorkshire: Drug Offense Distribution by District",
       x = "District", y = "Offenses per Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# West Yorkshire Drug Offenses by District-Year
west_yorkshire = crime %>%
  filter(CrimeType == "Drugs", County == "West Yorkshire", !is.na(District)) %>%
  group_by(District, Year) %>%
  summarise(Offenses = n(), .groups = "drop")

# Boxplot for West Yorkshire
ggplot(west_yorkshire, aes(x = reorder(District, Offenses, FUN = median), y = Offenses)) +
  geom_boxplot(fill = "purple", outlier.alpha = 0.9) +
  labs(title = "West Yorkshire: Drug Offense Distribution by District",
       x = "District", y = "Offenses per Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








#Radar chart for Vehicle crime rate for any one of the two counties (for any specific month and year)

vehicle_data = crime %>%
  filter(CrimeType == "Vehicle crime",
         County == "West Yorkshire",
         Month == "2025-04",
         !is.na(District)) %>%
  group_by(District) %>%
  summarise(Crimes = n()) %>%
  arrange(desc(Crimes))

# Create radar-style polar bar chart
ggplot(vehicle_data, aes(x = reorder(District, Crimes), y = Crimes, fill = District)) +
  geom_col(show.legend = FALSE, color = "blue") +
  coord_polar(start = 0) +
  labs(title = "Radar-Style Chart: Vehicle Crime in West Yorkshire (April 2025)",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, angle = 90))






#Pie chart for Robbery rate for any one of two counties (for any specific month and year)

pie_data = crime %>%
  filter(CrimeType == "Robbery",
         County == "South Yorkshire",
         Month == "2025-04",
         !is.na(District)) %>%
  group_by(District) %>%
  summarise(Crimes = n())

ggplot(pie_data, aes(x = "", y = Crimes, fill = District)) +
  geom_col(width = 3, color = "brown") +
  coord_polar("y") +
  labs(title = "Robbery Rate – South Yorkshire (April 2025)", y = "", x = "") +
  theme_void() +
  theme(legend.position = "right")







#Line chart for Drug offense rates per 10,000 people for both counties in same diagram for all years

town_long = town %>%
  pivot_longer(
    cols = starts_with("Population"),
    names_to = "Year",
    names_prefix = "Population",
    values_to = "Population"
  ) %>%
  mutate(Year = as.integer(Year)) %>%  
  select(District, County, Year, Population)

drug_crime_by_district = crime %>%
  filter(tolower(CrimeType) == "drugs") %>%
  group_by(District, Year) %>%
  summarise(Total_Offenses = n(), .groups = "drop") %>%
  mutate(Year = as.integer(Year))  

drug_crime_by_district = drug_crime_by_district %>%
  mutate(District = str_to_upper(str_trim(District)))

town_long = town_long %>%
  mutate(District = str_to_upper(str_trim(District)))


merged_data = drug_crime_by_district %>%
  left_join(town_long, by = c("District", "Year")) %>%
  filter(!is.na(Population)) %>%
  mutate(Rate_per_10000 = (Total_Offenses / Population) * 10000)

final_data = merged_data %>%
  group_by(District, Year) %>%
  summarise(
    Total_Offenses = sum(Total_Offenses, na.rm = TRUE),
    Population = sum(Population, na.rm = TRUE),
    Rate_per_10000 = (Total_Offenses / Population) * 10000,
    .groups = "drop"
  )


ggplot(final_data, aes(x = Year, y = Rate_per_10000, color = District)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Drug Offense Rates per 10,000 People by District (2022–2024)",
    x = "Year",
    y = "Rate per 10,000",
    color = "District"
  ) + theme_minimal(base_size=14)
