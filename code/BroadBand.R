library(tidyverse)

# Load cleaned broadband speed data
Broadband_speed <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_BroadBand_Speed.csv")

# Load town-level data
Towns <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv")
# Clean and standardize postcode formats
Broadband_speed <- Broadband_speed %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

Towns <- Towns %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

# Merge datasets on shortPostcode
BroadbandMerged <- Broadband_speed %>%
  left_join(Towns, by = "shortPostcode")

BroadbandMerged %>%
  filter(str_detect(tolower(County), "west yorkshire"),
         !is.na(Avg_Download),
         !is.na(District)) %>%
  ggplot(aes(x = reorder(District, Avg_Download, FUN = median), y = Avg_Download)) +
  geom_boxplot(fill = "green") +
  labs(title = "West Yorkshire: Download Speed by District",
       x = "District", y = "Avg Download Speed (Mbps)") +
  coord_flip() +
  theme_minimal()

BroadbandMerged %>%
  filter(str_detect(tolower(County), "south yorkshire"),
         !is.na(Avg_Download),
         !is.na(District)) %>%
  ggplot(aes(x = reorder(District, Avg_Download, FUN = median), y = Avg_Download)) +
  geom_boxplot(fill = "orange") +
  labs(title = "South Yorkshire: Download Speed by District",
       x = "District", y = "Avg Download Speed (Mbps)") +
  coord_flip() +
  theme_minimal()

BroadbandMerged %>%
  filter(str_detect(tolower(County), "west yorkshire"),
         !is.na(Avg_Download),
         !is.na(Town)) %>%
  ggplot(aes(x = reorder(Town, Avg_Download), y = Avg_Download)) +
  geom_col(fill = "steelblue") +
  labs(title = "West Yorkshire: Avg Download Speed by Town",
       x = "Town", y = "Avg Download Speed (Mbps)") +
  scale_y_continuous(labels = scales::label_number()) +
  coord_flip() +
  theme_minimal()
colnames(BroadbandMerged)
library(tidyverse)

# Load data
Broadband_speed <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_BroadBand_Speed.csv")
Towns <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv")

# Clean postcode for merging
Broadband_speed <- Broadband_speed %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

Towns <- Towns %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

# Merge datasets
BroadbandMerged <- Broadband_speed %>%
  left_join(Towns, by = "shortPostcode")





colnames(BroadbandMerged)
BroadbandMerged %>%
  filter(
    str_detect(tolower(County), "west yorkshire"),
    !is.na(Avg_Download),
    !is.na(District)
  ) %>%
  ggplot(aes(x = reorder(District, Avg_Download), y = Avg_Download)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "West Yorkshire: Avg Download Speed by District",
    x = "District",
    y = "Avg Download Speed (Mbps)"
  ) +
  scale_y_continuous(labels = scales::label_number()) +
  coord_flip() +
  theme_minimal()

BroadbandMerged %>%
  filter(
    str_detect(tolower(County), "south yorkshire"),
    !is.na(Avg_Download),
    !is.na(District)
  ) %>%
  ggplot(aes(x = reorder(District, Avg_Download), y = Avg_Download)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "South Yorkshire: Avg Download Speed by District",
    x = "District",
    y = "Avg Download Speed (Mbps)"
  ) +
  coord_flip() +   # Flip for better readability
  theme_minimal()

