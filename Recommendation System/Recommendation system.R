library(tidyverse)

library(scales)
library(stringr)
library(ggplot2)

HousePrices = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_House_Prices.csv") %>%
  select(shortPostcode, Price, County) %>%
  mutate(
    shortPostcode = str_replace_all(shortPostcode, " ", "") %>% toupper(),
    County = str_to_upper(str_trim(County))
  )

BroadBandSpeed = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_BroadBand_Speed.csv") %>%
  select(shortPostcode, Median_Download) %>%
  mutate(shortPostcode = str_replace_all(shortPostcode, " ", "") %>% toupper())

Crime = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    County = str_remove(County, " Police"),
    County = str_to_upper(str_trim(County))
  ) %>%
  group_by(County) %>%
  summarise(CrimeCount = n(), .groups = "drop")

School_2021_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2021-2022.csv") %>%
  select(LANAME, EBACCAPS)

School_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2022-2023.csv") %>%
  select(LANAME, EBACCAPS)

School_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2023-2024.csv") %>%
  select(LANAME, EBACCAPS)

unique(School_2021_2022$EBACCAPS)


SchoolAll <- bind_rows(School_2021_2022, School_2022_2023, School_2023_2024) %>%
  mutate(
    EBACCAPS = str_trim(EBACCAPS),                    # Clean whitespace
    EBACCAPS = na_if(EBACCAPS, "SUPP"),               # Remove "SUPP"
    EBACCAPS = na_if(EBACCAPS, "NE"),                 # Remove "NE"
    EBACCAPS = as.numeric(EBACCAPS),                  # Convert to numeric
    LANAME = str_to_title(LANAME)                    # Capitalize LANAME
  ) %>%
  filter(!is.na(EBACCAPS)) %>%                        # Remove NAs after conversion
  group_by(LANAME) %>%
  summarise(
    AvgEBACCAPS = mean(EBACCAPS, na.rm = TRUE), 
    .groups = "drop"
  )



TownData <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv") %>%
  select(shortPostcode, District, County) %>%
  rename(Town = District) %>%  # Rename District to Town (if that's what you meant)
  mutate(
    shortPostcode = str_replace_all(shortPostcode, " ", "") %>% toupper(),
    Town = toupper(str_trim(Town)),
    County = toupper(str_trim(County))
  )



mapping = tribble(
  ~Town,       ~LANAME,    ~County,
  "SHEFFIELD",     "Sheffield", "SOUTH YORKSHIRE",
  "BARNSLEY",      "Barnsley",  "SOUTH YORKSHIRE",
  "DONCASTER",     "Doncaster", "SOUTH YORKSHIRE",
  "ROTHERHAM",     "Rotherham", "SOUTH YORKSHIRE",
  "LEEDS",         "Leeds",     "WEST YORKSHIRE",
  "BRADFORD",      "Bradford",  "WEST YORKSHIRE",
  "HUDDERSFIELD",  "Kirklees",  "WEST YORKSHIRE",
  "WAKEFIELD",     "Wakefield", "WEST YORKSHIRE",
  "HALIFAX",       "Calderdale","WEST YORKSHIRE"
)

Town_mapped = Town %>%
  left_join(mapping, by = c("Town", "County")) %>%
  mutate(LANAME = str_to_title(LANAME))

HousePricesSummary = HousePrices %>%
  group_by(shortPostcode) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

BroadbandSummary = BroadBandSpeed %>%
  group_by(shortPostcode) %>%
  summarise(MedianDownload = median(Median_Download, na.rm = TRUE), .groups = "drop")

TownCombined = Town_mapped %>%
  left_join(HousePricesSummary, by = "shortPostcode") %>%
  left_join(BroadbandSummary, by = "shortPostcode") %>%
  left_join(Crime, by = "County") %>%
  left_join(SchoolAll, by = "LANAME")

TownCombined_clean = TownCombined %>%
  filter(!is.na(AvgPrice), !is.na(MedianDownload), !is.na(CrimeCount), !is.na(AvgEBACCAPS))

TownScored = TownCombined_clean %>%
  mutate(
    Score_House = rescale(AvgPrice, to = c(0, 10)),
    Score_Broadband = rescale(MedianDownload, to = c(0, 10)),
    Score_Crime = rescale(-CrimeCount, to = c(0, 10)),
    Score_School = rescale(AvgEBACCAPS, to = c(0, 10)),
    OverallScore = (Score_House + Score_Broadband + Score_Crime + Score_School) / 4
  ) %>%
  arrange(desc(OverallScore))

Top2Towns_all = TownScored %>%
  distinct(Town, .keep_all = TRUE) %>%
  slice_head(n = 3) %>%
  select(Town, County, AvgPrice, MedianDownload, CrimeCount, AvgEBACCAPS, OverallScore)

print(Top2Towns_all)
view(Top2Towns_all)


#Bar chart of top 2 towns by OverallScore
ggplot(Top2Towns_all, aes(x = reorder(Town, OverallScore), y = OverallScore, fill = County)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#FF6F61", "WEST YORKSHIRE" = "#6BAED6")) +
  labs(title = "Top 2 Towns by Overall Score", x = "Town", y = "Overall Score (0-10)") +
  theme_minimal()





#-------------------Score towns using 3 available variables-----------------------------------
TownScored = TownCombined %>%
  filter(!is.na(AvgPrice) & !is.na(CrimeCount) & !is.na(AvgEBACCAPS)) %>%
  mutate(
    PriceScore = rescale(AvgPrice, to = c(0, 10)),       # Higher = better
    CrimeScore = rescale(-CrimeCount, to = c(0, 10)),    # Lower = better
    SchoolScore = rescale(AvgEBACCAPS, to = c(0, 10))    # Higher = better
  ) %>%
  mutate(
    OverallScore = round((PriceScore + CrimeScore + SchoolScore) / 3, 2)
  )


Top3_Towns = TownScored %>%
  arrange(desc(OverallScore)) %>%
  distinct(Town, .keep_all = TRUE) %>%  
  slice(1:3)


ggplot(Top3_Towns, aes(x = OverallScore, y = fct_reorder(Town, OverallScore), fill = County)) +
  geom_col() +
  labs(
    title = "Top 3 Recommended Towns (House, Crime, School)",
    x = "Overall Score (0–10)",
    y = "Town"
  ) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#FC766AFF", "WEST YORKSHIRE" = "#00CFC1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(face = "bold")
  )

#-----------------------------------------------------------------------------------------------------------------------------











#------------------------------------Individual Data set plots and scores-----------------------------------------------------------------------------------------------------------------------

# Top 3 House Prices
Top3_House = TownScored %>%
  arrange(desc(AvgPrice)) %>%
  slice(1:3) %>%
  
  select(Town, County, AvgPrice)

# Top 3 Broadband
Top3_Broadband = TownScored %>%
  arrange(desc(MedianDownload)) %>%
  slice(1:3) %>%
  
  select(Town, County, MedianDownload)

# Top 3 Lowest Crime
Top3_Crime = TownScored %>%
  arrange(CrimeCount) %>%
  slice(1:3) %>%
  select(Town, County, CrimeCount)

# Top 3 School Performance
Top3_School = TownScored %>%
  arrange(desc(AvgEBACCAPS)) %>%
  slice(1:3) %>%
  select(Town, County, AvgEBACCAPS)



cat("\nTop 3 Towns by House Prices:\n")
print(Top3_House)


cat("\nTop 3 Towns by Broadband Speed:\n")
print(Top3_Broadband)

cat("\nTop 3 Towns by Lowest Crime:\n")
print(Top3_Crime)

cat("\nTop 3 Towns by School Performance:\n")
print(Top3_School)


library(ggplot2)
library(gridExtra)  # For arranging multiple plots

#Bar Chart for Top 3 House Prices
plot_house <- ggplot(Top3_House, aes(x = reorder(Town, AvgPrice), y = AvgPrice, fill = County)) +
  geom_col() +
  labs(title = "Top 3 Towns by House Price", x = "Town", y = "Average House Price (£)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

#Bar Chart for Top 3 Broadband Speed
plot_broadband <- ggplot(Top3_Broadband, aes(x = reorder(Town, MedianDownload), y = MedianDownload, fill = County)) +
  geom_col() +
  labs(title = "Top 3 Towns by Broadband Speed", x = "Town", y = "Median Download Speed (Mbps)") +
  theme_minimal()

#Bar Chart for Top 3 Lowest Crime
plot_crime <- ggplot(Top3_Crime, aes(x = reorder(Town, -CrimeCount), y = CrimeCount, fill = County)) +
  geom_col() +
  labs(title = "Top 3 Towns with Lowest Crime Count", x = "Town", y = "Crime Count") +
  theme_minimal()

#Bar Chart for Top 3 School Performance
plot_school <- ggplot(Top3_School, aes(x = reorder(Town, AvgEBACCAPS), y = AvgEBACCAPS, fill = County)) +
  geom_col() +
  labs(title = "Top 3 Towns by School Performance", x = "Town", y = "Avg EBACC APS Score") +
  theme_minimal()

grid.arrange(plot_house, plot_broadband, plot_crime, plot_school, ncol = 2)











