#-----------------------------------House Price vs Download Speed for both Counties in single diagram (include linear model summary report and correlation)---------------------------------------------------------------
library(tidyverse)
library(scales)

HousePrices = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_House_Prices.csv") %>%
  select(shortPostcode, Price)

BroadBandSpeed = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_BroadBand_Speed.csv") %>%
  select(shortPostcode, Median_Download)

Town <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv") %>%
  select(shortPostcode, District, County)



# 1. Aggregate or deduplicate each dataset by postcode
HousePrices_unique <- HousePrices %>%
  group_by(shortPostcode) %>%
  summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop")

BroadBandSpeed_unique <- BroadBandSpeed %>%
  group_by(shortPostcode) %>%
  summarise(Median_Download = mean(Median_Download, na.rm = TRUE), .groups = "drop")

Town_unique <- Town %>%
  distinct(shortPostcode, .keep_all = TRUE)

# 2. Join cleaned datasets
CombinedData <- HousePrices_unique %>%
  inner_join(BroadBandSpeed_unique, by = "shortPostcode") %>%
  inner_join(Town_unique, by = "shortPostcode") %>%
  filter(!is.na(Price) & !is.na(Median_Download) & !is.na(District))

# 3. Sample 100 rows from CombinedData
SampleData <- CombinedData %>%
  sample_n(100, replace = TRUE)



SampleModel = lm(Price ~ Median_Download, data = SampleData)


SampleData = SampleData %>%
  mutate(
    Predicted = predict(SampleModel),
    Residual = Price - Predicted
  )

class(SampleData$Town)
colnames(SampleData)


ggplot(SampleData, aes(x = Median_Download, y = Price)) +
  geom_point(aes(color = District), size = 2.5, alpha = 0.8) +  # color by District
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +  # regression line
  geom_segment(aes(xend = Median_Download, yend = Predicted), color = "black", alpha = 0.6) +  # residuals
  labs(
    title = "House Price vs Download Speed",
    x = "Median Download Speed (Mbps)",
    y = "House Price (£)",
    colour = "District"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()



FullModel = lm(Price ~ Median_Download, data = CombinedData)


correlation = cor(CombinedData$Price, CombinedData$Median_Download, use = "complete.obs")
cat("Correlation between Price and Download Speed:", correlation, "\n")

summary(FullModel)


#--------------------------------House price vs Drug rates (2023) per 10000 people for both counties in single diagram (include linear model summary report and correlation)-----------------------------------------


library(tidyverse)
library(lubridate)
library(scales)
library(stringr)

# House prices
HousePrices <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_House_Prices.csv") %>%
  mutate(
    Year = year(ymd(Date)),
    County = str_trim(str_to_upper(County)),
    shortPostcode = str_trim(str_to_upper(shortPostcode))
  ) %>%
  filter(Year == 2023) %>%
  select(shortPostcode, Price, County)

# Drug crimes
Crime <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    Year = as.integer(substr(Month, 1, 4)),
    County = str_replace(County, " Police$", ""),
    County = str_trim(str_to_upper(County))
  ) %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  group_by(County) %>%
  summarise(DrugCrimes = n(), .groups = "drop")

# County population
Population <- tibble(
  County = c("SOUTH YORKSHIRE", "WEST YORKSHIRE"),
  Population = c(1417000, 2342000)
)

# Crime rate
CrimeRate <- inner_join(Crime, Population, by = "County") %>%
  mutate(DrugRatePer10k = DrugCrimes / Population * 10000)

Town <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv") %>%
  mutate(
    shortPostcode = str_trim(str_to_upper(shortPostcode)),
    County = str_trim(str_to_upper(County)),
    Town = District  # ✅ Rename District to Town for clarity
  ) %>%
  select(shortPostcode, Town, County)

# Join house prices with towns
HousePrices_Town <- inner_join(HousePrices, Town, by = c("shortPostcode", "County"))

# Join with crime rate
CombinedData <- inner_join(HousePrices_Town, CrimeRate, by = "County") %>%
  filter(!is.na(Price), !is.na(DrugRatePer10k), !is.na(Town))  # ✅ Confirm Town exists

# Add jitter
CombinedData <- CombinedData %>%
  mutate(DrugRatePer10k_jitter = DrugRatePer10k + runif(n(), -0.05, 0.05))

# Fit model
FullModel <- lm(Price ~ DrugRatePer10k, data = CombinedData)

# Add predictions
CombinedData <- CombinedData %>%
  mutate(
    Predicted = predict(FullModel),
    Residual = Price - Predicted
  )

# Final plot
ggplot(CombinedData, aes(x = DrugRatePer10k_jitter, y = Price)) +
  geom_point(aes(color = Town), size = 2.5, alpha = 0.8) +
  geom_smooth(aes(x = DrugRatePer10k), method = "lm", se = FALSE, color = "red", size = 1.2) +
  geom_segment(aes(xend = DrugRatePer10k, yend = Predicted), color = "red", alpha = 0.6) +
  labs(
    title = "House Price vs Drug Crime Rate per 10,000 (2023)",
    x = "Drug Crime Rate per 10,000",
    y = "House Price (£)",
    colour = "Town"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "right")



cat("\n--- Linear Model Summary Report ---\n")
print(summary(FullModel))


correlation = cor(CombinedData$Price, CombinedData$DrugRatePer10k, use = "complete.obs")
cat("\n--- Correlation Analysis ---\n")
cat("Correlation between House Price and Drug Crime Rate per 10,000:", correlation, "\n")









#--------------------------------------Attainment 8 score vs House Price for both counties in single diagram (include linear model summary report and correlation)-----------------------------------------------------


library(tidyverse)
library(lubridate)
library(scales)
library(stringr)

HousePrices = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_House_Prices.csv") %>%
  mutate(
    Year = year(ymd(Date)),
    County = str_trim(str_to_upper(County)),
    shortPostcode = str_trim(str_to_upper(shortPostcode))
  ) %>%
  filter(Year == 2023) %>%
  select(shortPostcode, Price, County)


Town <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv") %>%
  mutate(
    shortPostcode = str_trim(str_to_upper(shortPostcode)),
    County = str_trim(str_to_upper(County)),
    Town = str_to_title(District)  
  ) %>%
  select(shortPostcode, Town, County)


HousePrices_Town = inner_join(HousePrices, Town, by = c("shortPostcode", "County"))

School_2021_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2021-2022.csv") %>%
  mutate(Year = 2022L)

School_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2022-2023.csv") %>%
  mutate(Year = 2023L)

School_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2023-2024.csv") %>%
  mutate(Year = 2024L)

AllSchools = bind_rows(School_2021_2022, School_2022_2023, School_2023_2024) %>%
  mutate(
    County = case_when(
      toupper(TOWN) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_upper(TOWN),
      toupper(ADDRESS3) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_upper(ADDRESS3),
      TRUE ~ NA_character_
    ),
    EBACCAPS = as.numeric(str_replace_all(EBACCAPS, "[^0-9.]", ""))  # remove non-numeric chars
  ) %>%
  filter(!is.na(County), !is.na(EBACCAPS)) %>%
  select(County, Year, EBACCAPS)

CombinedData = HousePrices_Town %>%
  inner_join(AllSchools, by = "County", relationship = "many-to-many") %>%
  rename(Attainment8 = EBACCAPS) %>%
  mutate(Attainment8_jitter = Attainment8 + runif(n(), -0.1, 0.1))


FullModel = lm(Price ~ Attainment8, data = CombinedData)

CombinedData = CombinedData %>%
  mutate(
    Predicted = predict(FullModel),
    Residual = Price - Predicted
  )

ggplot(CombinedData, aes(x = Attainment8_jitter, y = Price, color = Town)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(aes(x = Attainment8), method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
  geom_segment(aes(xend = Attainment8, yend = Predicted), color = "black", alpha = 0.5) +
  labs(
    title = "House Price vs Attainment 8 Score",
    x = "Attainment 8 Score",
    y = "House Price (£, 2023)",
    colour = "Town"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "right")

cat("\n--- Linear Model Summary Report ---\n")
print(summary(FullModel))

correlation = cor(CombinedData$Price, CombinedData$Attainment8, use = "complete.obs")
cat("\n--- Correlation Analysis ---\n")
cat("Correlation between House Price and Attainment 8 Score:", correlation, "\n")
#--------------------------------Attainment 8 scores vs Drug Offense rates per 10000 people---------------------------------------------------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)




School_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2022-2023.csv") %>%
  mutate(
    County = case_when(
      str_detect(str_to_upper(TOWN), "SOUTH YORKSHIRE") ~ "South Yorkshire",
      str_detect(str_to_upper(TOWN), "WEST YORKSHIRE") ~ "West Yorkshire",
      str_detect(str_to_upper(ADDRESS3), "SOUTH YORKSHIRE") ~ "South Yorkshire",
      str_detect(str_to_upper(ADDRESS3), "WEST YORKSHIRE") ~ "West Yorkshire",
      TRUE ~ NA_character_
    ),
    Town = str_to_title(LANAME),
    EBACCAPS = as.numeric(EBACCAPS)
  ) %>%
  filter(!is.na(County), !is.na(EBACCAPS), !is.na(Town)) %>%
  group_by(County, Town) %>%
  summarise(Attainment8 = mean(EBACCAPS, na.rm = TRUE), .groups = "drop")

Crime = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    Year = as.integer(substr(Month, 1, 4)),
    County = str_replace(County, " Police$", ""),
    County = str_to_title(County)
  ) %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  group_by(County) %>%
  summarise(DrugCrimes = n(), .groups = "drop")

Town = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv")



Population = tibble(
  County = c("South Yorkshire", "West Yorkshire"),
  Population = c(1417000, 2342000)
)


DrugRates = inner_join(Crime, Population, by = "County") %>%
  mutate(DrugRatePer10k = DrugCrimes / Population * 10000)


Combined = inner_join(School_2022_2023, DrugRates, by = "County") %>%
  mutate(
    Attainment8_jitter = Attainment8 + runif(n(), -0.1, 0.1)
  )


model = lm(Attainment8 ~ DrugRatePer10k * County, data = Combined)


Combined = Combined %>%
  mutate(
    Predicted = predict(model),
    Residual = Attainment8 - Predicted
  )


ggplot(Combined, aes(x = DrugRatePer10k, y = Attainment8_jitter, colour = Town)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(aes(y = Attainment8), method = "lm", se = TRUE, color = "red") +
  geom_segment(aes(xend = DrugRatePer10k, yend = Predicted), color = "blue", alpha = 2.9) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000",
    y = "Attainment 8 Score",
    colour = "Town"
  ) +
  theme_minimal() 


cor_value = cor(Combined$DrugRatePer10k, Combined$Attainment8)
cat("\n--- Correlation between Drug Rate and Attainment 8 Score ---\n")
cat("Correlation coefficient:", round(cor_value, 4), "\n")

cat("\n--- Linear Model Summary ---\n")
summary(model)









#-----------------------------------Average Download speed vs Drug Offense Rate per 10000 people for both counties in single diagram (include linear model summary report and correlation)------------------------------------------------


library(tidyverse)
library(stringr)
library(ggrepel)


Crime = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    County = str_replace(County, " Police$", ""),
    County = str_to_title(County),
    CrimeType = as.character(CrimeType),
    Town_Clean = str_trim(str_extract(LSOAname, "^[A-Za-z ]+"))
  ) %>%
  filter(CrimeType == "Drugs")
df <- read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv")
colnames(df)


Town = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv") %>%
  mutate(
    Town <- df %>%
      mutate(
        Town_Clean = str_to_title(str_trim(District)),
        County = str_to_title(County)
      )
    
  )

CrimeTown = Crime %>%
  group_by(County, Town_Clean, Year = as.integer(substr(Month, 1, 4))) %>%
  summarise(DrugCrimes = n(), .groups = "drop")

TownPop = Town %>%
  select(County, Town_Clean, Population = Population2023)

CrimeWithPopTown = inner_join(CrimeTown, TownPop, by = c("County", "Town_Clean")) %>%
  mutate(DrugRatePer10k = (DrugCrimes / Population) * 10000) %>%
  group_by(County, Town_Clean) %>%
  summarise(Avg_DrugRatePer10k = mean(DrugRatePer10k, na.rm = TRUE), .groups = "drop")

BroadBandSpeed = read_csv("C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_BroadBand_Speed.csv") %>%
  select(shortPostcode, Median_Download)

TownBroadband = inner_join(Town, BroadBandSpeed, by = "shortPostcode")

AvgDownloadTown = TownBroadband %>%
  group_by(County, Town_Clean) %>%
  summarise(Avg_Download = mean(Median_Download, na.rm = TRUE), .groups = "drop")

FinalTownData = inner_join(AvgDownloadTown, CrimeWithPopTown, by = c("County", "Town_Clean")) %>%
  filter(!is.na(Avg_Download), !is.na(Avg_DrugRatePer10k))

model_town = lm(Avg_DrugRatePer10k ~ Avg_Download, data = FinalTownData)

FinalTownData = FinalTownData %>%
  mutate(
    fitted = predict(model_town),
    residual = Avg_DrugRatePer10k - fitted
  )

large_resid = FinalTownData %>% 
  filter(abs(residual) > 1) %>%
  mutate(label = paste0(Town_Clean, "\nResidual: ", round(residual, 2)))

ggplot(FinalTownData, aes(x = Avg_Download, y = Avg_DrugRatePer10k, color = County)) +
  geom_jitter(size = 3, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1) +
  geom_segment(aes(xend = Avg_Download, yend = fitted), linetype = "dashed",max.overlaps = Inf ,color = "blue") +  
  geom_text_repel(data = large_resid, aes(label = label), size = 3, color = "blue") +
  labs(title = "Avg Download Speed vs Drug Offense Rate per 10,000 People",
       x = "Average Download Speed (Mbps)",
       y = "Drug Offense Rate (per 10,000 People)",
       color = "County") +
  theme_minimal()


cor_value_town = cor(FinalTownData$Avg_Download, FinalTownData$Avg_DrugRatePer10k)
cat("\n--- Correlation ---\n")


cat("Correlation between Download Speed and Drug Offense Rate:", round(cor_value_town, 4), "\n\n")

cat("--- Linear Model Summary ---\n")
print(summary(model_town))









