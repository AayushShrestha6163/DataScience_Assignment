library(tidyverse)


PopulationData =read.csv("C:/DataScience-R/AayushShrestha-230293/Obtain/population/Population2011_1656567141570.csv")
colnames(PopulationData)

column_names = c(
  "Housenumber","Price","Date", 
  "Postcode", "Zone 1","Zone 2",
  "Zone 3","PAON","SAON", "Street",
  "Locality","Town", "District", 
  "County","NA1","NA2"
)



HousePrices2021 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/house pricing/pp-2021.csv", col_names = FALSE) %>%
  set_names(column_names)

HousePrices2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/house pricing/pp-2022.csv", col_names = FALSE) %>%
  set_names(column_names)

HousePrices2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/house pricing/pp-2023.csv", col_names = FALSE) %>%
  set_names(column_names)

HousePrices2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/house pricing/pp-2024.csv", col_names = FALSE) %>%
  set_names(column_names)

PopulationData <- read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/population/Population2011_1656567141570.csv")

# Clean, convert and calculate population by short postcode
PopulationData_clean <- PopulationData %>%
  # Remove rows where Population cannot convert to numeric
  filter(!is.na(as.numeric(Population))) %>%
  # Convert Population to numeric
  mutate(Population = as.numeric(Population)) %>%
  # Extract first 4 characters from Postcode, trim spaces
  mutate(shortPostcode = str_trim(substring(Postcode, 1, 4))) %>%
  # Group by shortPostcode and sum population
  group_by(shortPostcode) %>%
  summarise(Population2011 = sum(Population, na.rm = TRUE)) %>%
  # Calculate population for years 2012 to 2024 using multipliers
  mutate(
    Population2012 = 1.00695353132322269 * Population2011,
    Population2013 = 1.00669740535540783 * Population2012,
    Population2014 = 1.00736463978721671 * Population2013,
    Population2015 = 1.00792367505802859 * Population2014,
    Population2016 = 1.00757874492811929 * Population2015,
    Population2017 = 1.00679374473924223 * Population2016,
    Population2018 = 1.00605929132212552 * Population2017,
    Population2019 = 1.00561255390388033 * Population2018,
    Population2020 = 1.00561255390388033 * Population2019,
    Population2021 = 1.005425 * Population2020,
    Population2022 = 1.004920 * Population2021,
    Population2023 = 1.004510 * Population2022,
    Population2024 = 1.004220 * Population2023
  )
PopulationData_clean %>% 
  select(shortPostcode, Population2021, Population2022, Population2023, Population2024)




HousePrices = HousePrices2021 %>% 
  add_row(HousePrices2022) %>% 
  add_row(HousePrices2023) %>% 
  add_row(HousePrices2024)

Towns <- HousePrices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(shortPostcode = str_trim(substr(Postcode, 1, 4))) %>%
  left_join(PopulationData_clean, by = "shortPostcode") %>%
  select(shortPostcode, District, County, Population2021, Population2022, Population2023, Population2024) %>%
  group_by(shortPostcode) %>%
  filter(row_number() == 1) %>%
  arrange(County)

write.csv(Towns, "C:/DataScience-R/AayushShrestha-230293/Cleaned data/Towns.csv")



#----------------------------House Pricing Cleaning-----------------------------------------------------------------------


HousePrices = HousePrices2021%>% 
  add_row(HousePrices2022) %>% 
  add_row(HousePrices2023) %>% 
  add_row(HousePrices2024) 
cleanHousePrices = HousePrices %>%
  filter(County=="SOUTH YORKSHIRE"|County=="WEST YORKSHIRE") %>% 
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  mutate(Year=substring(Date,7,10)) %>% 
  arrange(County) %>% 
  select(Postcode,shortPostcode,Price,Date,County,District)
write.csv(cleanHousePrices, "C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_House_Prices.csv")


#------------------------------Crime Data set Cleaning---------------------------------------------------------------

library(tidyverse)

south_yorkshire_2025 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/crime data set/crime2025/2025-04/2025-04-south-yorkshire-street.csv")
west_yorkshire_2025 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/crime data set/crime2025/2025-04/2025-04-west-yorkshire-street.csv")

south_yorkshire_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/crime data set/crime2024/2024-04/2024-04-south-yorkshire-street.csv")
west_yorkshire_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/crime data set/crime2024/2024-04/2024-04-west-yorkshire-street.csv")

south_yorkshire_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/crime data set/crime2023/2023-04/2023-04-south-yorkshire-street.csv")
west_yorkshire_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/crime data set/crime2023/2023-04/2023-04-west-yorkshire-street.csv")

south_yorkshire_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/crime data set/crime2022/2022-06/2022-06-south-yorkshire-street.csv")
west_yorkshire_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/crime data set/crime2022/2022-06/2022-06-west-yorkshire-street.csv")


CrimeData = south_yorkshire_2025 %>% 
  add_row(west_yorkshire_2025) %>% 
  add_row(south_yorkshire_2024) %>% 
  add_row(west_yorkshire_2024) %>% 
  add_row(south_yorkshire_2023) %>% 
  add_row(west_yorkshire_2023) %>% 
  add_row(south_yorkshire_2022) %>% 
  add_row(west_yorkshire_2022) 
  

Clean_crime= CrimeData %>%
  mutate(
    County = `Falls within`                       
  ) %>%
  select(
    CrimeID = `Crime ID`,
    LSOAname = `LSOA name`,
    Month,
    Reportedby = `Reported by`,
    Fallswithin = `Falls within`,
    County,
    Location,
    CrimeType = `Crime type`,
  ) %>%
  arrange(County, Location)

write.csv(Clean_crime,"C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_Crime_Dataset.csv")


#-------------------------------Broad-band Speed Cleaning--------------------------------------------------------


library(tidyverse)

coverage = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Broadband/201809_fixed_pc_coverage_r01.csv")
performance = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Broadband/201805_fixed_pc_performance_r03.csv")

colnames(coverage)
length(colnames(coverage))

colnames(performance)
length(colnames(performance))

clean_coverage = coverage %>% 
  mutate(shortPostcode = str_trim(substr(postcode, 1,4))) %>% 
  select(
    postcode,
    shortPostcode,
    SFBB_Availability = `SFBB availability (% premises)`,
    UFBB_Availability = `UFBB availability (% premises)`,
    FTTP_Availability = `FTTP availability (% premises)`,
    Below_2Mbps = `% of premises unable to receive 2Mbit/s`,
    Below_10Mbps = `% of premises unable to receive 10Mbit/s`
  )

clean_performance = performance %>% 
  mutate(shortPostcode = str_trim(substr(postcode,1,4))) %>% 
  select(
    postcode,
    shortPostcode,
    Median_Download = `Median download speed (Mbit/s)`,
    Avg_Download = `Average download speed (Mbit/s)`,
    Median_Upload = `Median upload speed (Mbit/s)`,
    Avg_Upload = `Average upload speed (Mbit/s)`,
    Avg_Data_Usage = `Average data usage (GB)`
  )

Broadband = left_join(clean_coverage,clean_performance,by = c("postcode","shortPostcode"))

write.csv(Broadband,"C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_BroadBand_Speed.csv")


#----------------------------------------Schools Cleaning 2021-2022---------------------------------------------------------------

library(tidyverse)

library(tidyverse)
library(readxl) 

final_2021_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2021-2022/england_ks4final.csv")
mats_performance_2021_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2021-2022/england_ks4-mats-performance.csv")
provisional_2021_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2021-2022/england_ks4provisional.csv")
pupdest_2021_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2021-2022/england_ks4-pupdest.csv")

underlying_1_2021_2022 = read_excel("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2021-2022/england_ks4underlying_1.xlsx")
underlying_entriesandgrades_2_2021_2022 = read_excel("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2021-2022/england_ks4underlying_entriesandgrades_2.xlsx")

school_information_2021_2022 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2021-2022/england_school_information.csv")

# Check duplicates in existing dataframes (make sure column 'URN' exists in each)
anyDuplicated(final_2021_2022$URN)
anyDuplicated(pupdest_2021_2022$URN)
anyDuplicated(mats_performance_2021_2022$URN)










keywords = c("South Yorkshire", "West Yorkshire")

school_filtered_2021_2022 = school_information_2021_2022 %>%
  filter(
    toupper(str_trim(TOWN)) %in% toupper(keywords) |
      toupper(str_trim(ADDRESS3)) %in% toupper(keywords)
  )




provisional_joined_2021_2022 = school_filtered_2021_2022 %>%
  inner_join(provisional_2021_2022, by = c("URN", "ESTAB"))


pupdest_joined_2021_2022 = provisional_joined_2021_2022 %>%
  left_join(pupdest_2021_2022, by = c("URN", "ESTAB"))


final_joined_2021_2022 = pupdest_joined_2021_2022 %>%
  left_join(final_2021_2022, by = c("URN", "ESTAB"))


cleaned_2021_2022 = final_joined_2021_2022 %>%
  select(
    URN,
    SCHNAME = SCHNAME.x,
    TOWN,
    ADDRESS3,
    LANAME,
    POSTCODE,
    SCHOOLTYPE,
    GENDER,
    AGELOW,
    AGEHIGH,
    P8MEA = P8MEA.x,
    P8MEA_FSM6CLA1A = P8MEA_FSM6CLA1A.x,
    P8MEA_NFSM6CLA1A = P8MEA_NFSM6CLA1A.x,
    EBACCAPS = EBACCAPS.x,
    PTEBACC_95 = PTEBACC_95.x,
    OVERALL_DESTPER,
    EMPLOYMENTPER,
    EDUCATIONPER
  ) %>%
  distinct()


write.csv(cleaned_2021_2022, "C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2021-2022.csv")

view(cleaned_2021_2022)


#----------------------------------------Schools Cleaning 2022-2023---------------------------------------------------------------

library(tidyverse)

ks2_final_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_ks2final.csv")
ks4_final_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_ks4final.csv")
ks2_mats_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_ks2-mats-performance.csv")
ks4_mats_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_ks4-mats-performance.csv")
provisional_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_ks4provisional.csv")
pupdest_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_ks4-pupdest.csv")
underlying_1_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_ks4underlying_1.xlsx")
underlying_2_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_ks4underlying_entriesandgrades_2.xlsx")
school_info_2022_2023 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2022-2023/england_school_information.csv")


keywords = c("South Yorkshire", "West Yorkshire")

school_filtered_2022_2023 = school_info_2022_2023 %>%
  filter(
    toupper(str_trim(TOWN)) %in% toupper(keywords) |
    toupper(str_trim(ADDRESS3)) %in% toupper(keywords)
  )

provisional_joined_2022_2023 = school_filtered_2022_2023 %>%
  inner_join(provisional_2022_2023, by = c("URN", "ESTAB"))

pupdest_joined_2022_2023 = provisional_joined_2022_2023 %>%
  left_join(pupdest_2022_2023, by = c("URN", "ESTAB"))

final_joined_2022_2023 = pupdest_joined_2022_2023 %>%
  left_join(ks4_final_2022_2023, by = c("URN", "ESTAB"))

cleaned_2022_2023 = final_joined_2022_2023 %>%
  select(
    URN,
    SCHNAME = SCHNAME.x,
    TOWN,
    ADDRESS3,
    LANAME,
    POSTCODE,
    SCHOOLTYPE,
    GENDER,
    AGELOW,
    AGEHIGH,
    P8MEA = P8MEA.x,
    P8MEA_FSM6CLA1A = P8MEA_FSM6CLA1A.x,
    P8MEA_NFSM6CLA1A = P8MEA_NFSM6CLA1A.x,
    EBACCAPS = EBACCAPS.x,
    PTEBACC_95 = PTEBACC_95.x,
    OVERALL_DESTPER,
    EMPLOYMENTPER,
    EDUCATIONPER
  ) %>%
  distinct()

write.csv(cleaned_2022_2023, "C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2022-2023.csv")

view(cleaned_2022_2023)


#----------------------------------------Schools Cleaning 2023-2024---------------------------------------------------------------

library(tidyverse)

library(readxl)

ks2_final_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_ks2final.csv")

ks2_mats_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_ks2-mats-performance.csv")

ks4_final_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_ks4final.csv")

ks4_mats_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_ks4-mats-performance.csv")

provisional_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_ks4provisional.csv")

pupdest_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_ks4-pupdest.csv")

underlying_1_2023_2024 = read_excel("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_ks4underlying_1.xlsx")

underlying_2_2023_2024 = read_excel("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_ks4underlying_entriesandgrades_2.xlsx")

school_info_2023_2024 = read_csv("C:/DataScience-R/AayushShrestha-230293/Obtain/Schooldata/2023-2024/england_school_information.csv")



keywords = c("South Yorkshire", "West Yorkshire")

school_filtered_2023_2024 = school_info_2023_2024 %>%
  filter(
    toupper(str_trim(TOWN)) %in% toupper(keywords) |
      toupper(str_trim(ADDRESS3)) %in% toupper(keywords)
  )

provisional_joined_2023_2024 = school_filtered_2023_2024 %>%
  inner_join(provisional_2023_2024, by = c("URN", "ESTAB"))

pupdest_joined_2023_2024 = provisional_joined_2023_2024 %>%
  left_join(pupdest_2023_2024, by = c("URN", "ESTAB"))

final_joined_2023_2024 = pupdest_joined_2023_2024 %>%
  left_join(ks4_final_2023_2024, by = c("URN", "ESTAB"))

cleaned_2023_2024 = final_joined_2023_2024 %>%
  select(
    URN,
    SCHNAME = SCHNAME.x,
    TOWN,
    ADDRESS3,
    LANAME,
    POSTCODE,
    SCHOOLTYPE,
    GENDER,
    AGELOW,
    AGEHIGH,
    P8MEA = P8MEA.x,
    P8MEA_FSM6CLA1A = P8MEA_FSM6CLA1A.x,
    P8MEA_NFSM6CLA1A = P8MEA_NFSM6CLA1A.x,
    EBACCAPS = EBACCAPS.x,
    PTEBACC_95 = PTEBACC_95.x,
    OVERALL_DESTPER,
    EMPLOYMENTPER,
    EDUCATIONPER
  ) %>%
  distinct()

write.csv(cleaned_2023_2024, "C:/DataScience-R/AayushShrestha-230293/Cleaned data/Cleaned_School_2023-2024.csv")

view(cleaned_2023_2024)







