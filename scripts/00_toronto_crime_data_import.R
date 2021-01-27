### Preamble ###
# Purpose: Clean the Neighbourhood Crime Rates data downloaded from Toronot Open Data
# Author: Rachael Lam
# Date: January 19 2021
# Contact: rachael.lam@mail.utoronto.ca
# Pre-req: None

### Workspace Set-Up ###
# install.packages("opendatatoronto")
# install.packages("tidyverse")
# install.packages("devtools")
# install.packages("dplyr")
# install.packages("reshape2") #to reshape data later down
# install.packages("ggplot2")
library(opendatatoronto)
library(tidyverse)
library(devtools)
library(dplyr)
library(reshape2)
library(ggplot2)

### Finding the dataset from Toronto Open Data ###
all_data <- 
  opendatatoronto::search_packages("crime") %>%
  filter(title == "Neighbourhood Crime Rates") %>%
  select(id) %>%
  opendatatoronto::list_package_resources() %>%
  get_resource()
view(all_data)

### Saving the dataset ###
write_csv(raw_data, "inputs/data/raw_data.csv")

### Cleaning the raw data ###
toronto_crime <- raw_data %>%
  as_tibble() %>%
  select(Neighbourhood, Robbery_2014, Robbery_2015, Robbery_2016, Robbery_2017, Robbery_2018, Robbery_2019) %>% # focusing on robbery. There are other crimes to choose from if desired.
  melt(id = c("Neighbourhood"), #using `reshape2` to change dataframe to long-format data for plotting. kept Neighbourhood as the ID
       variable.name = "Year",
       value.name = "Number_of_Robberies")

toronto_crime$Year <- as.character(toronto_crime$Year) # turned column from factor into character to mutate below

toronto_crime <-
  toronto_crime %>% # mutated values to only be year rather than type_year
  mutate(Year = case_when(
    str_detect(Year, "Robbery_2014") ~ "2014",
    str_detect(Year, "Robbery_2015") ~ "2015",
    str_detect(Year, "Robbery_2016") ~ "2016",
    str_detect(Year, "Robbery_2017") ~ "2017",
    str_detect(Year, "Robbery_2018") ~ "2018",
    str_detect(Year, "Robbery_2019") ~ "2019",
    TRUE ~ Year
  ))

head(toronto_crime, 10) # checking first 10 rows to make sure the years were mutated.

### Graphing robbery rates ###
toronto_crime %>%
  filter(Neighbourhood %in% c("Niagra", "South Parkdale", "Little Portugal", 
                              "Roncesvalles", "Dufferin Grove", "Trinity-Bellwoods", 
                              "Palmerston-Little Italy", "University", "Kensington-Chinatown", 
                              "Waterfront Communities-The Island", "Bay Street Corridor", "Church-Yonge Corridor", 
                              "Moss Park", "Cabbagetown-South St.James Town", "Regent Park", "North St.James Town")) %>% # chose 16 neighbourhoods in the Toronto downtown core based on the Toronto Neighbourhood Map.
  ggplot(aes(x = Year, y = Number_of_Robberies, color = Neighbourhood)) +
  geom_smooth(aes(group = Neighbourhood), se = FALSE) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(color = "Neighbourhoods",
       x = "Date",
       y = "Number of Robberies",
       title = "Toronto Crime Rate",
       subtitle = "Number of robberies per year") +
  theme_minimal()

### Finding top neighbourhood robbery rates ###
toronto_crime_highest <- raw_data %>%
  as_tibble() %>%
  select(Neighbourhood, Robbery_2014, Robbery_2015, Robbery_2016, Robbery_2017, Robbery_2018, Robbery_2019, Robbery_AVG) %>% # focusing on robbery. There are other crimes to choose from if desired.
  slice_max(Robbery_AVG, n = 10) %>% # choosing 10 neighbourhoods with the highest robbery average between 2014-2019
  select(Neighbourhood, Robbery_2014, Robbery_2015, Robbery_2016, Robbery_2017, Robbery_2018, Robbery_2019) %>%
  melt(id = c("Neighbourhood"), #using `reshape2` to change dataframe to long-format data for plotting. kept Neighbourhood as the ID
       variable.name = "Year",
       value.name = "Number_of_Robberies")

toronto_crime_highest$Year <- as.character(toronto_crime_highest$Year) # turned column from factor into character to mutate below

toronto_crime_highest <-
  toronto_crime_highest %>% # mutated values to only be year rather than type_year
  mutate(Year = case_when(
    str_detect(Year, "Robbery_2014") ~ "2014",
    str_detect(Year, "Robbery_2015") ~ "2015",
    str_detect(Year, "Robbery_2016") ~ "2016",
    str_detect(Year, "Robbery_2017") ~ "2017",
    str_detect(Year, "Robbery_2018") ~ "2018",
    str_detect(Year, "Robbery_2019") ~ "2019",
    TRUE ~ Year
  ))

toronto_crime_highest%>%
  ggplot(aes(x = Year, y = Number_of_Robberies, color = Neighbourhood)) +
  geom_smooth(aes(group = Neighbourhood), se = FALSE) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(color = "Neighbourhoods",
       x = "Date",
       y = "Number of Robberies",
       title = "Toronto Crime Rate",
       subtitle = "Number of robberies per year") +
  theme_minimal()

### Graphing neighbourhoods with the largest populations ###
toronto_crime_pop_top <- raw_data %>%
  as_tibble() %>%
  select(Neighbourhood, Population) %>% # focusing on robbery. There are other crimes to choose from if desired.
  slice_max(Population, n = 10)

toronto_crime_pop_top %>%
  ggplot(aes(x = Neighbourhood, y = Population, fill = Neighbourhood)) +
  geom_bar(stat = "identity") +
  ylab("proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### All crime ###
toronto_crime_all <- raw_data %>%
  select(Neighbourhood,
         Assault_2014, Assault_2015, Assault_2016, Assault_2017, Assault_2018, Assault_2019,
         AutoTheft_2014, AutoTheft_2015, AutoTheft_2016, AutoTheft_2017, AutoTheft_2018, AutoTheft_2019,
         BreakandEnter_2014, BreakandEnter_2015, BreakandEnter_2016, BreakandEnter_2017, BreakandEnter_2018, BreakandEnter_2019,
         Homicide_2014, Homicide_2015, Homicide_2016, Homicide_2017, Homicide_2018, Homicide_2019,
         TheftOver_2014, TheftOver_2015, TheftOver_2016, TheftOver_2017, TheftOver_2018, TheftOver_2019,
         Robbery_2014, Robbery_2015, Robbery_2016, Robbery_2017, Robbery_2018, Robbery_2019) %>%
  melt(id = c("Neighbourhood"), #using `reshape2` to change dataframe to long-format data for plotting. kept Neighbourhood as the ID
       variable.name = "Year",
       value.name = "Number_of_Crimes")

toronto_crime_all %>%
  group_by(Neighbourhood) %>%
  summarise(Number_of_Crimes = sum(Number_of_Crimes)) %>%
  slice_max(Number_of_Crimes, n = 10)

toronto_crime_pop <- raw_data %>%
  select(Neighbourhood, Population) %>% # focusing on robbery. There are other crimes to choose from if desired.
  filter(Neighbourhood %in% c("Waterfront Communities-The Island", "Bay Street Corridor", "Church-Yonge Corridor", 
                              "West Humber-Clairville", "Moss Park", "York University Heights",
                              "Downsview-Roding-CFB", "Kensington-Chinatown", "Woburn", "West Hill"))



### Assault_Rate ###
toronto_crime_rate <- raw_data %>%
  select(Neighbourhood, Robbery_Rate_2019)

toronto_crime_rate$Robbery_Rate_2019 <- as.integer(toronto_crime_rate$Robbery_Rate_2019)

toronto_crime_rate %>%
  slice_max(Robbery_Rate_2019, n = 10)
toronto_crime_rate
