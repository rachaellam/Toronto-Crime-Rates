### Preamble ###
# Purpose: Clean the Neighbourhood Crime Rates data downloaded from Toronto Open Data
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
# install.packages("bookdown")
# install.packages("here")
# install.packages("kableExtra")
library(opendatatoronto)
library(tidyverse)
library(devtools)
library(dplyr)
library(reshape2)
library(ggplot2)
library(bookdown)
library(here)
library(kableExtra)

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
raw_data <- read.csv("inputs/data/raw_data.csv")

### Grabbing specific columns ###
toronto_crime_all <- raw_data %>%
  as_tibble() %>%
  select(Neighbourhood,
         Assault_2014, Assault_2015, Assault_2016, Assault_2017, Assault_2018, Assault_2019,
         AutoTheft_2014, AutoTheft_2015, AutoTheft_2016, AutoTheft_2017, AutoTheft_2018, AutoTheft_2019,
         BreakandEnter_2014, BreakandEnter_2015, BreakandEnter_2016, BreakandEnter_2017, BreakandEnter_2018, BreakandEnter_2019,
         Homicide_2014, Homicide_2015, Homicide_2016, Homicide_2017, Homicide_2018, Homicide_2019,
         TheftOver_2014, TheftOver_2015, TheftOver_2016, TheftOver_2017, TheftOver_2018, TheftOver_2019,
         Robbery_2014, Robbery_2015, Robbery_2016, Robbery_2017, Robbery_2018, Robbery_2019)

### Finding the top 10 neighbourhoods (out of 140) with the most crimes ###
toronto_crime_top <- toronto_crime_all %>%
  melt(id = c("Neighbourhood"), #using `reshape2` to change dataframe to long-format data for plotting. kept Neighbourhood as the ID
       variable.name = "Year",
       value.name = "Number_of_Crimes") %>%
  group_by(Neighbourhood) %>%
  summarise(Number_of_Crimes = sum(Number_of_Crimes)) %>% # summing crime by neighbourhood 
  slice_max(Number_of_Crimes, n = 10) # selecting top 10 neighbourhoods with the most crime

### Graphing neighbourhoods with the highest crime numbers and their population ###
toronto_crime_pop <- raw_data %>%
  as_tibble() %>%
  select(Neighbourhood, Population) %>%
  filter(Neighbourhood %in% c("Waterfront Communities-The Island", "Bay Street Corridor", "Church-Yonge Corridor", 
                              "West Humber-Clairville", "Moss Park", "York University Heights",
                              "Downsview-Roding-CFB", "Kensington-Chinatown", "Woburn", "West Hill")) # grabbing the population of the top 10 neighbourhoods with the most crime

compare_pop_crime <- merge(x = toronto_crime_top, y = toronto_crime_pop, by = "Neighbourhood", all = TRUE) # merging the population and crime dataframes

compare_pop_crime %>% # plotting the number of crimes with their population
  ggplot(aes(x = reorder(Neighbourhood, -Population), y = Number_of_Crimes, fill = Population)) + # ordered by population size
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Neighbourhood",
       y = "Number of Crimes Committed",
       title = "Toronto Crime Rate",
       subtitle = "Top 10 neighbourhoods with the most crimes between 2014-2019") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Understanding neighbourhood population ###
toronto_crime_pop_overall <- raw_data %>%
  as_tibble() %>%
  select(Neighbourhood, Population) %>%
  summarize(
    The_min = min(Population),
    The_max = max(Population),
    Mean = mean(Population),
    Std_dev = sd(Population)) %>%
  arrange(desc(Mean))

toronto_crime_pop_overall %>%
  knitr::kable(digits = 2, 
               caption = "My first table.", 
               col.names = c("Population Min", "Population Max", "Population Mean", "Population Standard Deviation"),
               align = c('l', 'l', 'l', 'l')
  )

per_capita <- compare_pop_crime%>%
  transform(per_capita = (Number_of_Crimes / Population) * 100000)

per_capita %>%
  ggplot(aes(x = reorder(Neighbourhood, -per_capita), y = per_capita, fill = Population)) + # ordered by population size
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Neighbourhood",
       y = "Number of Crimes Committed",
       title = "Toronto Crime Rate",
       subtitle = "Top 10 neighbourhoods with the most crimes between 2014-2019") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Compare all sum crimes across neighbourhoods ###
toronto_crime_yearly <- toronto_crime_all %>%
  melt(id = c("Neighbourhood"), #using `reshape2` to change dataframe to long-format data for plotting. kept Neighbourhood as the ID
       variable.name = "Year",
       value.name = "Number_of_Crimes")
  
toronto_crime_yearly$Year <- as.character(toronto_crime_yearly$Year)

toronto_crime_yearly <- toronto_crime_yearly %>% # this code was help from https://community.rstudio.com/t/replace-entire-string-by-one-specific-word/17302/6
  mutate(Year = case_when(
    str_detect(Year, "Assault_2014") ~ "2014",
    str_detect(Year, "Assault_2015") ~ "2015",
    str_detect(Year, "Assault_2016") ~ "2016",
    str_detect(Year, "Assault_2017") ~ "2017",
    str_detect(Year, "Assault_2018") ~ "2018",
    str_detect(Year, "Assault_2019") ~ "2019",
    str_detect(Year, "AutoTheft_2014") ~ "2014",
    str_detect(Year, "AutoTheft_2015") ~ "2015",
    str_detect(Year, "AutoTheft_2016") ~ "2016",
    str_detect(Year, "AutoTheft_2017") ~ "2017",
    str_detect(Year, "AutoTheft_2018") ~ "2018",
    str_detect(Year, "AutoTheft_2019") ~ "2019",
    str_detect(Year, "BreakandEnter_2014") ~ "2014",
    str_detect(Year, "BreakandEnter_2015") ~ "2015",
    str_detect(Year, "BreakandEnter_2016") ~ "2016",
    str_detect(Year, "BreakandEnter_2017") ~ "2017",
    str_detect(Year, "BreakandEnter_2018") ~ "2018",
    str_detect(Year, "BreakandEnter_2019") ~ "2019",
    str_detect(Year, "Homicide_2014") ~ "2014",
    str_detect(Year, "Homicide_2015") ~ "2015",
    str_detect(Year, "Homicide_2016") ~ "2016",
    str_detect(Year, "Homicide_2017") ~ "2017",
    str_detect(Year, "Homicide_2018") ~ "2018",
    str_detect(Year, "Homicide_2019") ~ "2019",
    str_detect(Year, "TheftOver_2014") ~ "2014",
    str_detect(Year, "TheftOver_2015") ~ "2015",
    str_detect(Year, "TheftOver_2016") ~ "2016",
    str_detect(Year, "TheftOver_2017") ~ "2017",
    str_detect(Year, "TheftOver_2018") ~ "2018",
    str_detect(Year, "TheftOver_2019") ~ "2019",
    str_detect(Year, "Robbery_2014") ~ "2014",
    str_detect(Year, "Robbery_2015") ~ "2015",
    str_detect(Year, "Robbery_2016") ~ "2016",
    str_detect(Year, "Robbery_2017") ~ "2017",
    str_detect(Year, "Robbery_2018") ~ "2018",
    str_detect(Year, "Robbery_2019") ~ "2019",
    TRUE ~ Year
  )) %>%
  group_by(Neighbourhood, Year) %>%
  summarise(Number_of_Crimes = sum(Number_of_Crimes)) %>%
  filter(Neighbourhood %in% c("Waterfront Communities-The Island", "Bay Street Corridor", "Church-Yonge Corridor", 
                              "West Humber-Clairville", "Moss Park", "York University Heights",
                              "Downsview-Roding-CFB", "Kensington-Chinatown", "Woburn", "West Hill")) # grabbing the population of the top 10 neighbourhoods with the most crime


toronto_crime_yearly %>%
  group_by(Year) %>%
  ggplot(aes(x = Year, y = Number_of_Crimes, color = Neighbourhood)) +
  geom_smooth(aes(group = Neighbourhood), se = FALSE) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(color = "Neighbourhoods",
       x = "Date",
       y = "Number of Robberies",
       title = "Toronto Crime Rate",
       subtitle = "Number of robberies per year") +
  theme_minimal()

toronto_crime_yearly %>%
  group_by(Neighbourhood) %>%
  summarize(
    Mean = mean(Number_of_Crimes)) %>%
  arrange(desc(Mean))

