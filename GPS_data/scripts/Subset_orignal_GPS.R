setwd("E:/Rachel/Data/Data_from_Guillemette")

library(readr)
library(tidyverse)
library(lubridate)

GPS_moose_cleaned_data <- read_csv("orignaux/GPS_moose_cleaned_data.csv")
View(GPS_moose_cleaned_data)

# The GPS points are expressed in the UTM format. 

# First, we subset the df with only the data matching the years we have for other species

GPS_moose_2005_2008 <- GPS_moose_cleaned_data %>% 
  dplyr::select(c(id, date, latitude, longitude, altitude, temp, heure)) %>% 
  filter(latitude != "NA"& longitude != "NA")

# Modify the date format so its easier to split
GPS_moose_2005_2008 <- GPS_moose_2005_2008 %>% 
  mutate(formated_date = as.Date(date, format = "%m/%d/%Y"), 
         year = year(formated_date),
         month = month(formated_date),
         day = day(formated_date)) %>% 
  dplyr::select(-c(formated_date, date)) %>% 
  filter(year %in% c("2005","2006", "2007", "2008"))

# How many individuals and how many data/individuals?
ind <- GPS_moose_2005_2008 %>% 
  mutate(id = as.factor(id)) %>% 
  group_by(id,year) %>% 
  count()

ind %>% 
  ungroup() %>% 
  filter(year %in% c("2005","2006", "2007", "2008")) %>% 
  pivot_wider(names_from = id,
              values_from = n) 


# 5. Save the clean version of the dataframe
write.csv(GPS_moose_2005_2008, file = "GPS_moose_2005_2008.csv", row.names = FALSE)
