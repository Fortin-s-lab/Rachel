setwd("E:/Rachel/Data/Data_from_Guillemette")

library(readr)
library(tidyverse)
library(lubridate)

GPSdata_Dataset_Caribou_AllID_20052018 <- read_csv("caribous/GPSdata_Dataset_Caribou_AllID_20052018.csv")
View(GPSdata_Dataset_Caribou_AllID_20052018)

# First, we subset the df with only the data matching the years we have for other species

GPS_caribou_2005_2008 <- GPSdata_Dataset_Caribou_AllID_20052018 %>% 
  filter(latitude != "NA"& longitude != "NA") %>% 
  select(-c(id2, DNT_UniqueID))

# Modify the date format so its easier to split
GPS_caribou_2005_2008 <- GPS_caribou_2005_2008 %>% 
  mutate(formated_date = ymd_hm(date),
         year = year(formated_date),
         month = month(formated_date),
         day = day(formated_date),
         time = format(formated_date, "%H:%M:%S")) %>% 
  select(-c(formated_date, date), time) %>% 
  filter(year %in% c("2005","2006", "2007", "2008"))

# Quick overview of the dataset
summary(GPS_caribou_2005_2008)

# How many individuals and how many data/individuals?
ind <- GPS_caribou_2005_2008 %>% 
  mutate(Id = as.factor(id)) %>% 
  group_by(Id,year) %>% 
  count()

ind %>% 
  ungroup() %>% 
  filter(year %in% c("2005","2006", "2007", "2008")) %>% 
  pivot_wider(names_from = Id,
              values_from = n) %>% View()


# 5. Save the clean version of the dataframe
write.csv(GPS_caribou_2005_2008, file = "GPS_caribou_2005_2008.csv", row.names = FALSE)
