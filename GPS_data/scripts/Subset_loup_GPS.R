# setwd("E:/Rachel/Data/Data_from_Guillemette")
setwd("E:/Rachel/GPS_data_2005_2018")


library(readr)
library(tidyverse)
library(lubridate)

GPSdata_cleaned_loups_ok <- read_csv("loups/GPSdata_cleaned_loups_ok.csv")
View(GPSdata_cleaned_loups_ok)

# The GPS points are expressed in the UTM format. 

# First, we subset the df with only the data matching the years we have for other species

GPS_loup_2005_2008 <- GPSdata_cleaned_loups_ok %>% 
  select(c(1:7, "Altitude", "Temperature")) %>% 
  filter(X != "NA"& Y != "NA")

# Modify the date format so its easier to split
GPS_loup_2005_2008 <- GPS_loup_2005_2008 %>% 
  mutate(formated_date = ymd_hms(Date),
         year = year(formated_date),
         month = month(formated_date),
         day = day(formated_date),
         time = format(formated_date, "%H:%M:%S")) %>% 
  select(-c(formated_date, Date)) %>% 
  filter(year %in% c("2005","2006", "2007", "2008"))
  
# Quick overview of the dataset
summary(GPS_loup_2005_2008)

# How many individuals and how many data/individuals?
ind <- GPS_loup_2005_2008 %>% 
  mutate(Id = as.factor(Id)) %>% 
  group_by(Id,year) %>% 
  count()

ind %>% 
  ungroup() %>% 
  filter(year %in% c("2005","2006", "2007", "2008")) %>% 
  pivot_wider(names_from = Id,
              values_from = n) 


# Convert the UTM x and y coordinates to WSG84
# Ref:
# https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r

# 1. Extract the coordinates
points <- cbind(GPS_loup_2005_2008$X, GPS_loup_2005_2008$Y)

library(terra)
# 2. Create a spatial object and assign that known CRS
v <- vect(points, crs="+proj=utm +zone=19 +datum=WGS84  +units=m")

# 3. Convert to longitude and latitude 
y <- project(v, "+proj=longlat +datum=WGS84")

lonlat <- geom(y)[, c("x", "y")]

# 4. Store the values in the dataframe
GPS_loup_2005_2008$longitude <- geom(y)[, c("x")]
GPS_loup_2005_2008$latitude <- geom(y)[, c("y")]


# 5. Save the clean version of the dataframe
write.csv(GPS_loup_2005_2008, file = "GPS_loup_2005_2008.csv", row.names = FALSE)
