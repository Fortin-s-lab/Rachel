setwd("E:/Rachel/Data/Data_expanded_2005_2018")

library(readr)
library(tidyverse)
library(lubridate)

# Load caribou data
GPSdata_Dataset_Caribou_AllID_20052018 <- read_csv("caribous/GPSdata_Dataset_Caribou_AllID_20052018.csv")

# Load wolf data
GPSdata_Dataset_Loups_AllID_20052018 <- read_csv("loups/GPSdata_Dataset_Loups_AllID_20052018.csv")

# Load moose data
GPS_moose_cleaned_data <- read_csv("orignaux/GPS_moose_cleaned_data.csv")

# ------------------------------------------------------------------------------
#                           CARIBOU
# ------------------------------------------------------------------------------

# 1. Discard points for which we don't have long or lat values
GPS_caribou_cleaned <- GPSdata_Dataset_Caribou_AllID_20052018 %>% 
  filter(latitude != "NA"& longitude != "NA") %>% 
  dplyr::select(-c(pop, id2, DNT_UniqueID))

# 2. Modify the date format so its easier to split
GPS_caribou_cleaned <- GPS_caribou_cleaned %>% 
  mutate(old_date = date) %>% 
  separate(date, into = c("year", "month", "day_time"), sep = "-") %>%
  separate(day_time, into = c("day", "time"), sep = " ") %>% 
  mutate(time = paste0(substr(time, 1, 2), ":00:00"))
  
# Quick overview of the dataset
summary(GPS_caribou_cleaned)

# How many individuals and how many data/individuals?
ind <- GPS_caribou_cleaned %>% 
  mutate(Id = as.factor(id)) %>% 
  group_by(Id,year) %>% 
  count()

ind %>% 
  ungroup() %>% 
  pivot_wider(names_from = Id,
              values_from = n) %>% View()


# 3. Save the clean version of the dataframe
write.csv(GPS_caribou_cleaned, file = "~/GPS_data/Rachel/Df_saves/GPS_caribou_2005_2018_cleaned.csv",
          row.names = FALSE)


# ------------------------------------------------------------------------------
#                           WOLF
# ------------------------------------------------------------------------------

# 1. Discard points for which we don't have long or lat values
GPS_wolf_cleaned <- GPSdata_Dataset_Loups_AllID_20052018 %>% 
  dplyr::select(c(1:4)) %>% 
  filter(X != "NA"& Y != "NA")

# 2. Modify the date format so its easier to split
GPS_wolf_cleaned <- GPS_wolf_cleaned %>% 
  mutate(old_date = date) %>% 
  separate(date, into = c("year", "month", "day_time"), sep = "-") %>%
  separate(day_time, into = c("day", "time"), sep = " ") %>% 
  mutate(time = paste0(substr(time, 1, 2), ":00:00"))

# Quick overview of the dataset
summary(GPS_wolf_cleaned)

# How many individuals and how many data/individuals?
ind <- GPS_wolf_cleaned %>% 
  mutate(id = as.factor(id)) %>% 
  group_by(id,year) %>% 
  count()

ind %>% 
  ungroup() %>% 
  pivot_wider(names_from = id,
              values_from = n) 

# 3. Save the clean version of the dataframe
write.csv(GPS_wolf_cleaned, file = "~/GPS_data/Rachel/Df_saves/GPS_wolf_2005_2018_cleaned.csv",
          row.names = FALSE)


# Convert UTM to long and lat for GIS


# Convert the UTM x and y coordinates to WSG84
# Ref:
# https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r

# 1. Extract the coordinates
points <- cbind(GPS_wolf_cleaned$X, GPS_wolf_cleaned$Y)

library(terra)
# 2. Create a spatial object and assign that known CRS
v <- vect(points, crs="+proj=utm +zone=19 +datum=WGS84  +units=m")

# 3. Convert to longitude and latitude 
y <- project(v, "+proj=longlat +datum=WGS84")

lonlat <- geom(y)[, c("x", "y")]

GPS_wolf_2005_2018_longlat <- GPS_wolf_cleaned

# 4. Store the values in the dataframe
GPS_wolf_2005_2018_longlat$longitude <- geom(y)[, c("x")]
GPS_wolf_2005_2018_longlat$latitude <- geom(y)[, c("y")]


# 5. Save the clean version of the dataframe
write.csv(GPS_wolf_2005_2018_longlat, file = "GPS_wolf_2005_2018_longlat.csv", row.names = FALSE)



# ------------------------------------------------------------------------------
#                           MOOSE
# ------------------------------------------------------------------------------

# 1. Discard points for which we don't have long or lat values
GPS_moose_cleaned <- GPS_moose_cleaned_data %>% 
  dplyr::select(c(id, date, heure, latitude, longitude, x, y)) %>% 
  filter(latitude != "NA"& longitude != "NA")

# 2. Modify the date format so its easier to split
GPS_moose_cleaned <- GPS_moose_cleaned %>% 
  mutate(old_date = date) %>% 
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(time = paste0(heure, ":00:00"))


# How many individuals and how many data/individuals?
ind <- GPS_moose_cleaned %>% 
  mutate(id = as.factor(id)) %>% 
  group_by(id,year) %>% 
  count()

ind %>% 
  ungroup() %>% 
  pivot_wider(names_from = id,
              values_from = n) 


# 5. Save the clean version of the dataframe
write.csv(GPS_moose_cleaned, file = "~/GPS_data/Rachel/Df_saves/GPS_moose_2005_2018_cleaned.csv",
          row.names = FALSE)

