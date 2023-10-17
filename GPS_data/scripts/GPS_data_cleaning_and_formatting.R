# Load the packages 
library(readr)
library(tidyverse)
library(sp)
library(adehabitatHR)    
library(magrittr)

# Set working directory
setwd("E:/Rachel/Data/Data_expanded_2005_2018")

# Load the datasets
GPS_wolf_2005_2018_cleaned <- read_csv("GPS_wolf_2005_2018_cleaned.csv")
GPS_moose_2005_2018_cleaned <- read_csv("GPS_moose_2005_2018_cleaned.csv")
GPS_caribou_2005_2018_cleaned <- read_csv("GPS_caribou_2005_2018_cleaned.csv")

GPS_wolf_2005_2018_cleaned %>% 
  dplyr::select(id) %>% 
  unique()
# ------------------------------------------------------------------------------
# ---------------------- TIDY THE DATA -----------------------------------------
# ------------------------------------------------------------------------------

# Add a species category
GPS_moose_2005_2018_cleaned$species = "moose"
GPS_wolf_2005_2018_cleaned$species = "wolf"
GPS_caribou_2005_2018_cleaned$species = "caribou"

# Change format of some columns so merging can work properly
GPS_moose_2005_2018_cleaned$month <- as.character(GPS_moose_2005_2018_cleaned$month)
GPS_moose_2005_2018_cleaned$day <- as.character(GPS_moose_2005_2018_cleaned$day)
GPS_wolf_2005_2018_cleaned$old_date <- as.character(GPS_wolf_2005_2018_cleaned$old_date)
GPS_caribou_2005_2018_cleaned$old_date <- as.character(GPS_caribou_2005_2018_cleaned$old_date)


# Rename some columns
GPS_wolf_2005_2018_cleaned <- GPS_wolf_2005_2018_cleaned %>% 
  rename(x = X,
         y = Y)

# ------------------------------------------------------------------------------
# ---------------------- UTM CONVERSION ----------------------------------------
# ------------------------------------------------------------------------------ 

# Caribou GPS data are in format long/lat. Should be transformed in UTM
caribou_utm <- GPS_caribou_2005_2018_cleaned 

coordinates(caribou_utm) <- c("longitude", "latitude")
proj4string(caribou_utm) <- CRS("+proj=longlat +datum=WGS84")

utm_zone <- 19  # Example UTM zone, change it to your required zone
utm_crs <- CRS(paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84"))
caribou_utm <- spTransform(caribou_utm, utm_crs)


caribou_utm <- as.data.frame(caribou_utm)%>% 
  rename(x = longitude,
         y = latitude)



# Merging all the datasets to one
All_species_cleaned <- full_join(GPS_moose_2005_2018_cleaned, 
                                 GPS_wolf_2005_2018_cleaned) %>% 
  full_join(caribou_utm) %>% 
  mutate(id = as.factor(id))


# Save the UTM format dataframe
saveRDS(All_species_cleaned, file = "~/GPS_data/Rachel/Df_saves/df_utm.rds")
