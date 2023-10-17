# Add wolf and moose data

library(readr)
library("tidyverse")
GPS_loup_2005_2008 <- read_csv("Sub_data_sets/GPS_loup_2005_2008.csv")
GPS_moose_2005_2008 <- read_csv("Sub_data_sets/GPS_moose_2005_2008.csv")


GPS_moose_2005_2008$species = "moose"
GPS_loup_2005_2008$species = "wolf"

simple_wolf <- GPS_loup_2005_2008 %>% 
  select(c(Id, latitude, longitude, Altitude, Temperature,
           year, month, day, species)) %>% 
  rename(c(altitude = Altitude,
           temp = Temperature,
           id = Id))


moose_and_wolf_2005 <- rbind(GPS_moose_2005_2008,
                        simple_wolf) %>% 
  filter(year == 2005)

a <- moose_and_wolf %>% 
  group_by(year, month, day) %>% 
  nest()
