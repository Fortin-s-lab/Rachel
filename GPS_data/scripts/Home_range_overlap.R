# Load the packages 
library(readr)
library(tidyverse)
library(sp)
library(adehabitatHR)    
library(magrittr)


# Load the data
df_utm <- readRDS(file = "df_utm.rds")

# ------------------------------------------------------------------------------
# ---------------------- COMPUTE THE HOME RANGE --------------------------------
# ------------------------------------------------------------------------------

# Ref:
# https://www.youtube.com/watch?v=dsPsRPZiOC0
# https://jamesepaterson.github.io/jamespatersonblog/04_trackingworkshop_kernels



# 1. Select ID that contains at least 30 observations
splst <- all_species %>% 
  group_by(id) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  filter(n >= 30) %>% 
  dplyr::select(id) %>% 
  extract2(1) %>% 
  as.character()

all_species <- all_species %>% 
  filter(id %in% splst) %>% 
  droplevels()


# 1. Convert the data frame to a SpatialPointsDataFrame
df_sp <- SpatialPointsDataFrame(coords = all_species[, c("longitude", "latitude")], data = all_species)

# 2. Set the coordinate reference system (CRS) for the SpatialPointsDataFrame
proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")

# Compute Minimum Convex Polygon (MCP) home range for each individual 
# Here the [,1] indicates where the "id" column is.
kd <- mcp(df_sp[,1])

# Convert to dataframe to get the home-range size
res <- as.data.frame(kd)

# Plots
plot(kd)
plot(kd, col=as.factor(as.data.frame(kd)[,1]), add=TRUE)

# Compute the kernel distribution
kdu_href <- kernelUD(df_sp[,1], h="href")
# kdu_LSCV <- kernelUD(df_sp[,1], h="LSCV")

areas <- kernel.area(kdu_href, percent = 90)


# Plots
image(kdu_href)

# Measure the home ranges
kdu_href_poly <- getverticeshr(kdu_href, percent = 95) 
plot(kdu_href_poly)
print(kdu_href_poly) 

# Areas can also be extracted from the getverticeshr function
areas_per_ind <- as.list(kdu_href_poly@polygons)

# Plots
plot(kdu_href_poly, col = as.factor(kdu_href_poly@data$id))


# ------------------------------------------------------------------------------
# --------------- HOME RANGE PER INDIVIDUAL AND PER YEAR -----------------------
# ------------------------------------------------------------------------------
# Assuming 'df' is your data frame with columns 'id', 'latitude', 'longitude', 'year', 'month', 'day', 'time', 'species', 'altitude', 'temp', and 'uniqueID'
library(adehabitatHR)
library(sp)

# 1. Select ID that contains at least 30 observations
splst <- all_species %>% 
  group_by(id) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  filter(n >= 30) %>% 
  dplyr::select(id) %>% 
  extract2(1) %>% 
  as.character()

all_species <- all_species %>% 
  filter(id %in% splst) %>% 
  droplevels()


# 1. Convert the data frame to a SpatialPointsDataFrame
df_sp <- SpatialPointsDataFrame(coords = all_species[, c("longitude", "latitude")], data = all_species)

# 2. Set the coordinate reference system (CRS) for the SpatialPointsDataFrame
proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")

# Convert the 'id' column to factor
df_sp$id <- as.factor(df_sp$id)

# Create an empty list to store KDE home ranges for each individual per year
kde_hr_list_per_year <- list()

# Get unique IDs in the data
unique_ids <- unique(df_sp$id)

# Get unique years in the data
unique_years <- unique(df_sp$year)

id == CA62
year == 2005


# REVOIR EN METTANT CA DANS LA FONCTION DU DESSOUS 
subset_data <- df_sp[df_sp$id == "CA62" & df_sp$year == "2005", ]
subset_data@data$id <- factor(subset_data@data$id)
as.data.frame(ftable(subset_data@data$id))
# A 1972 observations

# Loop over each unique ID
for (id in unique_ids) {
  # Loop over each unique year
  for (year in unique_years) {
    # Subset the data for the current ID and year
    subset_data <- df_sp[df_sp$id == id & df_sp$year == year, ]
    subset_data@data$id <- factor(subset_data@data$id)
    
    # droplevels(subset_data$id)
    
    # Compute the KDE home range for the current individual and year
    kde_hr <- kernelUD(subset_data[,"id"], h = "href")
    
    # Store the result in the list
    kde_hr_list_per_year[[paste(id, year, sep = "_")]] <- kde_hr
  }
}


# ------------------------------------------------------------------------------
# Subset for a particular year and month


# Gather all the df 
all_species <- full_join(simple_caribou, simple_wolf) %>%
  full_join(GPS_moose_2005_2008) %>% 
  mutate(id = as.factor(id))


subset_test <- all_species %>% 
  filter(year == "2005",
         month == "12")


# 1. Select ID that contains at least 30 observations
splst <- all_species %>% 
  group_by(id) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  filter(n >= 30) %>% 
  dplyr::select(id) %>% 
  extract2(1) %>% 
  as.character()

subset_test <- subset_test %>% 
  filter(id %in% splst) %>% 
  droplevels()


# 1. Convert the data frame to a SpatialPointsDataFrame
df_sp <- SpatialPointsDataFrame(coords = subset_test[, c("longitude", "latitude")], data = subset_test)

# 2. Set the coordinate reference system (CRS) for the SpatialPointsDataFrame
proj4string(df_sp) <- CRS("+proj=longlat +datum=WGS84")

# Compute Minimum Convex Polygon (MCP) home range for each individual 
# Here the [,1] indicates where the "id" column is.
kd <- mcp(df_sp[,1])

# Convert to dataframe to get the home-range size
res <- as.data.frame(kd)

# Plots
unique_colors <- rainbow(length(unique(as.factor(as.data.frame(kd)[,1]))))
plot(kd)
plot(kd, col=as.factor(as.data.frame(kd)[,1]), add=TRUE)

# Set the level of transparency (alpha)
alpha_value <- 0.5  # Adjust the transparency level as needed


legend("bottomright", legend = as.factor(as.data.frame(kd)[,1]),
       col = unique_colors, pch = 16, border = "white", bg = "white")
title("Data from december 2005")


# Compute the kernel distribution
kdu_href <- kernelUD(df_sp[,1], h="href")
# kdu_LSCV <- kernelUD(df_sp[,1], h="LSCV")

areas <- kernel.area(kdu_href, percent = 90)


# Plots
image(kdu_href)

# Measure the home ranges
kdu_href_poly <- getverticeshr(kdu_href, percent = 95) 
plot(kdu_href_poly)
print(kdu_href_poly) 

# Areas can also be extracted from the getverticeshr function
areas_per_ind <- as.list(kdu_href_poly@polygons)

# Plots
plot(kdu_href_poly, col = as.factor(kdu_href_poly@data$id))

install.packages("adehabitatHR")
library(adehabitatHR)

# Compute the overlap
mcp_list <- kd

overlap_matrix <- kerneloverlap(df_sp[,1], grid = 500)

# The grid parameter controls the size of the grid cells used for the KDE
# computation. Increasing the grid parameter can help provide a more 
# accurate representation of the kernel density and mitigate potential 
# issues with high-density regions.

View(as.data.frame(overlap_matrix))

# Revoir cette partie mais semble correspondre a la figure


