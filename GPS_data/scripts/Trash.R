# Load the packages 
library(readr)
library(tidyverse)
library(sp)
library(adehabitatHR)
library(magrittr)
library(purrr)
library(tibble)
library(furrr)

# Set the working directory
setwd("C:/Users/lab/Documents/GPS_data/Rachel/Scripts")


# Load the data
df_utm <- readRDS(file = "~/GPS_data/Rachel/Df_saves/df_utm.rds")

# Convert time to a numeric value (you might need to adjust this based on your actual data)
df_utm$time_num <- as.numeric(as.POSIXct(df_utm$time, format = "%H:%M:%S"))

# Check time steps
df_utm %>% 
  dplyr::select(time) %>% 
  unique() %>% 
  View()


# UTM coordinates are expressed in meters, so the computed distances will also
# be in meters

# ==============================================================================
# ------- BETWEEN INDIVIDUALS, DISTANCE AT EACH COMMON TIME STEP TIME ----------
# ==============================================================================

# 1. Nest the dataframe per year, month, day and time
# and discard the data without "time"
nested_df <- df_utm %>% 
  filter(!is.na (time)) %>% 
  group_by(year, month, day, time) %>% 
  dplyr::select(c(year, month, day, time, id, x, y, species)) %>% 
  nest()

class(nested_df$data)

# Create a list of the correspondance between id and species
id_species <- df_utm %>% 
  dplyr::select(id, species) %>% 
  unique()


# FASTER VERSION cause it uses indexes instead of chr strings.
compute_distance <- function(data) {
  n_ids <- nrow(data)
  
  # Exception if the nested df contains only one row
  if (n_ids == 1) {
    return(tibble(id1 = data$id, id2 = data$id, distance = 0))
    
    # Else, we need to create the pair to compare
  } else {
    # id_pairs <- combn(data$id, 2, simplify = FALSE)
    id_pairs <- combn(row_number(data), 2, simplify = FALSE)
    # print(id_pairs)
    
    # row
    
    distance_tibbles <- vector("list", length = length(id_pairs))
    
    for (i in 1:length(id_pairs)) {
      id_pair <- id_pairs[[i]]
      id1 <- id_pair[1]
      id2 <- id_pair[2]
      
      x_id1 <- data[[2]][[id1]]
      # print(row)
      
      # row <- data %>%
      #   filter(id == id1) 
      # dplyr::select(x, y)
      
      x_id2 <- data[[2]][[id2]]
      
      y_id1 <- data[[3]][[id1]]
      y_id2 <- data[[3]][[id2]]
      
      species1 <- data[[4]][[id1]]
      species2 <- data[[4]][[id2]]
      
      
      distance <- sqrt(((x_id2 - x_id1)^2)+(y_id2 - y_id1)^2)
      
      #   col <- data %>%
      #     filter(id == id2) %>%
      #     dplyr::select(x, y)
      #   
      #   # browser()
      #   # Then compute the distance
      #   distance <- sqrt(sum((row - col)^2))
      #   

      
      distance_tibbles[[i]] <- data.frame(
        id1 = id1,
        id1_chr = data[[1]][[id1]],
        species1 = species1,
        id2 = id2,
        id2_chr = data[[1]][[id2]],
        species2 = species2,
        distance = distance,
        interaction = if_else(
          
          
              (species1 %in% c("caribou", "moose", "wolf")) &
              (species2 %in% c("caribou", "moose", "wolf")) &
              species1 == species2, "intraspecific", 
              
              if_else(
              
              (species1 %in% c("caribou", "moose")) &
              (species2 %in% c("caribou", "moose")) &
              species1 != species2, "interspecific", "trophic")
              
              
          
        ))
      #   )
      
      # Add interactions
      # df_interaction <- df %>%
      #   mutate(distance_results = map(distance_results, function(nested_df) {
      #     nested_df %>%
      #       mutate(
      #         interaction = if_else(
      #           (species1 %in% c("caribou", "moose", "wolf")) &
      #             (species2 %in% c("caribou", "moose", "wolf")) &
      #             species1 == species2, "intraspecific",
      #           (species1 %in% c("caribou", "moose")) &
      #             (species2 %in% c("caribou", "moose")) &
      #             species1 != species2, "interspecific",
      #           (species1 %in% c("caribou", "moose", "wolf")) &
      #             (species2 %in% c("caribou", "moose", "wolf")) &
      #             species1 != species2, "trophic",
      #           "unknown")
      #       )
      #   }))
      
      
      
      # distance_res[[i]] <- 
    }
    
    return(bind_rows(distance_tibbles))
  }
}


# qw <- nested_df$data
# qw[[1]]


# 3. Apply the previously defined function to our nested df
# Execute the computations on several R session in parallel so it goes faster
plan(multisession)


test <- nested_df %>%
  head(10) %>%
  droplevels()
#
res <- test %>%
  mutate(distance_results = map(data, compute_distance))

# res_comparaison <- test %>%
#   mutate(distance_results = map(data, compute_distance_dont_use))

res <- nested_df %>%
  mutate(distance_results = map(data, compute_distance))

# Distance in meters

# 4. Save the computations
saveRDS(res, "~/GPS_data/Rachel/Df_saves/computed_distance_2005_2018.rds")
