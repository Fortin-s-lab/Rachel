# Load the packages 
library(dplyr)

# Load the data
computed_distance_interaction_2005_2018 <- readRDS(file ="~/GPS_data/Rachel/Df_saves/computed_distance_interaction_2005_2018.rds")
df_utm <- readRDS("~/GPS_data/Rachel/Df_saves/df_utm.rds")


df_utm %>% 
filter(species == "wolf",
       year == 2018)

# Computes the last points recorded for each animal
last_points <- df_utm %>%
  arrange(desc(year), desc(month), desc(day), desc(time)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()


# Per day and hour
last_points <- last_points %>%
  mutate(distance_to_closest_predator = sapply(1:nrow(.), function(i) {
    current_point <- last_points[i, ]
    df_wolves <- df_utm %>%
      filter(year == current_point$year,
             month == current_point$month,
             day == current_point$day,
             time == current_point$time,
             species == "wolf")
    if (nrow(df_wolves) == 0) {
      return("no recorded predator around")
    } else {
      min_dist <- min(sqrt((df_wolves$x - current_point$x)^2 + (df_wolves$y - current_point$y)^2))
      return(min_dist)
    }
  }))

# Per day
last_points <- last_points %>%
  mutate(distance_to_closest_predator = sapply(1:nrow(.), function(i) {
    current_point <- last_points[i, ]
    df_wolves <- df_utm %>%
      filter(day == current_point$day,
             species == "wolf")
    if (nrow(df_wolves) == 0) {
      return(NA)
    } else {
      min_dist <- min(sqrt((df_wolves$x - current_point$x)^2 + (df_wolves$y - current_point$y)^2))
      return(min_dist)
    }
  }))



# For each prey individual, what was the closest they've been to a predator?

# closet_predator_prey <- computed_distance_interaction_2005_2018 %>% 
#   filter(map_lgl(distance_results, ~ nrow(.) > 1)) %>% # Filter to avoid 1 row df (ie without interaction)
#   pull(distances) %>%
#   map_dfr(as.data.frame) %>% 
#   filter(interaction == "trophic")

closet_predator_prey_without_doubles <- computed_distance_interaction_2005_2018 %>% 
  filter(map_lgl(distance_results, ~ nrow(.) > 1)) %>% # Filter to avoid 1 row df (ie without interaction)
  pull(distances) %>%
  map_dfr(as.data.frame) %>% 
  filter(interaction == "trophic") %>% 
  distinct(id1_chr, id2_chr, .keep_all = TRUE)



# Actual data of fate
library(readr)
Table_SuiviID_2022_MFFP <- read_csv("~/GPS_data/Rachel/Df_saves/Table_SuiviID_2022_MFFP.csv")


# Match between our data and the government's?


# Get unique IDs from both data frames
unique_ids_df1 <- unique(df_utm$id)
unique_ids_df2 <- unique(Table_SuiviID_2022_MFFP$IDANIMAL)

# Check for matching IDs
matching_ids <- unique_ids_df1 %in% unique_ids_df2

# Print the result 
if (any(matching_ids)) {
  cat("There are matching IDs between df1 and df2.\n")
  matching_id_values <- unique_ids_df1[matching_ids]
  print(matching_id_values)
} else {
  cat("There are no matching IDs between df1 and df2.\n")
}


matching_df <- data.frame(id = matching_ids,
                          cause_of_death = Table_SuiviID_2022_MFFP$CAUSE_MORT[match(matching_ids, Table_SuiviID_2022_MFFP$IDANIMAL)])

# TO BE CONTINUED