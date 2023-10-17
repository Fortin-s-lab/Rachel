# Set the working directory
setwd("C:/Users/lab/Documents/GPS_data/Rachel/Scripts")

# Load the data
computed_distance <- readRDS(file = "~/GPS_data/Rachel/Df_saves/computed_distance_2005_2018.rds")
df_utm <- readRDS(file = "~/GPS_data/Rachel/Df_saves/df_utm.rds")

# Load the packages 
library(readr)
library(tidyverse)
library(sp)
library(adehabitatHR)
library(magrittr)
library(purrr)
library(tibble)

# ==============================================================================
# --------------- SET INTERACTION TYPE BASED ON INVOLVED SPECIES----------------
# ==============================================================================

# We want to split the data according to specific conditions, to then calculate
# parameters of advection, reaction or diffusion

# Overview of the id and the species, to extract a regex (regular expression)
df_utm %>% 
  dplyr::select(c(id, species)) %>% 
  unique() %>% 
  View()

# For most of them id Starts with:
# LO = wolf
# CA = caribou
# KO = moose

# Few exceptions:
# 21576 and 25780 = wolf
# --> else: caribou

# 1. Add 2 columns "species1" and "species2" in the "distance_results" column
# precising the species involved for the distance computation.
df <- computed_distance %>%
  mutate(distance_results = map(distance_results, function(nested_df) {
    nested_df %>%
      mutate(
        id1 = as.character(id1),
        id2 = as.character(id2),
        species1 = case_when(
          grepl("^LO", id1) ~ "wolf",
          grepl("^CA", id1) ~ "caribou",
          grepl("^KO", id1) ~ "moose",
          TRUE ~ "unknown"
        ),
        species2 = case_when(
          grepl("^LO", id2) ~ "wolf",
          grepl("^CA", id2) ~ "caribou",
          grepl("^KO", id2) ~ "moose",
          TRUE ~ "unknown"
        )
      )
  }))

# 2. Save the df
saveRDS(df, "~/GPS_data/Rachel/Df_saves/computed_distance_species.rds")
df <- readRDS(file = "~/GPS_data/Rachel/Df_saves/computed_distance_species.rds")

library(furrr)
plan(multisession)


# TRY TO UNNEST THE DF TO RUN THE FUNCTIOn

# First 10000
# 3. Add an interaction column based on the species involved
# Set up parallel processing


# First 10000
subset1 <- df[1:10000, ]warnings()
subset2 <- df[10001:20000, ]
subset3 <- df[20001:30000, ]
subset4 <- df[30001:40000, ]
subset5 <- df[40001:50000, ]
subset6 <- df[50000:55523, ]


plan(multicore)

Rprof(filename = "profile.out")


df_interaction <- df %>%
  mutate(distance_results = map(distance_results, function(nested_df) {
    nested_df %>%
      mutate(
        interaction = if_else(
          (species1 %in% c("caribou", "moose", "wolf")) &
            (species2 %in% c("caribou", "moose", "wolf")) &
            species1 == species2, "intraspecific",
          (species1 %in% c("caribou", "moose")) &
            (species2 %in% c("caribou", "moose")) &
            species1 != species2, "interspecific",
          (species1 %in% c("caribou", "moose", "wolf")) &
            (species2 %in% c("caribou", "moose", "wolf")) &
            species1 != species2, "trophic",
          "unknown")
      )
  }))



# 4. Check if some interactions were forgotten (ie: check for any "unknown" in 
# the "interaction" category)
has_unknown <- any(df_interaction$distance_results %>% map(~ any(.x$interaction == "unknown")))

if (has_unknown) {
  cat("There are 'unknown' values in the interaction column.\n")
} else {
  cat("No 'unknown' values found in the interaction column.\n")
}


# 5. Save the partial df
saveRDS(df_interaction, "~/GPS_data/Rachel/Df_saves/computed_distance_species_interaction_sub1.rds")


# Overview of the results
