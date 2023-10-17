library(furrr)

df <- readRDS(file = "~/GPS_data/Rachel/Df_saves/computed_distance_species.rds")


# Set up parallel processing
plan(multicore)

test <- df %>% 
  head(500)


Rprof(filename = "profile.out")

# Optimize the interaction column creation using furrr
df_interaction <- test %>%
  mutate(distance_results = future_map(distance_results, ~ {
    .x %>%
      mutate(
        interaction = case_when(
          (species1 %in% c("caribou", "moose", "wolf")) &
            (species2 %in% c("caribou", "moose", "wolf")) &
            species1 == species2 ~ "intraspecific",
          (species1 %in% c("caribou", "moose")) &
            (species2 %in% c("caribou", "moose")) &
            species1 != species2 ~ "interspecific",
          (species1 %in% c("caribou", "moose", "wolf")) &
            (species2 %in% c("caribou", "moose", "wolf")) &
            species1 != species2 ~ "trophic",
          TRUE ~ "unknown"
        )
      )
  }))

Rprof(NULL)
summaryRprof("profile.out")

