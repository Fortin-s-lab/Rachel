library(ggplot2)
library(ggmap)



individual <- moose_and_wolf %>% 
  filter(id == "KO-01",
         year == "2005")

individual %>% 
  ggplot(aes(x = longitude, y = latitude, color = month)) +
  coord_quickmap() +
  geom_point()
