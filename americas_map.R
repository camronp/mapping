#dependent on "metadata_tidy" object from cleaning_plotting_data.R

library(sf)
library(rnaturalearth)
library(tidyverse)
library(scales)
library(ggforce)
library(gridExtra)


# Load world map data
world <- ne_countries(returnclass = "sf") %>% 
  filter(continent %in% c("North America", "South America") & name != "Greenland")


#not ideal
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "black") +
  geom_arc_bar(data = metadata_tidy, aes(x0 = longitude, y0 = latitude, fill = rlpm_q_pcr,
                                         r0 = 0, r = 5, 
                                         start = 0, end = 2*pi), 
               color = "black", alpha = 0.7) +
  coord_sf(xlim = c(-180, 0), ylim = c(-60, 80), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(linewidth = 2, color = "black")) +
  labs(title = "Distribution of rlpm_q_pcr by Country")

#---------------

ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "black") +
  geom_point(data = metadata_tidy2, aes(x = longitude, y = latitude), fill = "purple", color = "black", size = 3) +
  theme_void() +
  labs(title = "Map of the Americas") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
