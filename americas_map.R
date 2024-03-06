#dependent on "metadata_tidy" object from cleaning_plotting_data.R

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#library(rnaturalearthhires) #devtools::install_github("ropensci/rnaturalearthhires") needed for F.G.
library(ggspatial)
library(tidyverse)
library(scales)
library(ggforce)
library(gridExtra)


# Load world map data
# have to include europe for french guiana
world <- ne_countries(returnclass = "sf", scale = "medium") %>% 
  filter(continent %in% c("North America", "South America", "Europe") & name != "Greenland")

ggplot() +
  geom_sf(data = world, fill = "#eeeee4", color = "black") +
  geom_sf(data = french_guiana, fill = "#eeeee4", color = "black", alpha = 0.5) + # Overlay French Guiana
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
  theme_bw() +
  geom_sf(data = world, fill = "#eeeee4") +
  geom_point(data = metadata_tidy2, aes(x = longitude, y = latitude), fill = "purple", color = "purple", size = 3) +
  labs(title = "Map of the Americas") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  annotation_scale(location = "bl", width_hint = 0.2) +
  coord_sf(xlim = c(-140, -30), ylim = c(-60, 70))

#-----------

ggplot() +
  geom_sf(data = world, aes(fill = pop_est, color = "black")) +
  geom_point(data = metadata_tidy2, aes(x = longitude, y = latitude), fill = "purple", color = "black", size = 3) +
  theme_void() +
  labs(title = "Map of the Americas") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  scale_fill_viridis_c(trans = "sqrt")
