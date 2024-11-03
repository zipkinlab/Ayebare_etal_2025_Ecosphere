## Authors: Neil Gilbert & Samuel Ayebare
##Script:  Figure7_effect_of_elevation.R


## Description : To plot figure7 (i.e., Map showing the spatial changes in expected population density from 2007 to 2021 
##               for chimpanzees (left) and elephants (right))


#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

library(here)
setwd(here::here("Data"))

#----------------#
#-Load libraries-#
#----------------#

library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(ggthemes)
library(extrafont)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------##
#### inputs "Change_in_density_chimps_21_07.tif" and "Change_in_density_eles_21_07.tif" are obtained after running the script: Population_status_and_change_spatial.R
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------##

# Read in rasters
chimp <- terra::rast("Change_in_density_chimps_21_07.tif")
chimp
ele <- terra::rast("Change_in_density_eles_21_07.tif")
ele

# Read in protected area boundaries
kal <- sf::st_read("Kalinzu_FR.shp")
mar <- sf::st_read("Maramagambo_FR.shp")
plot(mar)
head(mar)

# the two rasters have different extents, 
# since elephants are for Maramagambo only
# extend the elephant raster to match that of the chimp raster
ele2 <- terra::extend(ele, chimp)

# blows my mind that combining rasters is this easy
both <- c(chimp, ele2)
# change the layer names for plotting
names(both) <- c("Chimpanzees\nModified hierarchical distance sampling model ", 
                 "Elephants\nModified hierarchical distance sampling model")

# plotting

ggplot2::ggplot() + 
  tidyterra::geom_spatraster(data = both) +
  # facet wrap by species
  ggplot2::facet_wrap(~lyr) + 
  # adding reserve boundaries
  geom_sf(data = mar, aes(geometry = geometry), fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = kal, aes(geometry = geometry), fill = NA, color = "blue", linewidth = 1) +
  
  # diverging color ramp, shared by species
  ggplot2::scale_fill_gradient2(
    "Change in density (2021 - 2007) per"~km^2,
    low = "#e76254",
    mid =  "#f2f2f2",
    high = "#1e466e",
    # doing custom breaks 
    breaks = c(-0.97, 0, 1.2, 2.3),
    na.value = NA) + 
  # Aesthetics
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "bottom",
                 legend.box.margin = margin(0, 0, 5, 0),
                 legend.text = element_text(family = "Arial", size = 22, color = "black",face = "bold"), 
                 legend.title = element_text(size = 25, color = "black", face = "bold"),
                 strip.text = element_text(family = "Arial", size = 22, margin=margin(0,0,5,0), color = "black", face = "bold"),
                 panel.background = element_rect(fill = "white", color = NA), 
                 plot.background = element_rect(fill = "white", color = NA)) +
  ggplot2::guides(fill = guide_colorbar(ticks = FALSE,
                                        size = 22,
                                        face = "bold",
                                        title.position = "top", 
                                        title.hjust = 0.5,
                                        barwidth = 25))

# save map
ggplot2::ggsave(
  filename = "Figure4_Spatial_changes_in_density.jpg", 
  width = 15, 
  height = 10, 
  units = "in", 
  dpi = 400
)



