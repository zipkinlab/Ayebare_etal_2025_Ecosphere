## Authors: Neil Gilbert & Samuel Ayebare
##Script:  Spatial_variation_in_density_eles_2021.R


## Description : To plot figure3 - Spatial variation in density for chimpanzees  and elephants 
##               using the hierarchical and modified hierarchical distance sampling models for the 2021 survey


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

#------------------------------------------------------------------------------------------------------------------------------------------##
#### inputs "d2021_eles_hds.tif" and "d2021_eles_mhds.tif" are obtained after running the script: Population_status_and_change_spatial.R
#------------------------------------------------------------------------------------------------------------------------------------------##


# Read in rasters
ele_hds <- terra::rast("d2021_eles_hds.tif")
ele_hds

ele_mhds <- terra::rast("d2021_eles_mhds.tif")
summary(ele_mhds)


# Read in protected area boundaries
kal <- sf::st_read("Kalinzu_FR.shp")
mar <- sf::st_read("Maramagambo_FR.shp")
plot(mar["Area"])
head(mar)


# combining rasters 
both <- c(ele_hds, ele_mhds)
summary(both)

# change the layer names for plotting
names(both) <- c("Elephants (2021)\nHierarchical distance sampling model ", 
                 "Elephants (2021)\nModified hierarchical distance sampling model")


# plotting


ggplot2::ggplot() + 
  tidyterra::geom_spatraster(data = both) +
  # facet wrap by species
  ggplot2::facet_wrap(~lyr) + 
  # adding reserve boundaries

  geom_sf(data = mar, aes(geometry = geometry), fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = kal, aes(geometry = geometry), fill = NA, color = "blue", linewidth = 1) +
  
  # diverging color ramp, shared by species
  ggplot2::scale_fill_gradientn(
    "Density (2021) per"~km^2,
    colours = c("#ffb339ff", "#3900b3ff"),
    limits  = c(0, 1.1),
    values  = scales::rescale(c(0, 0.2, 0.5, 1.1), from = c(0, 1.1)), 
    breaks = c( 0, 0.2, 0.5, 1.1),
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
  filename = "Figure5_ele_density_2021.jpg", 
  width = 15, 
  height = 10, 
  units = "in", 
  dpi = 400
)
