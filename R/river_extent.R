#river extent
library(plyr)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(terra)
meuSIG <- "inst/vector/rivers.GPKG"
rsl <- sf::st_read(meuSIG, layer = "centerline")
rsl_50km <- st_union(st_buffer(rsl, dist = 50000))
ggplot(rsl_50km) + 
  geom_sf() + 
  geom_sf(data = rsl)

